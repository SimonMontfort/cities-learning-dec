"""
prerender_chips.py
------------------
One-time script: pre-render NTL and MODIS thumbnail chips for every city
in the UCDB CSV and save them as small PNG files in a local chips/ directory.

Run once (takes a few minutes).  After that, app.py reads the PNGs directly
— no GDrive access, no rasterio at request time, instant loads.

Usage:
    python prerender_chips.py            # render all missing chips
    python prerender_chips.py --force    # re-render everything
    python prerender_chips.py --index 520 521 522   # specific cities only
"""

import argparse
import base64
import csv
import io
import json
import sys
from pathlib import Path
import rasterio

import numpy as np

import csv, sys
csv.field_size_limit(sys.maxsize)

# ── Config (re-use the same config as app.py) ──────────────────
sys.path.insert(0, str(Path(__file__).parent))
from config import (
    OUT_PATH as DATA_PATH,
    NTL_COG as NTL_PATH,
    MODIS_COG as MODIS_PATH,
    WSF_DIR,
    MODIS_PFT_COLORS,
)

CHIPS_DIR = DATA_PATH.parent / "chips"
KINDS     = ("ntl", "modis", "wsf")
PAD       = 0.5    # same as app.py thumbnail route
PX        = 300    # output size in pixels


# ── Geometry helpers ───────────────────────────────────────────

def compute_bbox(polygon, pad=PAD):
    try:
        coords = []
        gtype  = polygon.get("type", "")
        if gtype == "Polygon":
            coords = polygon["coordinates"][0]
        elif gtype == "MultiPolygon":
            for poly in polygon["coordinates"]:
                coords.extend(poly[0])
        if not coords:
            return None
        xs = [c[0] for c in coords]
        ys = [c[1] for c in coords]
        dx = (max(xs) - min(xs)) * pad or 0.05
        dy = (max(ys) - min(ys)) * pad or 0.05
        return [min(xs)-dx, min(ys)-dy, max(xs)+dx, max(ys)+dy]
    except Exception:
        return None


def bbox_for_city(city):
    try:
        poly = json.loads(city.get("polygon_geojson", "{}"))
        bbox = compute_bbox(poly)
        if bbox:
            return bbox, poly
    except Exception:
        pass
    lat  = float(city.get("lat", 0))
    lon  = float(city.get("lon", 0))
    area = float(city.get("area_km2") or 100)
    pad  = min(2.0, max(0.3, (area ** 0.5) / 5))
    return [lon-pad, lat-pad, lon+pad, lat+pad], None


# ── Colourisation (same logic as app.py _raster_to_pil) ────────

def raster_to_rgba(data, kind):
    import matplotlib.cm  as mcm
    import matplotlib.colors as mcolors
    h, w = data.shape
    if kind == "ntl":
        d      = np.log1p(np.clip(np.where(np.isnan(data), 0, data), 0, None))
        mx     = d.max()
        d_norm = d / mx if mx > 0 else d
        rgba   = (mcm.inferno(d_norm) * 255).astype(np.uint8)
        zero   = (data <= 0) | np.isnan(data)
        rgba[zero,  3] = 0
        rgba[~zero, 3] = 255
    elif kind == "modis":
        cmap_colors = [MODIS_PFT_COLORS[i][0] for i in range(12)]
        cmap        = mcolors.ListedColormap(cmap_colors)
        norm        = mcolors.BoundaryNorm(list(range(13)), cmap.N)
        nan_mask    = np.isnan(data)
        display     = np.clip(np.where(nan_mask, 0, data).astype(int), 0, 11)
        rgba        = (cmap(norm(display)) * 255).astype(np.uint8)
        rgba[nan_mask,  3] = 0
        rgba[~nan_mask, 3] = 255
    elif kind == "wsf":
        rgba = np.zeros((h, w, 4), dtype=np.uint8)
        rgba[data == 255] = [230, 60, 10, 255]
    else:
        rgba = np.zeros((h, w, 4), dtype=np.uint8)
    return rgba


# ── Projection helpers ─────────────────────────────────────────

def reproject_raster_to_3857(data, src_transform, src_crs, west, south, east, north):
    import rasterio.crs
    from rasterio.warp import reproject, Resampling, calculate_default_transform
    dst_crs = rasterio.crs.CRS.from_epsg(3857)
    dst_transform, dst_width, dst_height = calculate_default_transform(
        src_crs, dst_crs,
        data.shape[1], data.shape[0],
        left=west, bottom=south, right=east, top=north,
    )
    dst = np.full((dst_height, dst_width), np.nan, dtype=np.float32)
    reproject(
        source=data.astype(np.float32),
        destination=dst,
        src_transform=src_transform,
        src_crs=src_crs,
        dst_transform=dst_transform,
        dst_crs=dst_crs,
        resampling=Resampling.nearest,
        src_nodata=np.nan,
        dst_nodata=np.nan,
    )
    return dst, dst_transform


def polygon_to_3857(geojson):
    from pyproj import Transformer
    tr = Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
    def reproject_ring(coords):
        return [list(tr.transform(c[0], c[1])) for c in coords]
    gtype = geojson.get("type", "")
    if gtype == "Polygon":
        new_coords = [reproject_ring(ring) for ring in geojson["coordinates"]]
        all_pts = [pt for ring in new_coords for pt in ring]
    elif gtype == "MultiPolygon":
        new_coords = [[reproject_ring(ring) for ring in poly]
                      for poly in geojson["coordinates"]]
        all_pts = [pt for poly in new_coords for ring in poly for pt in ring]
    else:
        return geojson, None
    xs = [p[0] for p in all_pts]
    ys = [p[1] for p in all_pts]
    return {"type": gtype, "coordinates": new_coords}, (min(xs), min(ys), max(xs), max(ys))


# ── Polygon overlay ────────────────────────────────────────────

def draw_polygon_on_ax(ax, geojson, west, south, east, north, img_h, img_w):
    from matplotlib.patches import Polygon as MplPolygon
    def geo_to_px(lon, lat):
        x = (lon - west)  / (east  - west)  * img_w
        y = (north - lat) / (north - south) * img_h
        return x, y
    def add_ring(coords):
        pts   = [geo_to_px(c[0], c[1]) for c in coords]
        patch = MplPolygon(pts, closed=True,
                           fill=False, edgecolor="#00ffcc", linewidth=1.2, alpha=0.9)
        ax.add_patch(patch)
    gtype = geojson.get("type", "")
    try:
        if gtype == "Polygon":
            add_ring(geojson["coordinates"][0])
        elif gtype == "MultiPolygon":
            for poly in geojson["coordinates"]:
                add_ring(poly[0])
    except Exception:
        pass


# ── Render one chip to PNG bytes ───────────────────────────────

def render_chip(data, kind, west, south, east, north, polygon_geojson,
                missing_msg=None, src_transform=None, src_crs=None):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.patches as mpatches
    import matplotlib.colors as mcolors
    from PIL import Image as PILImage

    # ── Reproject raster + polygon to Web Mercator for correct display ──
    render_w, render_s, render_e, render_n = west, south, east, north
    render_polygon = polygon_geojson
    if data is not None and src_transform is not None and src_crs is not None:
        try:
            data, _ = reproject_raster_to_3857(data, src_transform, src_crs, west, south, east, north)
        except Exception:
            pass
    if polygon_geojson:
        try:
            render_polygon, _ = polygon_to_3857(polygon_geojson)
            # Reproject the padded bbox (not the tight polygon bounds) so the
            # padding that was computed in WGS84 is preserved after reprojection.
            from pyproj import Transformer
            tr = Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
            corners = [
                tr.transform(west,  south),
                tr.transform(east,  south),
                tr.transform(east,  north),
                tr.transform(west,  north),
            ]
            render_w = min(c[0] for c in corners)
            render_e = max(c[0] for c in corners)
            render_s = min(c[1] for c in corners)
            render_n = max(c[1] for c in corners)
        except Exception:
            pass

    # Composite raster onto dark background
    try:
        bg = PILImage.new("RGBA", (PX, PX), (20, 20, 30, 255))
        if data is not None:
            overlay = PILImage.fromarray(raster_to_rgba(data, kind))
            overlay = overlay.resize((PX, PX), PILImage.NEAREST)
            composite = PILImage.alpha_composite(bg, overlay).convert("RGB")
        else:
            composite = bg.convert("RGB")
        composite_arr = np.array(composite)
    except Exception:
        composite_arr = np.full((PX, PX, 3), 20, dtype=np.uint8)

    fig, ax = plt.subplots(figsize=(3, 3), dpi=100)
    fig.patch.set_facecolor("#0f1117")
    ax.set_facecolor("#0f1117")
    ax.imshow(composite_arr, interpolation="bilinear")

    if missing_msg:
        ax.text(0.5, 0.5, missing_msg, ha="center", va="center",
                color="#ef4444", fontsize=8, transform=ax.transAxes,
                bbox=dict(facecolor="#0f1117", alpha=0.7, edgecolor="none"))

    if kind == "modis" and data is not None:
        try:
            present = (set(int(v) for v in data[~np.isnan(data)])
                       if not np.all(np.isnan(data)) else set())
            if present:
                patches = [mpatches.Patch(color=MODIS_PFT_COLORS[c][0],
                                          label=f"{c}: {MODIS_PFT_COLORS[c][1]}")
                           for c in sorted(present) if c in MODIS_PFT_COLORS]
                ax.legend(handles=patches, loc="lower left", fontsize=5,
                          framealpha=0.85, facecolor="#0f1117", edgecolor="#2e3352",
                          labelcolor="white", handlelength=1, borderpad=0.4,
                          labelspacing=0.2)
        except Exception:
            pass

    if render_polygon:
        h, w = composite_arr.shape[:2]
        draw_polygon_on_ax(ax, render_polygon, render_w, render_s, render_e, render_n, h, w)

    ax.set_xlim(0, composite_arr.shape[1])
    ax.set_ylim(composite_arr.shape[0], 0)
    ax.axis("off")
    plt.tight_layout(pad=0)

    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", pad_inches=0,
                facecolor=fig.get_facecolor())
    plt.close(fig)
    buf.seek(0)
    return buf.read()


# ── WSF chip (unchanged logic from app.py) ─────────────────────

def render_wsf_chip(west, south, east, north, polygon_geojson):
    import re
    import rasterio
    from rasterio.merge import merge as rasterio_merge

    tile_paths = []
    for tif in WSF_DIR.glob("WSF2019_v1_*.tif"):
        m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif.name)
        if not m:
            continue
        tlon, tlat = int(m.group(1)), int(m.group(2))
        if tlon < east and tlon+2 > west and tlat < north and tlat+2 > south:
            tile_paths.append(tif)

    data        = None
    missing_msg = None
    if not tile_paths:
        missing_msg = "WSF tiles not downloaded"
    else:
        datasets = [rasterio.open(t) for t in tile_paths]
        try:
            mosaic, _ = rasterio_merge(datasets, bounds=(west, south, east, north))
            data = mosaic[0].astype(float)
        finally:
            for ds in datasets:
                ds.close()

    return render_chip(data, "wsf", west, south, east, north,
                       polygon_geojson, missing_msg)


# ── Main ───────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--force",  action="store_true",
                        help="Re-render chips that already exist")
    parser.add_argument("--kinds",  nargs="+", default=list(KINDS),
                        choices=KINDS, help="Which layers to render")
    parser.add_argument("--index",  nargs="+", type=int, default=None,
                        help="Only render specific city indices")
    args = parser.parse_args()

    CHIPS_DIR.mkdir(parents=True, exist_ok=True)

    # Load CSV
    cities = []
    with open(DATA_PATH, newline="", encoding="utf-8") as f:
        for i, row in enumerate(csv.DictReader(f)):
            row["_idx"] = i
            cities.append(row)

    if args.index:
        cities = [c for c in cities if c["_idx"] in args.index]

    print(f"Cities to process: {len(cities)}")
    print(f"Layers: {args.kinds}")
    print(f"Output: {CHIPS_DIR}\n")

    # ── NTL and MODIS: read COG once, extract all windows ──────
    for kind in [k for k in args.kinds if k in ("ntl", "modis")]:
        raster_path = NTL_PATH if kind == "ntl" else MODIS_PATH
        if not raster_path.exists():
            print(f"[{kind}] SKIP — raster not found: {raster_path}")
            continue

        print(f"[{kind}] Opening {raster_path.name} ...")
        import rasterio
        from rasterio.windows import from_bounds

        # Sort cities by row_off so we read the COG top-to-bottom,
        # maximising sequential I/O and minimising GDrive round-trips.
        city_windows = []
        with rasterio.open(raster_path) as src:
            for city in cities:
                out_path = CHIPS_DIR / f"{city['_idx']}_{kind}.png"
                if out_path.exists() and not args.force:
                    continue
                bbox, poly = bbox_for_city(city)
                w, s, e, n = bbox
                win = from_bounds(w, s, e, n, src.transform)
                win = rasterio.windows.Window(
                    col_off=int(win.col_off),
                    row_off=int(win.row_off),
                    width=max(1, int(round(win.width))),
                    height=max(1, int(round(win.height))),
                )
                city_windows.append((city, bbox, poly, win))

            city_windows.sort(key=lambda x: x[3].row_off)
            total = len(city_windows)
            print(f"[{kind}] Rendering {total} chips ...")

            for i, (city, bbox, poly, win) in enumerate(city_windows):
                idx      = city["_idx"]
                out_path = CHIPS_DIR / f"{idx}_{kind}.png"
                w, s, e, n = bbox
                try:
                    data        = src.read(1, window=win).astype(float)
                    nodata      = src.nodata
                    src_transform = src.window_transform(win)
                    src_crs     = src.crs
                    if nodata is not None:
                        data = np.where(data == nodata, np.nan, data)
                    png = render_chip(data, kind, w, s, e, n, poly,
                                      src_transform=src_transform, src_crs=src_crs)
                    out_path.write_bytes(png)
                    if (i+1) % 50 == 0 or (i+1) == total:
                        print(f"  {i+1}/{total}", flush=True)
                except Exception as exc:
                    print(f"  [{kind}] city {idx} ERROR: {exc}")

        print(f"[{kind}] Done.\n")

    # ── WSF: per-city (tiles are small and local) ───────────────
    if "wsf" in args.kinds:
        wsf_cities = []
        for city in cities:
            out_path = CHIPS_DIR / f"{city['_idx']}_wsf.png"
            if not out_path.exists() or args.force:
                wsf_cities.append(city)

        print(f"[wsf] Rendering {len(wsf_cities)} chips ...")
        for i, city in enumerate(wsf_cities):
            idx      = city["_idx"]
            out_path = CHIPS_DIR / f"{idx}_wsf.png"
            bbox, poly = bbox_for_city(city)
            w, s, e, n = bbox
            try:
                png = render_wsf_chip(w, s, e, n, poly)
                out_path.write_bytes(png)
            except Exception as exc:
                print(f"  [wsf] city {idx} ERROR: {exc}")
            if (i+1) % 50 == 0 or (i+1) == len(wsf_cities):
                print(f"  {i+1}/{len(wsf_cities)}", flush=True)
        print("[wsf] Done.\n")

    # Summary
    total_chips = len(list(CHIPS_DIR.glob("*.png")))
    total_mb    = sum(f.stat().st_size for f in CHIPS_DIR.glob("*.png")) / 1e6
    print(f"✓ {total_chips} chips in {CHIPS_DIR}  ({total_mb:.1f} MB total)")
    print("Next: restart app.py — it will serve chips directly from disk.")


if __name__ == "__main__":
    main()