
"""
wfs_intersect.py
-------------
Computes WSF2019 built pixel count and share for all UCDB cities.
Groups polygons by tile to minimise I/O — each tile opened once.

Output: data/wsf-2019/wsf_stats.csv  (columns: id, wsf_pixel_count, wsf_built_share)
"""

import csv
import re
from collections import defaultdict
from pathlib import Path

import numpy as np
import rasterio
from rasterio.windows import from_bounds
from rasterio.features import geometry_mask
from shapely.geometry import shape
from tqdm import tqdm

# ── Configure ─────────────────────────────────────────────────
BASE_DIR   = Path("/Users/simon/Documents/repo/cities-learning-dec")
UCDB_PATH  = BASE_DIR / "data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg"
WSF_DIR    = Path("/Users/simon/Library/CloudStorage/GoogleDrive-<<< email >>>/My Drive/repo/cities-learning-dec-data/wfs-2019")
OUT_PATH   = BASE_DIR / "data/wsf-2019/wsf_stats.csv"
WSF_NODATA = 0    # WSF: 255 = settlement, 0 = no settlement / nodata
# ──────────────────────────────────────────────────────────────


def tile_origin(tif_path):
    """Extract (lon, lat) SW corner from WSF tile filename."""
    m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif_path.name)
    if m:
        return int(m.group(1)), int(m.group(2))
    return None


def tiles_for_bbox(west, south, east, north, tile_index):
    """Return all tile paths whose 2°×2° extent overlaps the bbox."""
    covering = []
    for (tlon, tlat), path in tile_index.items():
        # tile covers [tlon, tlon+2] × [tlat, tlat+2]
        if tlon < east and tlon + 2 > west and tlat < north and tlat + 2 > south:
            covering.append(path)
    return covering


def wsf_stats_for_polygon(geojson, tile_paths):
    """
    Count WSF settlement pixels (value=255) within polygon.
    Handles polygons spanning multiple tiles.
    Returns (built_pixels, total_pixels).
    """
    geom = shape(geojson)
    west, south, east, north = geom.bounds

    built = 0
    total = 0

    for tile_path in tile_paths:
        with rasterio.open(tile_path) as src:
            try:
                window = from_bounds(west, south, east, north, src.transform)
                # Skip if window has no area in this tile
                if window.width <= 0 or window.height <= 0:
                    continue
                data = src.read(1, window=window)
                win_transform = src.window_transform(window)
            except Exception:
                continue

        if data.size == 0:
            continue

        # Mask to polygon shape
        try:
            mask = geometry_mask(
                [geom], transform=win_transform,
                invert=True,   # True inside polygon
                out_shape=data.shape
            )
        except Exception:
            continue

        inside = data[mask]
        built += int(np.sum(inside == 255))
        total += int(inside.size)

    return built, total


def main():
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)

    # ── Index tiles ──────────────────────────────────────────
    tile_files = list(WSF_DIR.rglob("WSF2019_v1_*.tif"))   # rglob handles subdirs
    tile_index = {}
    for t in tile_files:
        origin = tile_origin(t)
        if origin:
            tile_index[origin] = t
    print(f"Indexed {len(tile_index)} WSF tiles from {WSF_DIR}")

    # ── Load ALL UCDB polygons directly from gpkg ─────────────
    import geopandas as gpd
    print(f"Loading UCDB: {UCDB_PATH}")
    gdf = gpd.read_file(UCDB_PATH).to_crs("EPSG:4326")
    id_col = "ID_UC_G0" if "ID_UC_G0" in gdf.columns else gdf.columns[0]
    print(f"  {len(gdf)} urban centres loaded")

    # ── Group cities by tile(s) they touch ──────────────────
    city_tiles = {}
    skipped = 0
    for i, row in gdf.iterrows():
        geom = row.geometry
        if geom is None or geom.is_empty:
            skipped += 1
            continue
        west, south, east, north = geom.bounds
        covering = tiles_for_bbox(west, south, east, north, tile_index)
        if covering:
            city_tiles[i] = (row[id_col], geom.__geo_interface__, covering)
        else:
            skipped += 1

    print(f"Cities with tile coverage: {len(city_tiles)}, skipped (no tile downloaded): {skipped}")

    # ── Process tile by tile ─────────────────────────────────
    tile_to_cities = defaultdict(list)
    for city_idx, (ucdb_id, geojson, tile_paths) in city_tiles.items():
        for tp in tile_paths:
            tile_to_cities[tp].append(city_idx)

    results = {}  # city_index → (built, total)

    for tile_path, city_indices in tqdm(tile_to_cities.items(), desc="Processing tiles"):
        for city_idx in city_indices:
            if city_idx in results:
                continue
            ucdb_id, geojson, tile_paths = city_tiles[city_idx]
            built, total = wsf_stats_for_polygon(geojson, tile_paths)
            results[city_idx] = (built, total)

    # ── Write output ─────────────────────────────────────────
    with open(OUT_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["id", "wsf_built_pixels", "wsf_total_pixels", "wsf_built_share"])
        for i, row in gdf.iterrows():
            ucdb_id = row[id_col]
            if i in results:
                built, total = results[i]
                share = round(built / total, 4) if total > 0 else 0.0
                writer.writerow([ucdb_id, built, total, share])
            else:
                writer.writerow([ucdb_id, "", "", ""])

    print(f"\n✓ Written: {OUT_PATH}  ({len(results)}/{len(gdf)} cities)")

    shares = np.array([results[i][0]/results[i][1] for i in results if results[i][1] > 0])
    if shares.size:
        print(f"wsf_built_share — mean:{shares.mean():.3f}  median:{np.median(shares):.3f}  "
              f"zeros:{(shares==0).sum()}  ones:{(shares==1).sum()}")
    print("\nNext step: run 06_score_cities.py")


if __name__ == "__main__":
    main()