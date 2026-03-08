"""
UCDB Urban Centre Review Tool
Flask app for reviewing suspected false positive urban centres in the GHSL UCDB.

Usage:
    python app.py
    open http://localhost:5050
"""

import base64
import csv
import io
import json
import os
import shutil
import sys
import tempfile
import threading
import urllib.parse
import urllib.request
from datetime import datetime
from pathlib import Path

import numpy as np
from flask import Flask, jsonify, render_template, request

import sys
csv.field_size_limit(sys.maxsize)

# ── Import config ──────────────────────────────────────────────
sys.path.insert(0, str(Path(__file__).parent))
from config import (
    OUT_PATH as DATA_PATH, BACKUP_PATH,
    NTL_COG as NTL_PATH, MODIS_COG as MODIS_PATH, WSF_DIR,
    SCORE_WEIGHTS, CATEGORIES, MODIS_PFT_COLORS, FLASK_PORT, FLASK_DEBUG,
)
INDICATORS_PATH = DATA_PATH.parent / "country_indicators.csv"

# ── App ────────────────────────────────────────────────────────
app = Flask(__name__, template_folder=Path(__file__).parent / "templates")

# ── In-memory caches ───────────────────────────────────────────
_thumb_cache    = {}
_country_cache  = {}   # country → indicator data for scatter plots
_csv_lock       = threading.Lock()   # serialise all CSV reads + writes


# ── CSV helpers ────────────────────────────────────────────────

def load_cities():
    cities = []
    with open(DATA_PATH, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            row["index"] = i
            cities.append(row)
    return cities


def _load_cities_with_fieldnames():
    """Return (cities, fieldnames) where fieldnames comes directly from the CSV header."""
    cities = []
    with open(DATA_PATH, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        fieldnames = list(reader.fieldnames or [])
        for i, row in enumerate(reader):
            row["index"] = i
            cities.append(row)
    # Ensure review columns are always present in header
    for col in ("decision", "category", "note", "timestamp"):
        if col not in fieldnames:
            fieldnames.append(col)
    return cities, fieldnames


def save_decision(city_index, decision, category, note):
    with _csv_lock:
        cities, fieldnames = _load_cities_with_fieldnames()
        if city_index < 0 or city_index >= len(cities):
            return False
        cities[city_index]["decision"]  = decision
        cities[city_index]["category"]  = category
        cities[city_index]["note"]      = note
        cities[city_index]["timestamp"] = datetime.utcnow().isoformat()

        # Atomic write: write to a temp file in the same directory,
        # then replace — so a crash mid-write never corrupts the CSV.
        tmp_fd, tmp_path = tempfile.mkstemp(
            dir=DATA_PATH.parent, prefix=".cities_tmp_", suffix=".csv"
        )
        try:
            with os.fdopen(tmp_fd, "w", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                for row in cities:
                    writer.writerow({k: v for k, v in row.items() if k != "index"})
            # Safety: never overwrite with a shorter file (guards against truncated reads)
            original_rows = sum(1 for _ in open(DATA_PATH, encoding="utf-8")) - 1  # minus header
            written_rows  = len(cities)
            if written_rows < original_rows:
                os.unlink(tmp_path)
                raise RuntimeError(
                    f"Refusing to overwrite: read {written_rows} rows but CSV has {original_rows}"
                )
            os.replace(tmp_path, DATA_PATH)   # atomic on POSIX; best-effort on Windows
        except Exception:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise
    return True


def get_progress(cities):
    reviewed = sum(1 for c in cities if c.get("decision", "").strip())
    return reviewed, len(cities)


def find_next_pending(cities, current_index=None):
    start = (current_index + 1) if current_index is not None else 0
    for i in range(start, len(cities)):
        if not cities[i].get("decision", "").strip():
            return i
    for i in range(0, start):
        if not cities[i].get("decision", "").strip():
            return i
    return None


def compute_bbox(polygon, pad=0.2):
    """Return padded [west, south, east, north] from GeoJSON geometry."""
    try:
        coords = []
        gtype = polygon.get("type", "")
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


# ── Wikipedia ──────────────────────────────────────────────────

def fetch_wikipedia_search(query):
    encoded = urllib.parse.quote(query)
    url = (f"https://en.wikipedia.org/w/api.php"
           f"?action=opensearch&search={encoded}&limit=5&format=json")
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "UCDB-Review-Tool/1.0"})
        with urllib.request.urlopen(req, timeout=8) as resp:
            data = json.loads(resp.read())
        titles, snippets, urls = data[1], data[2], data[3]
        return {"results": [{"title": t, "url": u, "snippet": s}
                             for t, u, s in zip(titles, urls, snippets)]}
    except Exception as e:
        return {"results": [], "error": str(e)}


def fetch_wikipedia_page(title):
    try:
        url = (f"https://en.wikipedia.org/w/api.php"
               f"?action=parse&page={urllib.parse.quote(title)}"
               f"&prop=text&format=json&disabletoc=1")
        req = urllib.request.Request(url, headers={"User-Agent": "UCDB-Review-Tool/1.0"})
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read())
        html = data.get("parse", {}).get("text", {}).get("*", "")
        if not html:
            return {"found": False, "html": "", "url": ""}
        html = html.replace('href="/wiki/', 'href="https://en.wikipedia.org/wiki/')
        html = html.replace('src="//', 'src="https://')
        wiki_url = f"https://en.wikipedia.org/wiki/{urllib.parse.quote(title.replace(' ', '_'))}"
        return {"found": True, "html": html, "url": wiki_url, "title": title}
    except Exception as e:
        return {"found": False, "html": "", "url": "", "error": str(e)}


# ── Raster thumbnails ──────────────────────────────────────────

def _draw_polygon(ax, geojson, west, south, east, north, img_h, img_w):
    """Draw polygon in pixel coordinates matching the imshow image."""
    from matplotlib.patches import Polygon as MplPolygon

    def geo_to_px(lon, lat):
        """Linear lon/lat → pixel. Matches how imshow maps extent to pixels."""
        x = (lon - west)  / (east  - west)  * img_w
        y = (north - lat) / (north - south) * img_h  # y flipped: north=0
        return x, y

    def add_ring(coords):
        pts = [geo_to_px(c[0], c[1]) for c in coords]
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


def _wsf_tiles_for_bbox(west, south, east, north):
    """Return WSF tile paths covering the bbox."""
    import re
    covering = []
    for tif in WSF_DIR.glob("WSF2019_v1_*.tif"):
        m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif.name)
        if not m:
            continue
        tlon, tlat = int(m.group(1)), int(m.group(2))
        if tlon < east and tlon + 2 > west and tlat < north and tlat + 2 > south:
            covering.append(tif)
    return covering


def _raster_to_pil(data, kind):
    """
    Convert raster numpy array to a PIL RGBA image.
    Transparent pixels = no data / background → OSM shows through.
    Opaque pixels = data present → fully visible at slider=100%.
    """
    from PIL import Image as PILImage
    import matplotlib.colors as mcolors
    import matplotlib.cm as mcm

    h, w = data.shape

    if kind == "ntl":
        d = np.log1p(np.clip(np.where(np.isnan(data), 0, data), 0, None))
        mx = d.max()
        d_norm = d / mx if mx > 0 else d
        rgba = (mcm.inferno(d_norm) * 255).astype(np.uint8)
        # Zero/NaN → fully transparent so OSM background shows through
        zero = (data <= 0) | np.isnan(data)
        rgba[zero, 3] = 0
        # Non-zero → fully opaque (slider controls overall img opacity)
        rgba[~zero, 3] = 255

    elif kind == "modis":
        cmap_colors = [MODIS_PFT_COLORS[i][0] for i in range(12)]
        cmap = mcolors.ListedColormap(cmap_colors)
        norm = mcolors.BoundaryNorm(list(range(13)), cmap.N)
        nan_mask = np.isnan(data)
        display = np.clip(np.where(nan_mask, 0, data).astype(int), 0, 11)
        rgba = (cmap(norm(display)) * 255).astype(np.uint8)
        rgba[nan_mask,  3] = 0    # NaN → transparent
        rgba[~nan_mask, 3] = 255  # data → fully opaque

    elif kind == "wsf":
        rgba = np.zeros((h, w, 4), dtype=np.uint8)
        rgba[data == 255] = [230, 60, 10, 255]   # settlement → opaque orange-red
        # everything else stays transparent (alpha=0)

    else:
        rgba = np.zeros((h, w, 4), dtype=np.uint8)

    return PILImage.fromarray(rgba, "RGBA")


def _make_thumbnail(west, south, east, north, kind, polygon_geojson=None):
    import rasterio
    from rasterio.windows import from_bounds
    from rasterio.merge import merge as rasterio_merge
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.colors as mcolors
    import matplotlib.patches as mpatches

    PX = 300
    data = None
    missing_msg = None
    # ── Read raster data ─────────────────────────────────────
    if kind == "wsf":
        tile_paths = _wsf_tiles_for_bbox(west, south, east, north)
        if not tile_paths:
            missing_msg = "WSF tiles not downloaded"
        else:
            datasets = [rasterio.open(t) for t in tile_paths]
            try:
                if len(datasets) > 1:
                    mosaic, _ = rasterio_merge(datasets)
                    data = mosaic[0].astype(float)
                else:
                    window = from_bounds(west, south, east, north, datasets[0].transform)
                    data = datasets[0].read(1, window=window).astype(float)
            finally:
                for ds in datasets:
                    ds.close()
    else:
        raster_path = NTL_PATH if kind == "ntl" else MODIS_PATH
        if not raster_path.exists():
            missing_msg = f"Raster not found: {raster_path.name}"
        else:
            with rasterio.open(raster_path) as src:
                window = from_bounds(west, south, east, north, src.transform)
                data = src.read(1, window=window).astype(float)
                nodata = src.nodata
            if nodata is not None:
                data = np.where(data == nodata, np.nan, data)

    # ── Render raster on dark background (OSM served separately) ─
    try:
        from PIL import Image as PILImage
        bg = PILImage.new("RGBA", (PX, PX), (20, 20, 30, 255))
        if data is not None:
            overlay = _raster_to_pil(data, kind)
            overlay = overlay.resize((PX, PX), PILImage.NEAREST)
            composite = PILImage.alpha_composite(bg, overlay).convert("RGB")
        else:
            composite = bg.convert("RGB")
        composite_arr = np.array(composite)
    except Exception:
        composite_arr = np.full((PX, PX, 3), 20, dtype=np.uint8)

    # ── Draw via matplotlib (polygon + legend only) ───────────
    fig, ax = plt.subplots(figsize=(3, 3), dpi=100)
    fig.patch.set_facecolor("#0f1117")
    ax.set_facecolor("#0f1117")

    ax.imshow(composite_arr, interpolation="bilinear")

    if missing_msg:
        ax.text(0.5, 0.5, missing_msg, ha="center", va="center",
                color="#ef4444", fontsize=8, transform=ax.transAxes,
                bbox=dict(facecolor="#0f1117", alpha=0.7, edgecolor="none"))

    # MODIS legend
    if kind == "modis" and data is not None:
        try:
            present = set(int(v) for v in data[~np.isnan(data)]) if not np.all(np.isnan(data)) else set()
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

    if polygon_geojson:
        h, w = composite_arr.shape[:2]
        _draw_polygon(ax, polygon_geojson, west, south, east, north, h, w)

    ax.set_xlim(0, composite_arr.shape[1])
    ax.set_ylim(composite_arr.shape[0], 0)  # y=0 at top
    ax.axis("off")
    plt.tight_layout(pad=0)

    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", pad_inches=0,
                facecolor=fig.get_facecolor())
    plt.close(fig)
    buf.seek(0)
    return base64.b64encode(buf.read()).decode("utf-8")


# ── Routes ────────────────────────────────────────────────────

@app.route("/")
def index():
    cities = load_cities()
    reviewed, total = get_progress(cities)
    start_index = find_next_pending(cities) or 0
    total_w = sum(SCORE_WEIGHTS.values())
    weights_display = {k: round(v/total_w, 2) for k, v in SCORE_WEIGHTS.items() if v > 0}
    return render_template("index.html",
        categories=list(enumerate(CATEGORIES)),
        total=total, reviewed=reviewed, start_index=start_index,
        score_weights=weights_display)


@app.route("/api/city/<int:city_index>")
def get_city(city_index):
    cities = load_cities()
    if city_index < 0 or city_index >= len(cities):
        return jsonify({"error": "Index out of range"}), 404

    city = cities[city_index]
    reviewed, total = get_progress(cities)

    try:
        polygon = json.loads(city.get("polygon_geojson", "{}"))
    except Exception:
        polygon = {}

    bbox = compute_bbox(polygon)

    def sf(v, d=0.0):
        try: return float(v)
        except: return d

    def si(v, d=0):
        try: return int(float(v))
        except: return d

    return jsonify({
        "index":            city_index,
        "bbox":             bbox,
        "total":            total,
        "reviewed":         reviewed,
        "city_name":        city.get("city_name", ""),
        "country":          city.get("country", ""),
        "country_iso3":     city.get("country_iso3", ""),
        "dev_group":        city.get("dev_group", ""),
        "lat":              sf(city.get("lat", 0)),
        "lon":              sf(city.get("lon", 0)),
        "score":            sf(city.get("score", 0)),
        "ntl_suspicion":    sf(city.get("ntl_suspicion", 0)),
        "modis_suspicion":  sf(city.get("modis_suspicion", 0)),
        "wsf_suspicion":    sf(city.get("wsf_suspicion", 0)),
        "rural_share":      sf(city.get("rural_share", 0)),
        "share_urban":      sf(city.get("share_urban", 0)),
        "ntl_mean":         sf(city.get("ntl_mean", 0)),
        "wsf_built_share":  sf(city.get("wsf_built_share", 0)),
        "population":       si(city.get("population", 0)),
        "area_km2":         sf(city.get("area_km2", 0)),
        "total_pixels":     si(city.get("total_pixels", 0)),
        "decision":         city.get("decision", ""),
        "category":         city.get("category", ""),
        "note":             city.get("note", ""),
        "wiki_signal":      city.get("wiki_signal", ""),
        "wiki_title":       city.get("wiki_title", ""),
        "wiki_population":  city.get("wiki_population", ""),
        "google_url":       city.get("google_url", ""),
        "polygon":          polygon,
    })


@app.route("/api/city_list")
def get_city_list():
    cities = load_cities()
    return jsonify([{
        "index": i, "city_name": c.get("city_name", ""),
        "country": c.get("country", ""), "score": c.get("score", ""),
        "decision": c.get("decision", ""), "category": c.get("category", ""),
        "wiki_signal": c.get("wiki_signal", ""),
    } for i, c in enumerate(cities)])


@app.route("/api/thumbnail/<int:city_index>/<kind>")
def thumbnail(city_index, kind):
    if kind not in ("ntl", "modis", "wsf"):
        return jsonify({"error": "kind must be ntl, modis or wsf"}), 400

    cache_key = (city_index, kind)
    if cache_key in _thumb_cache:
        return jsonify({"png_b64": _thumb_cache[cache_key]})

    cities = load_cities()
    if city_index < 0 or city_index >= len(cities):
        return jsonify({"error": "Index out of range"}), 404

    city = cities[city_index]
    try:
        lat  = float(city.get("lat", 0))
        lon  = float(city.get("lon", 0))
        polygon_geojson = None
        bbox = None
        try:
            polygon_geojson = json.loads(city.get("polygon_geojson", "{}"))
            bbox = compute_bbox(polygon_geojson, pad=0.5)
        except Exception:
            pass

        if bbox:
            west, south, east, north = bbox
        else:
            area = float(city.get("area_km2") or 100)
            pad  = min(2.0, max(0.3, (area ** 0.5) / 5))
            west, east   = lon - pad, lon + pad
            south, north = lat - pad, lat + pad

        png_b64 = _make_thumbnail(west, south, east, north, kind, polygon_geojson)
        _thumb_cache[cache_key] = png_b64
        return jsonify({"png_b64": png_b64})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/api/clear_cache", methods=["POST"])
def clear_cache():
    _thumb_cache.clear()
    return jsonify({"ok": True})


@app.route("/api/wiki_search")
def wiki_search_route():
    return jsonify(fetch_wikipedia_search(request.args.get("q", "")))


@app.route("/api/wiki_page")
def wiki_page_route():
    return jsonify(fetch_wikipedia_page(request.args.get("title", "")))


@app.route("/api/decide", methods=["POST"])
def decide():
    data       = request.get_json()
    city_index = data.get("index")
    decision   = data.get("decision", "")
    category   = data.get("category", "")
    note       = data.get("note", "")

    if decision not in ("keep", "ambiguous", "drop"):
        return jsonify({"error": "Invalid decision"}), 400
    if decision in ("ambiguous", "drop") and not category:
        return jsonify({"error": "Sub-reason required"}), 400
    if category == "Other" and not note.strip():
        return jsonify({"error": "Note required for Other"}), 400

    if not save_decision(city_index, decision, category, note):
        return jsonify({"error": "Save failed"}), 500

    cities = load_cities()
    next_idx = find_next_pending(cities, current_index=city_index)
    reviewed, total = get_progress(cities)
    return jsonify({"success": True, "next_index": next_idx,
                    "reviewed": reviewed, "total": total})


@app.route("/api/export")
def export():
    cities    = load_cities()
    dropped   = [c for c in cities if c.get("decision") == "drop"]
    ambiguous = [c for c in cities if c.get("decision") == "ambiguous"]
    kept      = [c for c in cities if c.get("decision") == "keep"]
    by_cat = {}
    for c in dropped + ambiguous:
        cat = c.get("category", "Unknown")
        by_cat[cat] = by_cat.get(cat, 0) + 1
    return jsonify({
        "total_reviewed":  sum(1 for c in cities if c.get("decision", "").strip()),
        "total_cities":    len(cities),
        "kept_count":      len(kept),
        "ambiguous_count": len(ambiguous),
        "dropped_count":   len(dropped),
        "by_category":     by_cat,
        "kept":            kept,
        "ambiguous":       ambiguous,
        "dropped":         dropped,
    })


@app.route("/api/country_context/<path:country>")
def country_context(country):
    """
    Return scatter plot data for all cities in the given country.
    Used by the context panel to show where the current city sits
    in its national distribution.
    """
    import pandas as pd

    if country in _country_cache:
        return jsonify(_country_cache[country])

    if not INDICATORS_PATH.exists():
        return jsonify({"error": "country_indicators.csv not found — run 06_score_cities.py"}), 404

    try:
        df = pd.read_csv(INDICATORS_PATH)
        cdf = df[df["country"] == country].copy()
        if cdf.empty:
            return jsonify({"error": f"No data for {country}"}), 404

        def clean(series):
            return [None if (v != v or v is None) else round(float(v), 4)
                    for v in series]

        result = {
            "country":      country,
            "n":            len(cdf),
            "wsf":          clean(cdf["wsf_built_share"]),
            "share_urban":  clean(cdf["share_urban"]),
            "ntl_mean":     clean(cdf["ntl_mean"]),
            "score":        clean(cdf["score"]),
            # Percentile distributions (20 bins)
            "wsf_pct":      [round(float(p), 4) for p in
                             cdf["wsf_built_share"].dropna().quantile(
                                 [i/20 for i in range(21)]).tolist()],
            "urban_pct":    [round(float(p), 4) for p in
                             cdf["share_urban"].dropna().quantile(
                                 [i/20 for i in range(21)]).tolist()],
            "ntl_pct":      [round(float(p), 4) for p in
                             cdf["ntl_mean"].dropna().quantile(
                                 [i/20 for i in range(21)]).tolist()],
        }
        _country_cache[country] = result
        return jsonify(result)
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == "__main__":
    if DATA_PATH.exists() and not BACKUP_PATH.exists():
        shutil.copy(DATA_PATH, BACKUP_PATH)
        print(f"Backup created: {BACKUP_PATH}")
    app.run(debug=FLASK_DEBUG, port=FLASK_PORT)