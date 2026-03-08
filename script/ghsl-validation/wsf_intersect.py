"""
wfs_intersect.py
-------------
Computes WSF2019 built pixel count and share for all UCDB cities.
Groups polygons by tile to minimise I/O — each tile opened once.

Output: data/wsf-2019/wsf_stats.csv  (columns: id, wsf_pixel_count, wsf_built_share)
"""

import csv
import json
import re
from collections import defaultdict
from pathlib import Path

import numpy as np
import rasterio
from rasterio.windows import from_bounds
from rasterio.features import geometry_mask
from rasterio.transform import from_bounds as transform_from_bounds
from shapely.geometry import shape, box
from tqdm import tqdm

# ── Configure ─────────────────────────────────────────────────
BASE_DIR  = Path("/Users/simon/Documents/repo/cities-learning-dec")
CSV_PATH  = BASE_DIR / "data/ghsl_appraisal/cities_review.csv"
WSF_DIR   = Path("")
OUT_PATH  = BASE_DIR / "data/wsf-2019/wsf_stats.csv"
WSF_NODATA = 0    # WSF: 255 = settlement, 0 = no settlement / nodata
# ──────────────────────────────────────────────────────────────


def tile_origin(tif_path):
    """Extract (lon, lat) SW corner from WSF tile filename."""
    m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif_path.name)
    if m:
        return int(m.group(1)), int(m.group(2))
    return None


def polygon_bbox(geojson):
    """Return (west, south, east, north) of a GeoJSON geometry."""
    try:
        geom = shape(geojson)
        return geom.bounds  # (minx, miny, maxx, maxy)
    except Exception:
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
    tile_files = list(WSF_DIR.glob("WSF2019_v1_*.tif"))
    tile_index = {}
    for t in tile_files:
        origin = tile_origin(t)
        print(f"Indexed tile: {t.name}, origin: {origin}")
        if origin:
            tile_index[origin] = t
    print(f"Indexed {len(tile_index)} WSF tiles from {WSF_DIR}")

    # ── Load cities ──────────────────────────────────────────
    with open(CSV_PATH, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    print(f"Loaded {len(rows)} cities")

    # ── Group cities by tile(s) they touch ──────────────────
    # city_tiles[city_index] = list of tile paths
    city_tiles = {}
    skipped = 0
    for i, row in enumerate(rows):
        geojson_str = row.get("polygon_geojson", "")
        if not geojson_str:
            skipped += 1
            continue
        try:
            geojson = json.loads(geojson_str)
            bbox = polygon_bbox(geojson)
            if bbox is None:
                skipped += 1
                continue
            west, south, east, north = bbox
            covering = tiles_for_bbox(west, south, east, north, tile_index)
            if covering:
                city_tiles[i] = (geojson, covering)
            else:
                skipped += 1  # no tile downloaded yet for this city
        except Exception:
            skipped += 1

    print(f"Cities with tile coverage: {len(city_tiles)}, skipped: {skipped}")

    # ── Process tile by tile ─────────────────────────────────
    # Invert: for each tile, process all cities it covers
    tile_to_cities = defaultdict(list)
    for city_idx, (geojson, tile_paths) in city_tiles.items():
        for tp in tile_paths:
            tile_to_cities[tp].append(city_idx)

    results = {}  # city_index → (built, total)

    for tile_path, city_indices in tqdm(tile_to_cities.items(),
                                         desc="Processing tiles"):
        with rasterio.open(tile_path) as src:
            tile_transform = src.transform
            tile_bounds    = src.bounds

        for city_idx in city_indices:
            if city_idx in results:
                continue  # already computed (city spans multiple tiles — handled below)
            geojson, tile_paths = city_tiles[city_idx]
            built, total = wsf_stats_for_polygon(geojson, tile_paths)
            results[city_idx] = (built, total)

    # ── Write output ─────────────────────────────────────────
    with open(OUT_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["id", "wsf_built_pixels", "wsf_total_pixels", "wsf_built_share"])
        for i, row in enumerate(rows):
            if i in results:
                built, total = results[i]
                share = round(built / total, 4) if total > 0 else 0.0
                writer.writerow([row.get("id", i), built, total, share])
            else:
                writer.writerow([row.get("id", i), "", "", ""])

    print(f"\n✓ Written: {OUT_PATH}")
    print(f"  Cities with results: {len(results)}")

    # Quick summary
    shares = [results[i][0]/results[i][1] for i in results if results[i][1] > 0]
    if shares:
        shares = np.array(shares)
        print(f"  wsf_built_share — mean: {shares.mean():.3f}, "
              f"median: {np.median(shares):.3f}, "
              f"zero: {(shares==0).sum()}")


if __name__ == "__main__":
    main()