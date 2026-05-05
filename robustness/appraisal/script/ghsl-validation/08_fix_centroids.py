"""
fix_centroids.py
----------------
Fills lat/lon in cities_review.csv from the UC_centroids layer of the UCDB GeoPackage.

    python fix_centroids.py
"""

import csv
import geopandas as gpd
from pathlib import Path
import sys
csv.field_size_limit(sys.maxsize)
from config import BASE_DIR, DATA_DIR, UCDB_PATH

CSV_PATH  = BASE_DIR / "data/ghsl_appraisal/cities_review.csv"
LAYER     = "GHS_UCDB_GLOBE_R2024A_small"

CSV_ID_COL  = "id"        # column in cities_review.csv
UCDB_ID_COL = "ID_UC_G0"  # column in UC_centroids layer — confirm below if error


def main():
    print(f"Reading layer '{LAYER}' from: {UCDB_PATH}")
    gdf = gpd.read_file(UCDB_PATH, layer=LAYER, rows=1)
    print(f"  Available columns: {list(gdf.columns)[:10]} ...")

    # Reload with just the columns we need
    gdf = gpd.read_file(UCDB_PATH, layer=LAYER)

    # Auto-detect ID column if not found
    id_col = UCDB_ID_COL
    if id_col not in gdf.columns:
        candidates = [c for c in gdf.columns if "ID" in c.upper() or "UC" in c.upper()]
        print(f"  '{id_col}' not found. Candidates: {candidates}")
        raise SystemExit(f"Set UCDB_ID_COL to one of: {candidates}")

    # Auto-detect lat/lon columns
    lat_col = next((c for c in gdf.columns if "LAT" in c.upper()), None)
    lon_col = next((c for c in gdf.columns if "LON" in c.upper()), None)
    if not lat_col or not lon_col:
        # Fall back to geometry centroid
        print("  No LAT/LON columns found — computing from geometry")
        gdf["_lat"] = gdf.geometry.y
        gdf["_lon"] = gdf.geometry.x
        lat_col, lon_col = "_lat", "_lon"

    print(f"  Using: id={id_col}, lat={lat_col}, lon={lon_col}")

    # Reproject to WGS84 and use geometry centroids
    if gdf.crs is not None and gdf.crs.to_epsg() != 4326:
        print(f"  Reprojecting from {gdf.crs.to_epsg()} to WGS84...")
        gdf = gdf.to_crs(epsg=4326)

    lookup = {
        int(row[id_col]): (round(row.geometry.y, 6), round(row.geometry.x, 6))
        for _, row in gdf.iterrows()
        if row[id_col] is not None and row.geometry is not None
    }
    print(f"  Loaded {len(lookup):,} centroids")

    # Read CSV
    with open(CSV_PATH, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        fieldnames = list(reader.fieldnames)
        rows = list(reader)

    if "lat" not in fieldnames:
        fieldnames.append("lat")
    if "lon" not in fieldnames:
        fieldnames.append("lon")

    fixed = skipped = missing = 0
    for row in rows:
        lat = row.get("lat", "").strip()
        lon = row.get("lon", "").strip()
        # Skip only if looks like valid WGS84 degrees (abs < 180)
        try:
            if lat and lat not in ("", "None") and abs(float(lat)) <= 90 and abs(float(lon)) <= 180:
                skipped += 1
                continue
        except (ValueError, TypeError):
            pass
        try:
            ucdb_id = int(float(row[CSV_ID_COL]))
        except (ValueError, TypeError, KeyError):
            missing += 1
            continue
        if ucdb_id in lookup:
            row["lat"] = round(lookup[ucdb_id][0], 6)
            row["lon"] = round(lookup[ucdb_id][1], 6)
            fixed += 1
        else:
            missing += 1

    with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"\n✓ Done: {fixed} filled, {skipped} already had coords, {missing} not matched")
    print("\nSample:")
    for row in rows[:3]:
        print(f"  {row.get('city_name',''):25s}  lat={row.get('lat')}  lon={row.get('lon')}")


if __name__ == "__main__":
    main()
