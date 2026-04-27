import os
import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
import rasterio.crs
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from rasterio.windows import from_bounds
from rasterio.warp import reproject, Resampling, calculate_default_transform
from rasterio.plot import show
from rasterstats import zonal_stats
import contextily as ctx

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

MODIS_PATH = "/Users/simon/Library/CloudStorage/GoogleDrive-<<<< email >>>>/My Drive/repo/cities-learning-dec-data//lc_mcd12q1v061.t5_c_500m_s_20210101_20211231_go_epsg.4326_v20230818.tif"

# ── PFT class definitions ──────────────────────────────────────
PFT_CLASSES = {
    0:  "water",
    1:  "evergreen_needleleaf",
    2:  "evergreen_broadleaf",
    3:  "deciduous_needleleaf",
    4:  "deciduous_broadleaf",
    5:  "shrub",
    6:  "grass",
    7:  "cereal_cropland",
    8:  "broadleaf_cropland",
    9:  "urban",
    10: "snow_ice",
    11: "barren",
    # 255 = fill/unclassified, excluded via nodata
}

PFT_COLORS = {
    0:  "#4a90d9",
    1:  "#1a7a1a",
    2:  "#2ecc71",
    3:  "#145a32",
    4:  "#27ae60",
    5:  "#8e6b3e",
    6:  "#c8d96f",
    7:  "#f5d76e",
    8:  "#f39c12",
    9:  "#e74c3c",
    10: "#ecf0f1",
    11: "#bdc3c7",
}

# ── Load UCDB ──────────────────────────────────────────────────
print("Loading UCDB data...")
ucdb = gpd.read_file("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
print("Original CRS:", ucdb.crs)
ucdb = ucdb.to_crs("EPSG:4326")

# ── Basel visualization ────────────────────────────────────────
print("Finding Basel in UCDB...")
basel = ucdb[ucdb["GC_UCN_MAI_2025"].str.contains("Basel", case=False, na=False)]
print("Matched UCDB entries:", basel["GC_UCN_MAI_2025"].values)

BASEL_LON, BASEL_LAT = 7.5886, 47.5596
pad = 0.05

if len(basel) > 0:
    bounds = basel.geometry.iloc[0].bounds  # (minx, miny, maxx, maxy) in WGS84
else:
    bounds = (BASEL_LON - 0.15, BASEL_LAT - 0.15,
              BASEL_LON + 0.15, BASEL_LAT + 0.15)

plot_bounds = (bounds[0]-pad, bounds[1]-pad, bounds[2]+pad, bounds[3]+pad)

# ── Read raster window in WGS84 ────────────────────────────────
with rasterio.open(MODIS_PATH) as src:
    src_crs = src.crs
    window = from_bounds(*plot_bounds, transform=src.transform)
    raster_wgs84 = src.read(1, window=window)
    src_transform = src.window_transform(window)

# ── Reproject raster to Web Mercator for display ───────────────
# Without this, pixels appear tilted at mid-latitudes (47N for Basel)
# because imshow stretches degree-spaced pixels into a Mercator canvas.
# Resampling.nearest preserves integer class values exactly.
dst_crs = rasterio.crs.CRS.from_epsg(3857)

dst_transform, dst_width, dst_height = calculate_default_transform(
    src_crs, dst_crs,
    raster_wgs84.shape[1], raster_wgs84.shape[0],
    left=plot_bounds[0], bottom=plot_bounds[1],
    right=plot_bounds[2], top=plot_bounds[3],
)

raster_3857 = np.empty((dst_height, dst_width), dtype=np.uint8)

reproject(
    source=raster_wgs84,
    destination=raster_3857,
    src_transform=src_transform,
    src_crs=src_crs,
    dst_transform=dst_transform,
    dst_crs=dst_crs,
    resampling=Resampling.nearest,
)

# ── Colormap ───────────────────────────────────────────────────
max_class = 11
cmap_colors = [PFT_COLORS.get(i, "#ffffff") for i in range(max_class + 1)]
cmap = mcolors.ListedColormap(cmap_colors)
bounds_cmap = np.arange(-0.5, max_class + 1.5, 1)
norm = mcolors.BoundaryNorm(bounds_cmap, cmap.N)

# ── Plot ───────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(10, 10))

# rasterio.plot.show() uses the affine transform directly
# so pixels are placed at their correct geographic positions
show(raster_3857, transform=dst_transform, ax=ax,
     cmap=cmap, norm=norm, alpha=0.65, zorder=2)

if len(basel) > 0:
    basel.to_crs("EPSG:3857").boundary.plot(
        ax=ax, color="black", linewidth=2, zorder=3, label="UCDB polygon")

ctx.add_basemap(ax, crs="EPSG:3857",
                source=ctx.providers.OpenStreetMap.Mapnik, zorder=1)

present = np.unique(raster_3857)
handles = [
    plt.Rectangle((0, 0), 1, 1,
                  facecolor=PFT_COLORS.get(v, "#fff"), alpha=0.8,
                  label=f"{v}: {PFT_CLASSES.get(v, 'unknown')}")
    for v in present if v in PFT_CLASSES
]
ax.legend(handles=handles, loc="lower right", fontsize=9, title="PFT Class")
ax.set_title("MODIS PFT Land Cover (t5) + UCDB Polygon — Basel", fontsize=13)

plt.tight_layout()
plt.savefig("basel_modis_pft.png", dpi=150, bbox_inches="tight")
plt.show()
print("Saved to basel_modis_pft.png")

# ── Zonal statistics ───────────────────────────────────────────
# Run on the original WGS84 raster — UCDB polygons are also in WGS84.
# The reprojection above was only for display.
print("\nComputing PFT class shares per UCDB polygon...")
stats = zonal_stats(
    ucdb,
    MODIS_PATH,
    categorical=True,
    nodata=255,         # PFT fill = 255, excluded from pixel counts
    all_touched=False,  # only pixels whose centre falls within polygon
)

# ── Build results dataframe ────────────────────────────────────
print("Building results dataframe...")
records = []
for i, s in enumerate(stats):
    # s = {pixel_value: count} for valid (non-nodata) pixels only
    total = sum(s.values()) if s else 0
    row = {
        "ID_UC_G0":     ucdb["ID_UC_G0"].iloc[i],
        "total_pixels": total,
    }
    for class_id, label in PFT_CLASSES.items():
        count = s.get(class_id, 0) if s else 0
        row[f"share_{label}"] = count / total if total > 0 else np.nan
    records.append(row)

results = pd.DataFrame(records)
share_cols = [f"share_{label}" for label in PFT_CLASSES.values()]

# ── Validation ─────────────────────────────────────────────────
# 1. Shares should sum to 1 per polygon (excluding NaN rows)
row_sums = results[share_cols].sum(axis=1)
bad_sums = results[(row_sums - 1).abs() > 0.001]
print(f"\nShare sum check: {len(bad_sums)} polygons where shares != 1 "
      f"(expected 0, except fully-nodata polygons)")

# 2. Any shares outside [0, 1]?
out_of_range = (results[share_cols] < 0).any(axis=1) | \
               (results[share_cols] > 1).any(axis=1)
print(f"Out-of-range shares [0,1]: {out_of_range.sum()} polygons")

# 3. Polygons with zero valid pixels (too small or fully nodata/ocean)
zero_px = (results["total_pixels"] == 0).sum()
print(f"Polygons with 0 valid pixels: {zero_px}")

# 4. Urban share sanity check
print(f"\nUrban share (share_urban) distribution:")
print(results["share_urban"].describe().round(4))
print(f"  share_urban == 0:   {(results['share_urban'] == 0).sum()}")
print(f"  share_urban > 0.3:  {(results['share_urban'] > 0.3).sum()}")
print(f"  share_urban > 0.5:  {(results['share_urban'] > 0.5).sum()}")

# ── Sanity ─────────────────────────────────────────────────
# 1. Check Basel specifically — should be high urban share, sensible ground truth
print(results[results["ID_UC_G0"].isin(basel["ID_UC_G0"])][
    ["ID_UC_G0", "share_urban", "share_grass", "share_cereal_cropland", 
     "share_evergreen_broadleaf", "total_pixels"]
])

# 2. Look at the low end — cities with share_urban == 0
# These are your false positive candidates
zero_urban = results[results["share_urban"] == 0].sort_values("total_pixels")
print(f"\n{len(zero_urban)} cities with zero urban pixels")
print(zero_urban[["ID_UC_G0", "total_pixels", "share_grass", 
                   "share_cereal_cropland", "share_barren"]].head(20))

# 3. Plot distribution to visually inspect
import matplotlib.pyplot as plt
results["share_urban"].hist(bins=50, figsize=(10, 4))
plt.axvline(0.47, color="red", linestyle="--", label="mean")
plt.xlabel("Urban pixel share (MODIS PFT)")
plt.ylabel("Count of UCDB polygons")
plt.title("Distribution of urban share across UCDB polygons")
plt.legend()
plt.tight_layout()
plt.savefig("urban_share_distribution.png", dpi=150)
plt.show()

# ── Export ─────────────────────────────────────────────────────
results["share_non_urban"] = 1 - results["share_urban"].fillna(0)

os.makedirs("data/modis", exist_ok=True)
results.to_csv("data/modis/modis_pft_shares.csv", index=False)
print(f"\nSaved {len(results)} rows to data/modis/modis_pft_shares.csv")
print(results[["ID_UC_G0", "share_urban", "share_non_urban",
               "total_pixels"]].describe().round(3))