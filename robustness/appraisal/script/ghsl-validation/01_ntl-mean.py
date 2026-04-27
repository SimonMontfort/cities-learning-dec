from rasterstats import zonal_stats
import geopandas as gpd
import numpy as np
import pandas as pd
import os
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.colors import LogNorm
from matplotlib.patches import Polygon as MplPolygon
import rasterio
from rasterio.windows import from_bounds

os.chdir("/Users/simon/Documents/repo/cities-learning-dec/robustness/appraisal")

print("Loading UCDB data...")
ucdb = gpd.read_file("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
ucdb["geometry"] = ucdb.geometry.segmentize(1000)  # 1km intervals
ucdb = ucdb.to_crs("EPSG:4326")

NTL_PATH = "/Users/simon/Library/CloudStorage/GoogleDrive-<<<< email >>>>/My Drive/repo/cities-learning-dec-data/night-time-lights/VNL_npp_2024_global_vcmslcfg_v2_c202502261200.average_masked.dat.tif"

# ── Pick an example city ───────────────────────────────────────
EXAMPLE_CITY = ucdb[ucdb["GC_UCN_MAI_2025"].str.contains("Basel", case=False, na=False)].iloc[[0]]
city_name = EXAMPLE_CITY["GC_UCN_MAI_2025"].values[0]

# Reproject city to raster CRS (EPSG:4326) just for the plot
with rasterio.open(NTL_PATH) as src:
    raster_crs = src.crs

# city_4326 = EXAMPLE_CITY.to_crs(raster_crs)
city_4326 = EXAMPLE_CITY
bounds = city_4326.total_bounds  # [west, south, east, north]

# ── Read raster window over city bbox (no masking) ────────────
with rasterio.open(NTL_PATH) as src:
    window = from_bounds(*bounds, transform=src.transform)
    data = src.read(1, window=window).astype(float)
    win_transform = src.window_transform(window)

# Treat 0 as nodata
data = np.where(data == 0, np.nan, data)

# ── Compute stats ──────────────────────────────────────────────
mean_with_zeros    = np.nanmean(np.where(np.isnan(data), 0, data))
mean_without_zeros = np.nanmean(data)
lit_pixel_share    = np.sum(~np.isnan(data)) / data.size

# ── Plot ───────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(7, 6))

vmin = np.nanpercentile(data[~np.isnan(data)], 2)  if np.any(~np.isnan(data)) else 0.1
vmax = np.nanpercentile(data[~np.isnan(data)], 98) if np.any(~np.isnan(data)) else 1.0
vmin = max(vmin, 0.01)

im = ax.imshow(
    data,
    cmap="inferno",
    norm=LogNorm(vmin=vmin, vmax=vmax),
    interpolation="nearest",
    extent=[bounds[0], bounds[2], bounds[1], bounds[3]],  # geographic extent
    origin="upper",
)

# Draw polygon outline in geographic coordinates
def draw_polygon_outline(ax, geom):
    parts = geom.geoms if geom.geom_type == "MultiPolygon" else [geom]
    for part in parts:
        x, y = part.exterior.xy
        ax.plot(x, y, color="#00ffcc", linewidth=1.5, alpha=0.9)

draw_polygon_outline(ax, city_4326.geometry.values[0])

cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
cbar.set_label("Radiance (nW·cm⁻²·sr⁻¹)", fontsize=9)

ax.set_title(f"Night-time lights — {city_name}", fontsize=11, fontweight="bold")
ax.set_xlabel("Longitude")
ax.set_ylabel("Latitude")

stats_text = (
    f"Mean (lit pixels only): {mean_without_zeros:.2f}\n"
    f"Mean (incl. dark pixels): {mean_with_zeros:.2f}\n"
    f"Lit pixel share: {lit_pixel_share:.1%}"
)
ax.text(0.02, 0.03, stats_text, transform=ax.transAxes,
        fontsize=7.5, color="white", va="bottom",
        bbox=dict(facecolor="#0f1117", alpha=0.75, edgecolor="none", pad=4))

plt.tight_layout()
plt.savefig(f"data/ntl/ntl_{city_name.replace(' ', '_')}.png", dpi=150, bbox_inches="tight")
plt.show()
print(f"Saved plot for {city_name}")


# ── Zonal stats for all cities ────────────────────────────────
print("Calculating zonal statistics for night-time lights data...")
stats = zonal_stats(
    ucdb,
    NTL_PATH,
    stats=["mean", "std"],
    nodata=0,
    all_touched=False,
)

print("Saving results to CSV...")
results = pd.DataFrame({
    "ID_UC_G0": ucdb["ID_UC_G0"].values,
    "ntl_mean": [s.get("mean") for s in stats],
    "ntl_std":  [s.get("std")  for s in stats],
})
results.to_csv("data/ntl/ntl_zonal_stats.csv", index=False)
print("Done.")