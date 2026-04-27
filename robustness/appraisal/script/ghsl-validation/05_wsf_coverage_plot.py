"""
wsf_coverage_plot.py
--------------------
Plots WSF2019 tile footprints on a world map using filenames only —
no raster reading, runs in seconds.

Usage:
    python wsf_coverage_plot.py
"""

import re
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import Rectangle
from pathlib import Path

# ── Configure ─────────────────────────────────────────────────
OUT_DIR  = Path("/Users/simon/Library/CloudStorage/GoogleDrive-<<<< email >>>>/My Drive/repo/cities-learning-dec-data/wfs-2019")   # same as wsf_download.py
OUT_PNG  = OUT_DIR / "wsf2019_coverage_map.png"
# ──────────────────────────────────────────────────────────────

def main():
    tifs = sorted(OUT_DIR.glob("WSF2019_v1_*.tif"))
    if not tifs:
        print(f"No WSF tiles found in {OUT_DIR}")
        return
    print(f"Found {len(tifs)} tiles — building map from filenames only...")

    tiles = []
    for tif in tifs:
        m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif.name)
        if m:
            tiles.append((int(m.group(1)), int(m.group(2))))

    lons = [t[0] for t in tiles]
    lats = [t[1] for t in tiles]
    print(f"  Lon: {min(lons)}° to {max(lons)}°")
    print(f"  Lat: {min(lats)}° to {max(lats)}°")

    # ── World background from Natural Earth via matplotlib ────
    try:
        import cartopy.crs as ccrs
        import cartopy.feature as cfeature
        fig = plt.figure(figsize=(20, 11), dpi=150)
        ax  = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
        ax.set_extent([-180, 180, -90, 90], crs=ccrs.PlateCarree())
        ax.add_feature(cfeature.OCEAN,      facecolor="#0d2240")
        ax.add_feature(cfeature.LAND,       facecolor="#1a1f2e")
        ax.add_feature(cfeature.COASTLINE,  linewidth=0.4, edgecolor="#445566")
        ax.add_feature(cfeature.BORDERS,    linewidth=0.2, edgecolor="#334455")
        ax.gridlines(color="#ffffff11", linewidth=0.3)
        use_cartopy = True
    except ImportError:
        use_cartopy = False
        fig, ax = plt.subplots(figsize=(20, 11), dpi=150)
        ax.set_facecolor("#1a1f2e")
        ax.fill_between([-180, 180], -90, 90, color="#0d2240")
        ax.set_xlim(-180, 180)
        ax.set_ylim(-90, 90)
        ax.set_xticks(range(-180, 181, 30))
        ax.set_yticks(range(-90, 91, 30))
        ax.tick_params(colors="#556080", labelsize=7)
        ax.grid(color="#ffffff11", linewidth=0.3)
        for spine in ax.spines.values():
            spine.set_edgecolor("#2e3352")

    fig.patch.set_facecolor("#0a0e1a")

    # Draw tile footprints
    coord_set = set(tiles)
    for lon, lat in tiles:
        rect = Rectangle(
            (lon, lat), 2, 2,
            linewidth=0.2,
            edgecolor="#ffffff33",
            facecolor="#e05020",
            alpha=0.65,
            zorder=3,
            transform=ax.transData if not use_cartopy else __import__('cartopy').crs.PlateCarree()
        )
        ax.add_patch(rect)

    ax.set_title(
        f"WSF2019 — {len(tiles)} tiles  |  "
        f"lon {min(lons)}°–{max(lons)}°  "
        f"lat {min(lats)}°–{max(lats)}°",
        color="#e8eaf6", fontsize=12, pad=10
    )

    # Legend
    legend_handle = mpatches.Patch(facecolor="#e05020", alpha=0.65, label="Downloaded tile (2°×2°)")
    ax.legend(handles=[legend_handle], loc="lower left", fontsize=8, framealpha=0.6,
            facecolor="#0f1117", edgecolor="#2e3352", labelcolor="white")

    fig.tight_layout()
    fig.savefig(OUT_PNG, bbox_inches="tight", facecolor=fig.get_facecolor(), dpi=150)
    plt.close(fig)
    print(f"✓ Saved: {OUT_PNG}")


if __name__ == "__main__":
    main()