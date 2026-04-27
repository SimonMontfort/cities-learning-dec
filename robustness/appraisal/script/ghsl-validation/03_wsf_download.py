"""
wsf_download.py
---------------
Downloads all WSF2019 tiles from a URL list file.
Checks global coverage and plots a test tile for Basel.

Usage:
    python wsf_download.py

Adjust OUT_DIR and URL_LIST_PATH at the top.
"""

import os
import re
import time
import urllib.request
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import rasterio
from rasterio.merge import merge
from rasterio.windows import from_bounds

# ── Configure ─────────────────────────────────────────────────
OUT_DIR       = Path("/Users/simon/Library/CloudStorage/GoogleDrive-<<<< email >>>>/My Drive/repo/cities-learning-dec-data/wsf-2019")          
URL_LIST_PATH = Path("/Users/simon/Library/CloudStorage/GoogleDrive-<<<< email >>>>/My Drive/repo/cities-learning-dec-data/failed_tiles.txt")  
MAX_WORKERS   = 4       # parallel downloads (be polite to DLR server)
RETRY         = 3       # retries per tile
TEST_CITY     = "Basel"
TEST_LAT, TEST_LON = 47.5596, 7.5886   # Basel centroid
TEST_PAD_DEG  = 0.15                   # ~16km around Basel
# ──────────────────────────────────────────────────────────────


def parse_urls(url_list_path: Path):
    urls = []
    with open(url_list_path) as f:
        for line in f:
            line = line.strip()
            if line.startswith("http"):
                urls.append(line)
    return urls


def tile_coords(url: str):
    """Extract (lon, lat) origin from tile filename."""
    m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", url)
    if m:
        return int(m.group(1)), int(m.group(2))
    return None


def check_coverage(urls: list[str]) -> dict:
    """
    Parse tile grid and check for missing tiles across all world regions.
    WSF2019 tiles are 2°×2°, covering land areas.
    Returns dict with coverage stats and any suspicious gaps.
    """
    coords = [tile_coords(u) for u in urls]
    coords = [c for c in coords if c is not None]

    lons = sorted(set(c[0] for c in coords))
    lats = sorted(set(c[1] for c in coords))

    print(f"\n── Coverage check ────────────────────────────────")
    print(f"  Total tiles:      {len(coords)}")
    print(f"  Lon range:        {min(lons)}° to {max(lons)}°")
    print(f"  Lat range:        {min(lats)}° to {max(lats)}°")
    print(f"  Unique lons:      {len(lons)}")
    print(f"  Unique lats:      {len(lats)}")

    # Check expected world regions
    regions = {
        "Africa":        [(-20, -40, 55, 40)],
        "Europe":        [(-12, 35, 45, 72)],
        "Asia":          [(25, 0, 150, 75)],
        "N. America":    [(-170, 15, -50, 75)],
        "S. America":    [(-85, -60, -30, 15)],
        "Oceania":       [(110, -50, 180, 10)],
    }
    coord_set = set(coords)
    print(f"\n  Region spot-checks (sample tile presence):")
    for region, boxes in regions.items():
        found = []
        for (w, s, e, n) in boxes:
            sample = [(lon, lat) for lon in range(w, e, 10)
                                  for lat in range(s, n, 10)
                                  if (lon, lat) in coord_set]
            found.extend(sample)
        status = f"✓ {len(found)} sample tiles" if found else "⚠ NO TILES FOUND"
        print(f"    {region:15s}: {status}")

    return {"total": len(coords), "lons": lons, "lats": lats, "coords": coord_set}


def download_tile(url: str, out_dir: Path, retry: int = 3) -> tuple[str, bool, str]:
    """Download a single tile. Returns (url, success, message)."""
    fname = url.split("/")[-1]
    dest  = out_dir / fname
    if dest.exists() and dest.stat().st_size > 1000:
        return url, True, "already exists"

    for attempt in range(retry):
        try:
            req = urllib.request.Request(
                url, headers={"User-Agent": "WSF2019-downloader/1.0"}
            )
            with urllib.request.urlopen(req, timeout=60) as resp:
                data = resp.read()
            dest.write_bytes(data)
            return url, True, f"{len(data)//1024} KB"
        except Exception as e:
            if attempt < retry - 1:
                time.sleep(2 ** attempt)
            else:
                return url, False, str(e)


def download_all(urls: list[str], out_dir: Path):
    out_dir.mkdir(parents=True, exist_ok=True)
    total   = len(urls)
    '''
    done    = sum(1 for u in urls if (out_dir / u.split("/")[-1]).exists()
                  and (out_dir / u.split("/")[-1]).stat().st_size > 1000)
    print(f"\n── Downloading ───────────────────────────────────")
    print(f"  {done}/{total} tiles already on disk, downloading remainder...")

    to_download = [u for u in urls
                   if not ((out_dir / u.split("/")[-1]).exists()
                           and (out_dir / u.split("/")[-1]).stat().st_size > 1000)]
    '''
    to_download = urls

    if not to_download:
        print("  All tiles already downloaded.")
        return

    failed = []
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        futures = {pool.submit(download_tile, u, out_dir): u for u in to_download}
        for i, fut in enumerate(as_completed(futures), 1):
            url, ok, msg = fut.result()
            fname = url.split("/")[-1]
            status = "✓" if ok else "✗"
            print(f"  [{i:4d}/{len(to_download)}] {status} {fname}  {msg}")
            if not ok:
                failed.append((url, msg))

    print(f"\n  Done. {len(to_download)-len(failed)} downloaded, {len(failed)} failed.")
    if failed:
        print("  Failed tiles:")
        for url, msg in failed:
            print(f"    {url.split('/')[-1]}: {msg}")
        # Write failed list for retry
        fail_path = out_dir / "failed_tiles.txt"
        fail_path.write_text("\n".join(u for u, _ in failed))
        print(f"  Failed list saved to: {fail_path}")


def plot_basel_test(out_dir: Path):
    """Find the tile(s) covering Basel, crop, and plot."""
    print(f"\n── Basel test plot ───────────────────────────────")

    west  = TEST_LON - TEST_PAD_DEG
    east  = TEST_LON + TEST_PAD_DEG
    south = TEST_LAT - TEST_PAD_DEG
    north = TEST_LAT + TEST_PAD_DEG

    # Find tiles covering Basel (lon 7, lat 47 → tile origin at lon=6, lat=46 for 2° tiles)
    tif_files = list(out_dir.glob("WSF2019_v1_*.tif"))
    covering  = []
    for tif in tif_files:
        m = re.search(r"WSF2019_v1_(-?\d+)_(-?\d+)\.tif", tif.name)
        if not m:
            continue
        tile_lon, tile_lat = int(m.group(1)), int(m.group(2))
        # Tile covers [tile_lon, tile_lon+2] × [tile_lat, tile_lat+2]
        if (tile_lon <= TEST_LON < tile_lon + 2 and
                tile_lat <= TEST_LAT < tile_lat + 2):
            covering.append(tif)

    if not covering:
        print(f"  ⚠ No downloaded tile covers Basel yet.")
        print(f"  Expected tile: WSF2019_v1_6_46.tif (lon=6, lat=46)")
        return

    print(f"  Using tile(s): {[t.name for t in covering]}")

    # Read and crop
    datasets = [rasterio.open(t) for t in covering]
    if len(datasets) > 1:
        mosaic, transform = merge(datasets)
        data = mosaic[0]
    else:
        with rasterio.open(covering[0]) as src:
            window = from_bounds(west, south, east, north, src.transform)
            data = src.read(1, window=window)

    for ds in datasets:
        ds.close()

    print(f"  Crop shape: {data.shape}, unique values: {np.unique(data)}")

    fig, ax = plt.subplots(figsize=(6, 6), dpi=120)
    fig.patch.set_facecolor("#0f1117")
    ax.set_facecolor("#111")
    # WSF: 255=settlement, 0=no settlement
    display = np.where(data == 255, 1.0, 0.0)
    ax.imshow(display, cmap="hot", interpolation="nearest",
              extent=[west, east, south, north], vmin=0, vmax=1)
    ax.set_title(f"WSF2019 — {TEST_CITY} (white = settlement)",
                 color="white", fontsize=11, pad=8)
    ax.set_xlabel("Longitude", color="#8890b0", fontsize=9)
    ax.set_ylabel("Latitude",  color="#8890b0", fontsize=9)
    ax.tick_params(colors="#8890b0", labelsize=8)
    for spine in ax.spines.values():
        spine.set_edgecolor("#2e3352")

    out_png = out_dir / f"test_{TEST_CITY.lower()}.png"
    fig.savefig(out_png, bbox_inches="tight", facecolor=fig.get_facecolor(), dpi=120)
    plt.close(fig)
    print(f"  Plot saved: {out_png}")


def main():
    print(f"WSF2019 Downloader")
    print(f"  URL list:  {URL_LIST_PATH}")
    print(f"  Output:    {OUT_DIR}")

    if not URL_LIST_PATH.exists():
        raise FileNotFoundError(f"URL list not found: {URL_LIST_PATH}")

    urls = parse_urls(URL_LIST_PATH)
    print(f"  URLs found: {len(urls)}")

    # 1. Coverage check
    check_coverage(urls)

    # 2. Download
    download_all(urls, OUT_DIR)

    # 3. Basel test plot
    plot_basel_test(OUT_DIR)


if __name__ == "__main__":
    main()