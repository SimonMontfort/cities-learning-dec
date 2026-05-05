"""
config.py
---------
Single source of truth for all paths, weights, and thresholds.
Edit this file to reconfigure the pipeline without touching any other script.
"""

from pathlib import Path

# ── Directories ────────────────────────────────────────────────
BASE_DIR   = Path("/Users/simon/Documents/repo/cities-learning-dec/robustness/appraisal")
DATA_DIR   = BASE_DIR / "data"
WSF_DIR    = Path("/Volumes/data/wsf2019")

# ── Input data ─────────────────────────────────────────────────
UCDB_PATH  = DATA_DIR / "GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg"
NTL_PATH   = DATA_DIR / "ntl/ntl_zonal_stats.csv"
NTL_COG    = DATA_DIR / "ntl/ntl_cog.tif"
MODIS_PATH = DATA_DIR / "modis/modis_pft_shares.csv"
MODIS_COG  = DATA_DIR / "modis/modis_cog.tif"
WSF_PATH   = DATA_DIR / "wsf-2019/wsf_stats.csv"

# ── Output ─────────────────────────────────────────────────────
OUT_PATH    = DATA_DIR / "ghsl_appraisal/cities_review.csv"
BACKUP_PATH = DATA_DIR / "ghsl_appraisal/cities_review.backup.csv"

# ── UCDB column names (R2024A release) ─────────────────────────
UCDB_COLS = {
    "id":         "ID_UC_G0",
    "city_name":  "GC_UCN_MAI_2025",
    "country":    "GC_CNT_GAD_2025",
    "dev_group":  "GC_DEV_WIG_2025",   # income group
    "lat":        "GC_UCC_LAT_2025",
    "lon":        "GC_UCC_LON_2025",
    "population": "GC_POP_TOT_2025",
    "area_km2":   "GC_UCA_KM2_2025",
}

# ── Scoring weights (must sum to any positive value; normalised automatically) ─
SCORE_WEIGHTS = {
    "modis": 0.30,
    "wsf":   0.70,
    # ntl excluded from scoring; kept as display indicator only
}

# ── Two-stage queue thresholds ─────────────────────────────────
# Stage 1: global bottom percentile → unconditional queue entry
STAGE1_GLOBAL_PCT     = 0.10   # bottom 10% globally

# Stage 2: within-country bottom %, applied to countries where
# Stage 1 already flagged > STAGE2_MIN_FLAG_RATE of their cities
STAGE2_MIN_FLAG_RATE  = 0.05   # country must have >5% flagged in Stage 1

# Within-country percentile thresholds by income group
STAGE2_PCT = {
    "Low income":    0.20,
    "Lower Middle":  0.15,
    "Upper Middle":  0.15,
    "High income":   0.05,
    "-":             0.15,   # unknown → treat as Lower Middle
}

# ── App settings ───────────────────────────────────────────────
FLASK_PORT  = 5050
FLASK_DEBUG = True

# ── Review categories ──────────────────────────────────────────
CATEGORIES = [
    "Does not exist / pure data artefact",
    "Exists but too small / unreliable population estimate",
    "Administrative unit, not a city",
    "Other",
]

# ── MODIS PFT colormap ─────────────────────────────────────────
MODIS_PFT_COLORS = {
    0:  ("#1a6faf", "Water"),
    1:  ("#1a7a1a", "Evergreen needle forest"),
    2:  ("#2db82d", "Evergreen broad forest"),
    3:  ("#4daf4d", "Deciduous needle forest"),
    4:  ("#80cc80", "Deciduous broad forest"),
    5:  ("#b2e6b2", "Mixed forest"),
    6:  ("#c8a050", "Shrubland"),
    7:  ("#d4c87a", "Grassland / Savanna"),
    8:  ("#e8e840", "Cropland"),
    9:  ("#e03030", "Urban"),
    10: ("#f0f0ff", "Snow / Ice"),
    11: ("#c8b080", "Barren / Sparse"),
}