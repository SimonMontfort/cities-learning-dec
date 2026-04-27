"""
E3_expand_queue.py
==================
Adds unreviewed UCDB cities to cities_review.csv for countries that
have not yet reached their recall stopping criterion.

Pipeline position: THIRD — run after E1 and E2.

Batch size per country comes from E2's extrapolation_summary.csv:
  - status="extrapolated"  → use n_more_estimated directly
  - status="indeterminate" → trend was flat/rising; use fallback heuristic
    (add all remaining cities up to --max-per-country cap)
  - status="insufficient_data" → too few reviewed; add a fixed small batch
    (DEFAULT_SMALL_BATCH) to gather more evidence

Skips:
  - Countries already stopped (p_biased <= P_STOP)
  - High income countries (low FP rate, not worth expanding)
  - Countries with 0 UCDB cities

Countries with 0 FPs found so far are included with a small initial batch
(DEFAULT_SMALL_BATCH) to gather evidence for E2.

Inputs:
  data/ghsl_appraisal/country_stopping_summary.csv   ← from E1
  data/ghsl_appraisal/extrapolation_summary.csv      ← from E2
  data/GHS_UCDB_GLOBE_R2024A_V1_0/...gpkg
  data/ghsl_appraisal/country_indicators.csv          ← scores for all cities

Outputs:
  data/ghsl_appraisal/cities_review.csv              ← updated in place
  data/ghsl_appraisal/cities_review.expand_queue_backup_<timestamp>.csv
  data/ghsl_appraisal/stage_batch_log.csv

Run:
    python E3_expand_queue.py [--dry-run] [--max-per-country N]
"""

import argparse
import json
import logging
import os
import sys
import warnings
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd

warnings.filterwarnings("ignore")
logging.basicConfig(level=logging.INFO, format="%(message)s")
log = logging.getLogger(__name__)

sys.path.insert(0, str(Path(__file__).parent))
from config import UCDB_PATH, UCDB_COLS, OUT_PATH, BASE_DIR

os.chdir(BASE_DIR)

# ── Config ─────────────────────────────────────────────────────────────────────

STOPPING_CSV   = Path("data/ghsl_appraisal/country_stopping_summary.csv")
EXTRAP_CSV     = Path("data/ghsl_appraisal/extrapolation_summary.csv")
BATCH_LOG      = Path("data/ghsl_appraisal/stage_batch_log.csv")

P_STOP         = round(1 - 0.90, 10)   # 0.10 -- must match E1/E2
SKIP_GROUPS    = {"High income"}
MAX_PER_COUNTRY_DEFAULT = 300
DEFAULT_SMALL_BATCH     = 25   # for countries with insufficient_data / 0 FPs yet

# ── Helpers ────────────────────────────────────────────────────────────────────

def polygon_to_geojson(geom):
    try:
        return json.dumps(geom.__geo_interface__)
    except Exception:
        return "{}"


# ══════════════════════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true",
                        help="Print plan without writing anything")
    parser.add_argument("--max-per-country", type=int,
                        default=MAX_PER_COUNTRY_DEFAULT,
                        help=f"Cap on cities added per country "
                             f"(default {MAX_PER_COUNTRY_DEFAULT})")
    args = parser.parse_args()
    dry  = args.dry_run
    cap  = args.max_per_country

    log.info("=" * 60)
    log.info("E3  Expand queue")
    log.info("=" * 60)
    if dry:
        log.info("  *** DRY RUN — nothing will be written ***")

    # ── [1] Load and validate inputs ──────────────────────────────────────────

    log.info("\n[1/4] Loading inputs…")

    # CHECK: E1 and E2 outputs must exist
    for path, script in [(STOPPING_CSV, "E1"), (EXTRAP_CSV, "E2")]:
        if not path.exists():
            log.error(f"✗  {path} not found — run {script} first")
            sys.exit(1)

    # CHECK: warn if review CSV is newer than E1/E2 outputs
    review_mtime = os.path.getmtime(OUT_PATH)
    for path, script in [(STOPPING_CSV, "E1"), (EXTRAP_CSV, "E2")]:
        if review_mtime > os.path.getmtime(path):
            log.warning(f"  ⚠  cities_review.csv is newer than {path.name} — "
                        f"consider re-running {script}")

    stop_df   = pd.read_csv(STOPPING_CSV)
    extrap_df = pd.read_csv(EXTRAP_CSV)

    # CHECK: config consistency — P_STOP implied by can_stop_biased
    if "p_biased" in stop_df.columns and "can_stop_biased" in stop_df.columns:
        inconsistent = stop_df[
            (stop_df["can_stop_biased"]) &
            stop_df["p_biased"].notna() &
            (stop_df["p_biased"] > P_STOP + 0.001)
        ]
        if not inconsistent.empty:
            log.warning(f"  ⚠  {len(inconsistent)} rows: can_stop_biased=True "
                        f"but p_biased > {P_STOP} — E1 may use different P_STOP")

    log.info(f"  ✓  Stopping summary  : {len(stop_df)} countries")
    log.info(f"  ✓  Extrapolation     : {len(extrap_df)} countries "
             f"({(extrap_df['status']=='extrapolated').sum()} extrapolated, "
             f"{(extrap_df['status']=='full_tail_required').sum()} full_tail_required, "
             f"{(extrap_df['status']=='indeterminate').sum()} indeterminate, "
             f"{(extrap_df['status']=='insufficient_data').sum()} insufficient)")

    # Merge: use E2's n_more_estimated where available
    merged = stop_df.merge(
        extrap_df[["country", "n_more_estimated", "status",
                   "trend_slope", "trend_r2"]],
        on="country", how="left"
    )

    # Countries to expand:
    #   - Not already stopped
    #   - Not high income
    #   - Present in UCDB (n_ucdb_total > 0)
    #   Note: countries with n_fps == 0 are included -- they get a small batch
    #   to gather initial evidence. Excluding them would mean we never add
    #   their remaining cities to review.
    to_expand = merged[
        (merged["dev_group"].notna()) &
        (~merged["dev_group"].isin(SKIP_GROUPS)) &
        (merged["n_ucdb_total"] > 0) &
        (merged["p_biased"].isna() | (merged["p_biased"] > P_STOP))
    ].copy()

    # Separate count for informational logging
    n_with_fps    = (to_expand["n_fps"] > 0).sum()
    n_without_fps = (to_expand["n_fps"] == 0).sum()
    log.info(f"  Countries to expand  : {len(to_expand)} "
             f"({n_with_fps} with FPs, {n_without_fps} with 0 FPs -- will get small batch)")

    # ── [2] Load review CSV and UCDB ──────────────────────────────────────────

    log.info(f"\n[2/4] Loading review CSV and UCDB…")

    review = pd.read_csv(OUT_PATH, dtype=str)
    review["id"] = pd.to_numeric(review["id"], errors="coerce").astype("Int64")
    already_queued_ids = set(review["id"].dropna())
    log.info(f"  Cities in queue      : {len(review):,}")

    gdf = gpd.read_file(UCDB_PATH)
    rename = {v: k for k, v in UCDB_COLS.items() if v in gdf.columns}
    gdf = gdf.rename(columns=rename)
    gdf["id"] = pd.to_numeric(
        gdf.get("id", pd.Series(dtype=float)), errors="coerce"
    ).astype("Int64")
    gdf = gdf.to_crs("EPSG:4326")

    # Covariates + scores from country_indicators.csv (all 11,422 cities)
    # country_indicators.csv has: id, country, dev_group, score,
    #   wsf_built_share, share_urban, ntl_mean
    # We merge ALL of these so new rows have full covariates for scatter plots.
    indicators_path = OUT_PATH.parent / "country_indicators.csv"
    INDICATOR_COLS  = ["score", "wsf_built_share", "share_urban", "ntl_mean"]
    if indicators_path.exists():
        ind = pd.read_csv(indicators_path)
        ind["id"] = pd.to_numeric(ind["id"], errors="coerce").astype("Int64")
        available_ind_cols = [c for c in INDICATOR_COLS if c in ind.columns]
        gdf = gdf.merge(
            ind[["id"] + available_ind_cols],
            on="id", how="left", suffixes=("", "_ind")
        )
        # Prefer indicator values over any stale UCDB values for overlapping cols
        for col in available_ind_cols:
            ind_col = f"{col}_ind"
            if ind_col in gdf.columns:
                gdf[col] = gdf[ind_col].combine_first(gdf.get(col, pd.Series(dtype=float)))
                gdf.drop(columns=[ind_col], inplace=True)
        log.info(f"  Indicators loaded    : {len(ind):,} cities from country_indicators.csv")
        log.info(f"  Indicator cols       : {available_ind_cols}")
    else:
        # Fallback: pull score only from review CSV
        score_lookup = (
            review[["id", "score"]]
            .dropna(subset=["score"])
            .assign(score=lambda d: pd.to_numeric(d["score"], errors="coerce"))
            .set_index("id")["score"]
        )
        gdf["score"] = gdf["id"].map(score_lookup)
        log.warning("  ⚠  country_indicators.csv not found — using review CSV scores, covariates will be missing")

    # Fallback score for cities with no indicator entry (uses wsf rank if available)
    if "wsf_built_share" in gdf.columns:
        gdf["score"] = gdf["score"].fillna(
            1 - gdf["wsf_built_share"].rank(pct=True)
        )

    # Compute derived covariates from the merged indicators:
    #   rural_share, and the three suspicion columns (global ranks).
    # These mirror 06_score_cities.py compute_score() so the app displays
    # consistent suspicion bars and scatter positions for new batch cities.
    if "share_urban" in gdf.columns:
        gdf["rural_share"]     = 1 - gdf["share_urban"].fillna(0.5)
        gdf["modis_suspicion"] = gdf["rural_share"].rank(pct=True, na_option="bottom")
    if "ntl_mean" in gdf.columns:
        gdf["ntl_suspicion"]   = 1 - gdf["ntl_mean"].fillna(0).rank(pct=True, na_option="bottom")
    if "wsf_built_share" in gdf.columns:
        gdf["wsf_suspicion"]   = 1 - gdf["wsf_built_share"].fillna(0).rank(pct=True, na_option="bottom")

    n_missing_score = gdf["score"].isna().sum()
    if n_missing_score > 0:
        log.warning(f"  ⚠  {n_missing_score} UCDB cities have no score — "
                    f"will sort to bottom of country queue")

    log.info(f"  UCDB cities          : {len(gdf):,}")

    # ── [3] Determine batch size per country and build rows ───────────────────

    log.info(f"\n[3/4] Building batch…")
    log.info(f"\n  {'Country':<35} {'p_now':>6}  {'ω':>5}  "
             f"{'source':>14}  {'n_add':>6}  {'available':>9}")
    log.info(f"  {'-'*35} {'-'*6}  {'-'*5}  {'-'*14}  {'-'*6}  {'-'*9}")

    out_cols = [
        "id", "city_name", "country", "country_iso3", "dev_group",
        "lat", "lon", "population", "area_km2",
        "score", "ntl_suspicion", "ntl_mean",
        "modis_suspicion", "rural_share", "share_urban", "total_pixels",
        "wsf_suspicion", "wsf_built_share", "wsf_built_pixels",
        "polygon_geojson",
        "decision", "category", "note", "wiki_signal",
        "wiki_title", "wiki_population", "google_url", "timestamp",
    ]

    batch_log = []
    new_rows  = []

    for _, row in to_expand.sort_values(
        "p_biased", ascending=False, na_position="last"
    ).iterrows():
        country   = row["country"]
        dev_group = row["dev_group"]
        n_ucdb    = int(row["n_ucdb_total"])
        n_fps     = int(row["n_fps"])
        n_reviewed= int(row["n_reviewed"])
        omega     = float(row["omega"]) if pd.notna(row["omega"]) else 1.0
        p_biased  = row["p_biased"] if pd.notna(row["p_biased"]) else None
        status    = row.get("status", None)
        n_more    = row.get("n_more_estimated", None)
        r2        = row.get("trend_r2", None)

        # Cities for this country not yet in queue
        c_gdf = gdf[
            (gdf["country"] == country) &
            (~gdf["id"].isin(already_queued_ids))
        ].copy()

        n_available = len(c_gdf)
        if n_available == 0:
            log.info(f"  {country:<35} — no unqueued UCDB cities")
            continue

        # Determine batch size from E2 status
        if status == "extrapolated" and pd.notna(n_more):
            n_add_est = int(n_more)
            if n_add_est == 0:
                # Boundary case: extrapolation projects stopping at or before
                # current position, but p_biased is still > P_STOP (just barely).
                # A single new city provides almost no evidence. Add a small
                # buffer so there's enough signal to either cross the threshold
                # or reveal the trend was wrong.
                n_add_est = DEFAULT_SMALL_BATCH
                batch_src = f"E2 (R2={r2:.2f}) +buffer"
            else:
                batch_src = f"E2 (R2={r2:.2f})" if pd.notna(r2) else "E2"
        elif status == "full_tail_required":
            # Linear trend too shallow to reach p_stop within remaining cities.
            # Add everything available up to cap.
            n_add_est = n_available
            batch_src = "full_tail(!)"
            log.warning(f"  !  {country}: full tail required -- "
                        f"stopping criterion may need multiple batch rounds. "
                        f"Adding {min(n_available, cap)} cities.")
        elif status == "indeterminate":
            # Trend not falling -- add all remaining, up to cap
            n_add_est = n_available
            batch_src = "fallback(indet)"
        elif n_fps == 0:
            # No FPs found yet -- need a small initial batch
            n_add_est = DEFAULT_SMALL_BATCH
            batch_src = "initial(0fp)"
        else:
            # insufficient_data or no E2 entry -- small batch to gather evidence
            n_add_est = DEFAULT_SMALL_BATCH
            batch_src = "fallback(small)"

        n_add = min(max(n_add_est, 1), cap, n_available)

        p_label = f"{p_biased:.3f}" if p_biased is not None else "n/a"
        log.info(f"  {country:<35} {p_label:>6}  {omega:>5.1f}  "
                 f"{batch_src:>14}  {n_add:>6}  {n_available:>9}")

        # Take highest-scoring (most suspicious) unreviewed cities first
        c_gdf = c_gdf.sort_values("score", ascending=False, na_position="last").head(n_add)

        # Build output rows
        c_gdf = c_gdf.copy()
        c_gdf["polygon_geojson"] = c_gdf["geometry"].apply(polygon_to_geojson)
        for col in ["decision", "category", "note", "wiki_signal",
                    "wiki_title", "wiki_population", "google_url", "timestamp"]:
            c_gdf[col] = ""
        if "lat" not in c_gdf.columns or c_gdf["lat"].isna().all():
            c_gdf["lon"] = c_gdf.geometry.centroid.x
            c_gdf["lat"] = c_gdf.geometry.centroid.y

        new_rows.append(c_gdf[[c for c in out_cols if c in c_gdf.columns]])

        batch_log.append({
            "country":            country,
            "dev_group":          dev_group,
            "p_biased_before":    p_biased,
            "omega":              omega,
            "n_added":            n_add,
            "n_more_estimated":   n_add_est,
            "batch_source":       batch_src,
            "trend_r2":           r2,
            "n_fps_before":       n_fps,
            "n_reviewed_before":  n_reviewed,
            "n_ucdb_total":       n_ucdb,
            "run_timestamp":      datetime.now().isoformat(timespec="seconds"),
        })

        already_queued_ids.update(c_gdf["id"].dropna())

    # ── [4] Checks and write ──────────────────────────────────────────────────

    if not new_rows:
        log.info("\nNothing to add — all countries covered or no unqueued cities.")
        return

    appended    = pd.concat(new_rows, ignore_index=True)
    total_added = len(appended)

    log.info(f"\n[4/4] Checks and write…")

    # CHECK: no duplicates introduced
    new_ids = appended["id"].dropna()
    dup_ids = new_ids[new_ids.isin(set(review["id"].dropna()))]
    if not dup_ids.empty:
        log.error(f"✗  {len(dup_ids)} duplicate IDs would be introduced — aborting")
        sys.exit(1)
    log.info(f"  ✓  No duplicate IDs")

    # CHECK: all new rows have a score
    missing_score = appended["score"].isna().sum() if "score" in appended else 0
    if missing_score > 0:
        log.warning(f"  ⚠  {missing_score} new rows missing score")
    else:
        log.info(f"  ✓  All new rows have a score")

    # CHECK: new rows have empty decision (ready for review)
    if "decision" in appended.columns:
        non_empty = (appended["decision"].fillna("").str.strip() != "").sum()
        if non_empty > 0:
            log.warning(f"  ⚠  {non_empty} new rows have non-empty decision — unexpected")
        else:
            log.info(f"  ✓  All new rows have empty decision (ready for review)")

    log.info(f"\n  Cities to add        : {total_added:,}")
    log.info(f"  New queue size       : {len(review) + total_added:,}")

    if dry:
        log.info("\n  *** DRY RUN — nothing written ***")
        log.info("\n  Per-country summary:")
        for entry in sorted(batch_log, key=lambda x: -x["n_added"]):
            log.info(f"    {entry['country']:<35} +{entry['n_added']:>4}  "
                     f"source={entry['batch_source']}  "
                     f"(p_before={str(entry['p_biased_before'])[:5]})")
        return

    # Write
    updated = pd.concat([review, appended], ignore_index=True)

    ts     = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup = OUT_PATH.with_name(f"cities_review.expand_queue_backup_{ts}.csv")
    review.to_csv(backup, index=False)
    log.info(f"  Backup written       : {backup.name}")

    updated.to_csv(OUT_PATH, index=False)
    log.info(f"  Updated review CSV   : {OUT_PATH}  ({len(updated):,} rows)")

    # Append to batch log
    log_df = pd.DataFrame(batch_log)
    if BATCH_LOG.exists():
        log_df = pd.concat([pd.read_csv(BATCH_LOG), log_df], ignore_index=True)
    log_df.to_csv(BATCH_LOG, index=False)
    log.info(f"  Batch log updated    : {BATCH_LOG}")

    # Flow diagram summary
    log.info("\n── Flow diagram numbers ──────────────────────────────────────")
    log.info(f"  Before expansion : {len(review):,} cities in queue")
    log.info(f"  Added this run   : +{total_added:,}")
    log.info(f"  New total        : {len(updated):,}")
    log.info("\n  Per-country (largest batches first):")
    for entry in sorted(batch_log, key=lambda x: -x["n_added"]):
        log.info(f"    {entry['country']:<35} +{entry['n_added']:>4}  "
                 f"({entry['dev_group']}, {entry['batch_source']}, "
                 f"p_before={str(entry['p_biased_before'])[:5]})")

    log.info("\n── E3 complete ───────────────────────────────────────────────")
    log.info("  Next step: review new cities, then re-run E1 → E2 → E3")


if __name__ == "__main__":
    main()