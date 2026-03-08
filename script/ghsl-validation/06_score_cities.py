"""
06_score_cities.py
------------------
Builds cities_review.csv using a two-stage queue:

  Stage 1 — global bottom STAGE1_GLOBAL_PCT on compound score → unconditional
  Stage 2 — within-country bottom N% for countries where Stage 1
             flagged > STAGE2_MIN_FLAG_RATE of their cities.
             N% varies by income group (STAGE2_PCT in config.py).

Queue is ordered country-by-country, countries sorted by Stage 1 flag rate
descending, cities within each country sorted by score ascending (worst first).

All thresholds and paths live in config.py.
"""

import json
import logging
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd
import country_converter as coco

warnings.filterwarnings("ignore")
logging.basicConfig(level=logging.INFO, format="%(message)s")
log = logging.getLogger(__name__)

import sys
sys.path.insert(0, str(Path(__file__).parent))
from config import (
    UCDB_PATH, NTL_PATH, MODIS_PATH, WSF_PATH, OUT_PATH,
    UCDB_COLS, SCORE_WEIGHTS,
    STAGE1_GLOBAL_PCT, STAGE2_PCT,
)

COUNTRY_OVERRIDES = {"México": "MEX"}


def rank_normalize(series):
    return series.rank(pct=True, na_option="bottom")


def polygon_to_geojson(geom):
    try:
        return json.dumps(geom.__geo_interface__)
    except Exception:
        return "{}"


def load_ucdb(path):
    log.info(f"Loading UCDB: {path}")
    gdf = gpd.read_file(path)
    log.info(f"  {len(gdf)} urban centres")
    rename = {v: k for k, v in UCDB_COLS.items() if v in gdf.columns}
    gdf = gdf.rename(columns=rename)
    for key, orig in UCDB_COLS.items():
        if key not in gdf.columns:
            log.warning(f"  ⚠ '{orig}' not found → '{key}' missing")
    gdf["geometry"] = gdf["geometry"].segmentize(100)
    gdf = gdf.to_crs("EPSG:4326")
    if "lat" not in gdf.columns or gdf["lat"].isna().all():
        log.info("  Deriving lat/lon from polygon centroids")
        gdf["lon"] = gdf.geometry.centroid.x
        gdf["lat"] = gdf.geometry.centroid.y
    return gdf


def add_iso3(gdf):
    if "country" not in gdf.columns:
        gdf["country_iso3"] = "not found"
        return gdf
    cc = coco.CountryConverter()
    logging.getLogger("country_converter").setLevel(logging.ERROR)
    # Apply overrides only for ISO3 lookup — do NOT mutate the display name
    lookup_series = gdf["country"].replace(COUNTRY_OVERRIDES)
    gdf["country_iso3"] = cc.pandas_convert(
        series=lookup_series, to="ISO3", not_found="not found"
    )
    n = (gdf["country_iso3"] == "not found").sum()
    if n:
        bad = gdf.loc[gdf["country_iso3"] == "not found", "country"].unique()[:5]
        log.warning(f"  ⚠ ISO3 not found for {n} rows: {list(bad)}")
    return gdf


def merge_stats(gdf, path, cols, label):
    if not Path(path).exists():
        log.warning(f"  ⚠ {label} not found: {path} — skipping")
        return gdf
    df = pd.read_csv(path)
    if "ID_UC_G0" in df.columns:
        df = df.rename(columns={"ID_UC_G0": "id"})
    df["id"]  = pd.to_numeric(df["id"],  errors="coerce").astype("Int64")
    gdf["id"] = pd.to_numeric(gdf["id"], errors="coerce").astype("Int64")
    available = [c for c in cols if c in df.columns]
    gdf = gdf.merge(df[["id"] + available], on="id", how="left")
    log.info(f"  {label} merged — missing: {gdf[available[0]].isna().sum()}/{len(gdf)}")
    return gdf


def compute_score(gdf, weights):
    total_w = sum(weights.values())
    w = {k: v / total_w for k, v in weights.items()}
    components = {}

    if w.get("modis", 0) > 0:
        if "share_urban" not in gdf.columns:
            gdf["share_urban"] = np.nan
        gdf["rural_share"]     = 1 - gdf["share_urban"].fillna(0.5)
        gdf["modis_suspicion"] = rank_normalize(gdf["rural_share"])
        components["modis"]    = ("modis_suspicion", w["modis"])

    if w.get("wsf", 0) > 0:
        wsf_ok = False
        if "wsf_built_share" in gdf.columns:
            n_valid = gdf["wsf_built_share"].notna().sum()
            if n_valid >= 10:
                gdf["wsf_built_share_filled"] = gdf["wsf_built_share"].fillna(0)
                gdf["wsf_suspicion"] = 1 - rank_normalize(gdf["wsf_built_share_filled"])
                components["wsf"]    = ("wsf_suspicion", w["wsf"])
                log.info(f"  WSF included (n_valid={n_valid})")
                wsf_ok = True
            else:
                log.warning(f"  ⚠ WSF: only {n_valid} valid values — redistributing weight")
        else:
            log.warning("  ⚠ WSF column missing — redistributing weight")

        if not wsf_ok:
            remaining = {k: v for k, v in w.items() if k != "wsf" and v > 0}
            r_total   = sum(remaining.values())
            if r_total > 0:
                for k in remaining:
                    if k in components:
                        col, old_w = components[k]
                        components[k] = (col, old_w + w["wsf"] * remaining[k] / r_total)

    # NTL: display only
    if "ntl_mean" in gdf.columns:
        gdf["ntl_suspicion"] = 1 - rank_normalize(gdf["ntl_mean"].fillna(0))

    if not components:
        log.warning("  ⚠ No components — score = 0")
        gdf["score"] = 0.0
        return gdf

    gdf["score"] = sum(gdf[col] * wt for col, wt in components.values())
    formula = " + ".join(f"{wt:.2f}×{col}" for col, wt in components.values())
    log.info(f"  Score = {formula}")
    log.info(f"  Score range: {gdf['score'].min():.3f} – {gdf['score'].max():.3f}")
    return gdf


def _test_scoring(gdf):
    """
    Sanity checks on score direction and plausibility.
    Exits with error if any check fails.
    """
    import sys
    errors = []

    # 1. Score range
    smin, smax = gdf["score"].min(), gdf["score"].max()
    if not (smin < 0.05 and smax > 0.95):
        errors.append(f"Score range too narrow: {smin:.4f}–{smax:.4f} (expected ~0–1)")

    # 2. Direction: cities with high WSF built share should have LOW score
    top_wsf    = gdf.nlargest(100,  "wsf_built_share")["score"].mean()
    bottom_wsf = gdf.nsmallest(100, "wsf_built_share")["score"].mean()
    if top_wsf >= bottom_wsf:
        errors.append(
            f"Score direction wrong: top-WSF cities mean={top_wsf:.3f} "
            f">= bottom-WSF mean={bottom_wsf:.3f}. "
            f"High WSF should → low suspicion score."
        )

    # 3. Known genuine large cities should not appear in top 5% suspicious
    top5pct_thresh = gdf["score"].quantile(0.95)
    known_real = ["London", "Paris", "Tokyo", "Berlin", "Sydney", "Toronto",
                  "Amsterdam", "Vienna", "Stockholm", "Zurich"]
    for name in known_real:
        match = gdf[gdf["city_name"].str.contains(name, case=False, na=False)]
        if not match.empty:
            s = match.iloc[0]["score"]
            if s >= top5pct_thresh:
                errors.append(f"Known real city '{name}' score={s:.3f} is in top 5% suspicious")

    # 4. Bangladesh: top-20% by score should have lower WSF than bottom-20%
    bd = gdf[gdf["country"] == "Bangladesh"]
    if len(bd) > 10:
        n20        = max(1, int(len(bd) * 0.2))
        top_wsf_bd = bd.nlargest(n20,  "score")["wsf_built_share"].mean()
        bot_wsf_bd = bd.nsmallest(n20, "score")["wsf_built_share"].mean()
        if top_wsf_bd >= bot_wsf_bd:
            errors.append(
                f"Bangladesh direction check failed: "
                f"top-20% suspicious mean WSF={top_wsf_bd:.3f} "
                f">= bottom-20% mean WSF={bot_wsf_bd:.3f}"
            )

    if errors:
        log.error("\n── SCORING TESTS FAILED ────────────────────────")
        for e in errors:
            log.error(f"  ✗ {e}")
        log.error("────────────────────────────────────────────────")
        sys.exit(1)

    log.info("  ✓ Scoring tests passed: direction correct, range 0–1, known cities safe")


def build_queue(gdf):
    """
    Stage 1: within-country TOP N% of suspicion score by income group,
             applied to ALL countries.
             HIGH score = high suspicion = should be reviewed.
             Selects score >= quantile(1 - pct), i.e. the most suspicious cities.

    Stage 2: global top STAGE1_GLOBAL_PCT as safety net for single-city
             outliers in countries that contributed nothing via Stage 1.

    Queue order:
      1. Income tier: Low → Lower Middle → Upper Middle → High → unknown
      2. Within tier: countries ordered by mean suspicion rank desc
         (most systematically suspicious countries first)
      3. Within country: score descending (most suspicious city first)
    """
    log.info(f"\n── Queue construction ────────────────────────────")

    INCOME_ORDER = ["Low income", "Lower Middle", "Upper Middle", "High income", "-"]

    # ── Diagnostic: check score direction ────────────────────
    top5    = gdf.nlargest(5, "score")[["country", "wsf_built_share",
                                         "share_urban", "score"]]
    bottom5 = gdf.nsmallest(5, "score")[["country", "wsf_built_share",
                                          "share_urban", "score"]]
    log.info("\n  TEST — top 5 scores (should be low wsf/urban = suspicious):")
    log.info(top5.to_string(index=False))
    log.info("  TEST — bottom 5 scores (should be high wsf/urban = genuine cities):")
    log.info(bottom5.to_string(index=False))

    # ── Diagnostic: Bangladesh spot-check ────────────────────
    bd = gdf[gdf["country"] == "Bangladesh"].sort_values("score", ascending=False)
    if not bd.empty:
        log.info(f"\n  TEST — Bangladesh top 3 suspicious (score desc):")
        log.info(bd[["city_name", "wsf_built_share", "share_urban", "score"]
                    ].head(3).to_string(index=False))
        log.info(f"  TEST — Bangladesh bottom 3 (least suspicious):")
        log.info(bd[["city_name", "wsf_built_share", "share_urban", "score"]
                    ].tail(3).to_string(index=False))

    # ── Stage 1: within-country TOP N% suspicion ─────────────
    stage1       = pd.Series(False, index=gdf.index)
    country_stats = []

    for country, cdf in gdf.groupby("country"):
        dev  = cdf["dev_group"].iloc[0] if "dev_group" in cdf.columns else "-"
        pct  = STAGE2_PCT.get(dev, STAGE2_PCT["-"])
        # TOP N% most suspicious = score >= (1-pct) quantile
        thr  = cdf["score"].quantile(1 - pct)
        mask = (gdf["country"] == country) & (gdf["score"] >= thr)
        stage1 = stage1 | mask

        # Mean score for country ordering — higher = more systematically suspicious
        # (using raw score not rank so countries with genuinely high scores rise)
        mean_susp = cdf["score"].mean()
        country_stats.append({
            "country":   country,
            "dev_group": dev,
            "n_s1":      mask.sum(),
            "mean_susp": mean_susp,
            "pct_used":  pct,
            "thr":       thr,
        })

    stats_df = pd.DataFrame(country_stats)

    # ── Diagnostic: per-country threshold check ───────────────
    log.info("\n  TEST — Stage 1 thresholds for key countries:")
    test_countries = ["Bangladesh", "India", "China", "Afghanistan", "Germany"]
    for tc in test_countries:
        row = stats_df[stats_df["country"] == tc]
        if not row.empty:
            r = row.iloc[0]
            n_country = len(gdf[gdf["country"] == tc])
            log.info(f"    {tc:15s} pct={r['pct_used']:.0%}  "
                     f"thr={r['thr']:.4f}  "
                     f"n_flagged={r['n_s1']}/{n_country}")

    log.info(f"\n  Stage 1 (within-country top N%): {stage1.sum()} cities across "
             f"{(stats_df['n_s1'] > 0).sum()} countries")

    # ── Stage 2: global top N% safety net ────────────────────
    thresh_global = gdf["score"].quantile(1 - STAGE1_GLOBAL_PCT)
    global_high   = gdf["score"] >= thresh_global
    stage2        = global_high & ~stage1
    log.info(f"  Stage 2 (global top p{STAGE1_GLOBAL_PCT*100:.0f} safety net): "
             f"+{stage2.sum()} additional cities")

    in_queue = stage1 | stage2
    log.info(f"  Total queue: {in_queue.sum()} cities")

    # ── Build sort key ────────────────────────────────────────
    income_rank = {g: i for i, g in enumerate(INCOME_ORDER)}
    stats_df["income_rank"] = stats_df["dev_group"].map(
        lambda d: income_rank.get(d, len(INCOME_ORDER))
    )
    stats_df = stats_df.sort_values(
        ["income_rank", "mean_susp"],
        ascending=[True, False]
    ).reset_index(drop=True)
    stats_df["country_order"] = stats_df.index

    queue_df = gdf[in_queue].copy()
    queue_df = queue_df.merge(
        stats_df[["country", "country_order"]], on="country", how="left"
    )
    # Score descending within country (most suspicious first)
    queue_df = queue_df.sort_values(
        ["country_order", "score"],
        ascending=[True, False]
    ).drop(columns=["country_order"])

    # ── Summary ──────────────────────────────────────────────
    log.info(f"\n── Queue by country (top 20, developing first) ──")
    summary = (
        queue_df.groupby(["country", "dev_group"] if "dev_group" in queue_df.columns
                         else ["country"])
        .agg(n_queue=("score", "count"),
             score_min=("score", "min"),
             score_max=("score", "max"),
             wsf_mean=("wsf_built_share", "mean"))
        .reset_index()
        .merge(stats_df[["country", "income_rank", "mean_susp"]], on="country", how="left")
        .sort_values(["income_rank", "mean_susp"], ascending=[True, False])
        .head(20)
        .drop(columns=["income_rank", "mean_susp"])
    )
    log.info(summary.to_string(index=False))

    return queue_df


def build_review_csv():
    log.info("=" * 54)
    log.info("UCDB Two-Stage Suspicion Scoring")
    log.info("=" * 54)
    total_w = sum(SCORE_WEIGHTS.values())
    for k, v in SCORE_WEIGHTS.items():
        log.info(f"  {k:6s}: {v/total_w:.2f}")
    log.info("  ntl   : display only")

    gdf = load_ucdb(UCDB_PATH)
    gdf = add_iso3(gdf)
    gdf = merge_stats(gdf, NTL_PATH,   ["ntl_mean"],                               "NTL")
    gdf = merge_stats(gdf, MODIS_PATH, ["share_urban", "total_pixels"],            "MODIS")
    gdf = merge_stats(gdf, WSF_PATH,   ["wsf_built_pixels", "wsf_total_pixels",
                                         "wsf_built_share"],                        "WSF")

    log.info("\nComputing scores…")
    gdf = compute_score(gdf, SCORE_WEIGHTS)
    _test_scoring(gdf)

    queue_df = build_queue(gdf)

    log.info("\nSerialising geometries…")
    queue_df["polygon_geojson"] = queue_df["geometry"].apply(polygon_to_geojson)

    # Write full indicator table for ALL cities (used by app country context panel)
    indicator_cols = ["id", "country", "dev_group", "score",
                      "wsf_built_share", "share_urban", "ntl_mean"]
    ind_path = OUT_PATH.parent / "country_indicators.csv"
    gdf[[c for c in indicator_cols if c in gdf.columns]].to_csv(ind_path, index=False)
    log.info(f"Country indicators written: {ind_path} ({len(gdf)} cities)")

    out_cols = [
        "id", "city_name", "country", "country_iso3", "dev_group",
        "lat", "lon", "population", "area_km2",
        "score",
        "ntl_suspicion", "ntl_mean",
        "modis_suspicion", "rural_share", "share_urban", "total_pixels",
        "wsf_suspicion", "wsf_built_share", "wsf_built_pixels",
        "polygon_geojson",
    ]
    df_out = queue_df[[c for c in out_cols if c in queue_df.columns]].copy()

    # ── Carry forward existing decisions by city id ───────────
    decision_cols = ["decision", "category", "note", "wiki_signal",
                     "wiki_title", "wiki_population", "google_url", "timestamp"]
    for col in decision_cols:
        df_out[col] = ""

    if OUT_PATH.exists():
        try:
            old = pd.read_csv(OUT_PATH, dtype=str)
            old["id"] = pd.to_numeric(old["id"], errors="coerce").astype("Int64")
            df_out["id"] = pd.to_numeric(df_out["id"], errors="coerce").astype("Int64")
            coded = old[old["decision"].str.strip().ne("")]
            carried = coded[["id"] + [c for c in decision_cols if c in coded.columns]]
            df_out = df_out.merge(carried, on="id", how="left", suffixes=("", "_old"))
            for col in decision_cols:
                old_col = f"{col}_old"
                if old_col in df_out.columns:
                    df_out[col] = df_out[old_col].fillna("").where(
                        df_out[old_col].notna(), df_out[col]
                    )
                    df_out.drop(columns=[old_col], inplace=True)
            n_carried = (df_out["decision"].str.strip().ne("")).sum()
            n_new     = len(df_out) - (old["id"].isin(df_out["id"])).sum()
            log.info(f"  Carried forward {n_carried} existing decisions")
            log.info(f"  New cities added to queue: {n_new}")
        except Exception as e:
            log.warning(f"  ⚠ Could not carry forward decisions: {e}")
    else:
        log.info("  No existing CSV — fresh queue")

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df_out.to_csv(OUT_PATH, index=False)
    log.info(f"\n✓ Written: {OUT_PATH}  ({len(df_out)} cities)")
    log.info("\nTop 10 queue entries:")
    log.info(df_out[["city_name", "country", "dev_group", "score",
                      *[c for c in ["modis_suspicion", "wsf_suspicion"]
                        if c in df_out.columns]]
                    ].head(10).to_string(index=False))


if __name__ == "__main__":
    build_review_csv()