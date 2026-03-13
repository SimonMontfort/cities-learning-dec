"""
E1_buscar_country_analysis.py
==============================
Per-country stopping criteria analysis using buscarpy.

Pipeline position: FIRST — run this before E2 and E3.

Inputs:
  data/ghsl_appraisal/cities_review.csv
  data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg

Outputs:
  data/ghsl_appraisal/country_stopping_summary.csv   <- consumed by E2 and E3
  plots/buscar/overview_stopping_criteria.png         <- two-panel: p-value + omega
  plots/buscar/recall_frontiers_by_country.png

Key decisions:
  - N per country = full UCDB count (not just queue) -- conservative
  - FP = any decision != "keep" (drop + ambiguous)
  - omega computed from within-country score-percentile odds ratio of FPs vs kept
    (within-country rank, not global, so country-level signal is preserved)
  - Both conservative (omega=1) and biased-urn (omega) p-values reported
  - RECALL_TARGET and CONFIDENCE must match E2 exactly

Run:
    python E1_buscar_country_analysis.py
"""

import os
import re
import sys
import warnings
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import fiona
from buscarpy import calculate_h0, recall_frontier

warnings.filterwarnings("ignore")

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from config import BASE_DIR

os.chdir(BASE_DIR)

# -- Config --------------------------------------------------------------------

CSV_PATH   = "data/ghsl_appraisal/cities_review.csv"
GPKG_PATH  = "data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg"
PLOT_DIR   = "plots/buscar"
TABLE_DIR  = "data/ghsl_appraisal"
OUT_CSV    = os.path.join(TABLE_DIR, "country_stopping_summary.csv")

MIN_REVIEWED  = 10
MIN_FPS       = 0
RECALL_TARGET = 0.90   # keep in sync with E2
CONFIDENCE    = 0.90   # keep in sync with E2
P_STOP        = round(1 - CONFIDENCE, 10)   # 0.10

# Countries in these groups are NOT expanded by E3.
# E1 still computes statistics for them but marks them accordingly.
SKIP_GROUPS   = {"High income"}

GROUP_COLORS = {
    "Low income":    "#e05c3a",
    "Lower Middle":  "#f0a030",
    "Upper Middle":  "#4f8eff",
    "High income":   "#a0a0c0",
    "-":             "#888888",
}

os.makedirs(PLOT_DIR, exist_ok=True)
os.makedirs(TABLE_DIR, exist_ok=True)

# -- Helpers -------------------------------------------------------------------

def normalize_country(s):
    """
    Transliterate accented characters to their ASCII base letters using NFD
    decomposition: é→e, ç→c, ã→a, ô→o, etc.  This ensures country names like
    'Côte d'Ivoire', 'México', 'São Tomé and Príncipe' match their UCDB
    counterparts stored without diacritics in GC_CNT_GAD_2025.
    """
    import unicodedata
    if not isinstance(s, str):
        return s
    return unicodedata.normalize("NFD", s).encode("ascii", "ignore").decode("ascii").strip()

# Keep old name as alias so any other callers still work
strip_non_ascii = normalize_country


from pipeline_utils import compute_bias, make_labels


# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

print("=" * 60)
print("E1  Buscar country analysis")
print("=" * 60)

print("\n[1/4] Loading cities_review.csv...")
df = pd.read_csv(CSV_PATH)
df["decision"]  = df["decision"].fillna("").str.strip()
df["score"]     = pd.to_numeric(df["score"], errors="coerce")
df["id"]        = pd.to_numeric(df["id"],    errors="coerce").astype("Int64")
df["score_pct_global"] = df["score"].rank(pct=True)

n_total    = len(df)
n_reviewed = df["decision"].ne("").sum()
n_keep     = (df["decision"] == "keep").sum()
n_fp       = (df["decision"].ne("") & df["decision"].ne("keep")).sum()
n_unrev    = df["decision"].eq("").sum()

print(f"  Total in queue  : {n_total:,}")
print(f"  Reviewed        : {n_reviewed:,}  ({100*n_reviewed/n_total:.1f}%)")
print(f"    keep          : {n_keep:,}")
print(f"    FP (drop+amb) : {n_fp:,}  ({100*n_fp/max(n_reviewed,1):.1f}% of reviewed)")
print(f"  Unreviewed      : {n_unrev:,}")

score_missing = df["score"].isna().sum()
if score_missing > 0:
    print(f"  WARNING: {score_missing} rows missing score -- will affect omega calculation")
else:
    print(f"  score column complete")

valid_decisions = {"keep", "drop", "ambiguous", ""}
bad_decisions = df[~df["decision"].isin(valid_decisions)]["decision"].unique()
if len(bad_decisions) > 0:
    print(f"  FAIL: Unexpected decision values: {bad_decisions.tolist()}")
    sys.exit(1)
print(f"  All decision values valid")

# -- Load UCDB for true N ------------------------------------------------------

print("\n[2/4] Loading UCDB for true country totals...")

with fiona.open(GPKG_PATH) as src:
    all_records = [
        {"id": f["properties"].get("ID_UC_G0"),
         "country_raw": f["properties"].get("GC_CNT_GAD_2025", "")}
        for f in src
    ]

ucdb = pd.DataFrame(all_records)
ucdb["id"]      = pd.to_numeric(ucdb["id"], errors="coerce").astype("Int64")
ucdb["country"] = ucdb["country_raw"].apply(strip_non_ascii)
ucdb_country_n  = ucdb.groupby("country")["id"].count().rename("n_ucdb_total")

print(f"  UCDB total cities : {len(ucdb):,}")
print(f"  Countries in UCDB : {ucdb['country'].nunique():,}")

csv_countries  = set(df["country"].dropna().unique())
ucdb_countries = set(ucdb["country"].unique())
unmatched = csv_countries - ucdb_countries
if unmatched:
    print(f"  WARNING: {len(unmatched)} CSV countries not found in UCDB (N falls back to queue size):")
    for c in sorted(unmatched)[:10]:
        print(f"       {c}")
else:
    print(f"  All {len(csv_countries)} CSV countries matched in UCDB")

# ==============================================================================
# 2. PER-COUNTRY ANALYSIS
# ==============================================================================

print("\n[3/4] Running per-country buscar analysis...")
print(f"  Recall target : {RECALL_TARGET*100:.0f}%")
print(f"  Confidence    : {CONFIDENCE*100:.0f}%  (p_stop = {P_STOP})")
print(f"  Min reviewed  : {MIN_REVIEWED}   Min FPs: {MIN_FPS}")
print(f"  omega method  : within-country score percentile odds ratio")
print()

results = []
n_stopped_biased    = 0
n_skipped           = 0
n_skip_group        = 0
n_insufficient      = 0

# All countries: union of queue countries + UCDB countries
all_countries = sorted(
    set(df["country"].dropna().unique()) | set(ucdb["country_raw"].dropna().unique())
)

for country in all_countries:
    cdf        = df[df["country"] == country].copy()
    c_reviewed = cdf[cdf["decision"] != ""]

    n_queue   = len(cdf)
    n_rev     = len(c_reviewed)
    n_fps_c   = (c_reviewed["decision"] != "keep").sum()
    n_kept_c  = (c_reviewed["decision"] == "keep").sum()

    # dev_group: from queue rows if available, else UCDB raw lookup
    if cdf["dev_group"].notna().any():
        dev_group = cdf["dev_group"].dropna().iloc[0]
    else:
        dev_group = "-"

    country_norm = normalize_country(country)
    n_ucdb_c = int(ucdb_country_n.get(country_norm, n_queue if n_queue > 0 else 0))

    if n_queue > 0 and n_ucdb_c < n_queue:
        print(f"  WARNING {country}: n_ucdb={n_ucdb_c} < n_queue={n_queue} -- using n_queue")
        n_ucdb_c = n_queue

    # Determine whether this country is in the stopping pipeline
    in_pipeline = dev_group not in SKIP_GROUPS

    has_enough = (n_rev >= MIN_REVIEWED) and (n_fps_c >= MIN_FPS)
    omega = p_cons = p_biased = None

    if not in_pipeline:
        # High income (or other skip groups): compute omega/p for information
        # but mark as not in pipeline and do NOT count toward stopped/skipped
        n_skip_group += 1
        if has_enough:
            labels   = make_labels(c_reviewed)
            omega    = compute_bias(c_reviewed)
            p_cons   = calculate_h0(labels, N=n_ucdb_c, recall_target=RECALL_TARGET, bias=1)
            p_biased = calculate_h0(labels, N=n_ucdb_c, recall_target=RECALL_TARGET, bias=omega)
            p_cons   = round(float(p_cons),   4) if p_cons   is not None else None
            p_biased = round(float(p_biased), 4) if p_biased is not None else None
        # skip console line — high-income countries are expected/intentional
    elif not has_enough:
        n_insufficient += 1
        if n_rev > 0 or n_queue > 0:
            # Only print countries that have some queue presence
            pass  # silent; summary counts cover this
    else:
        labels   = make_labels(c_reviewed)
        omega    = compute_bias(c_reviewed)
        p_cons   = calculate_h0(labels, N=n_ucdb_c, recall_target=RECALL_TARGET, bias=1)
        p_biased = calculate_h0(labels, N=n_ucdb_c, recall_target=RECALL_TARGET, bias=omega)
        p_cons   = round(float(p_cons),   4) if p_cons   is not None else None
        p_biased = round(float(p_biased), 4) if p_biased is not None else None

        stopped_b = p_biased is not None and p_biased <= P_STOP
        stopped_c = p_cons   is not None and p_cons   <= P_STOP
        if stopped_b:
            n_stopped_biased += 1

        sc = "STOP    " if stopped_c else "continue"
        sb = "STOP    " if stopped_b else "continue"
        print(f"  {country:<35} omega={omega:>5.1f}  "
              f"p_cons={str(p_cons)[:6]:>6} [{sc}]  "
              f"p_bias={str(p_biased)[:6]:>6} [{sb}]  "
              f"({n_fps_c} FPs / {n_rev} rev of {n_queue} queued / {n_ucdb_c} UCDB total)")

    # Stopping columns: only meaningful for in-pipeline countries with enough data
    if in_pipeline and has_enough:
        stopped_b = p_biased is not None and p_biased <= P_STOP
        stopped_c = p_cons   is not None and p_cons   <= P_STOP
        can_stop_b = stopped_b
        can_stop_c = stopped_c
        note_val   = ""
    elif not in_pipeline:
        can_stop_b = can_stop_c = False
        note_val   = f"not_in_pipeline ({dev_group})"
    else:
        can_stop_b = can_stop_c = False
        note_val   = f"insufficient_data (<{MIN_REVIEWED} reviewed or <{MIN_FPS} FPs)"
        n_skipped += 1

    results.append({
        "country":               country,
        "dev_group":             dev_group,
        "in_stopping_pipeline":  in_pipeline,
        "n_ucdb_total":          n_ucdb_c,
        "n_queue":               n_queue,
        "n_reviewed":            n_rev,
        "pct_reviewed_queue":    round(100 * n_rev / n_queue,   1) if n_queue   > 0 else None,
        "pct_reviewed_ucdb":     round(100 * n_rev / n_ucdb_c,  1) if n_ucdb_c  > 0 else None,
        "n_fps":                 n_fps_c,
        "n_kept":                n_kept_c,
        "omega":                 omega,
        # p-values: NA for skip-group countries (not in pipeline)
        "p_conservative":        p_cons   if in_pipeline else None,
        "p_biased":              p_biased if in_pipeline else None,
        "can_stop_conservative": can_stop_c if in_pipeline else None,
        "can_stop_biased":       can_stop_b if in_pipeline else None,
        "note":                  note_val,
    })

results_df = pd.DataFrame(results).sort_values(
    ["in_stopping_pipeline", "can_stop_biased", "p_biased", "country"],
    ascending=[False, False, True, True],
    na_position="last"
).reset_index(drop=True)

# -- Summary checks ------------------------------------------------------------

pipeline_df   = results_df[results_df["in_stopping_pipeline"] == True]
skipgrp_df    = results_df[results_df["in_stopping_pipeline"] == False]
n_with_data   = pipeline_df["omega"].notna().sum()
n_with_data_sg= skipgrp_df["omega"].notna().sum()

print()
print("-- E1 Summary checks -------------------------------------------------")
print(f"  Total countries in output  : {len(results_df):,}")
print(f"  ── In stopping pipeline ({', '.join(sorted(set(results_df['dev_group'].unique()) - SKIP_GROUPS - {'-'}))} etc.)")
print(f"     Countries              : {len(pipeline_df):,}")
print(f"     With sufficient data   : {n_with_data:,}")
print(f"     Stopped (biased urn)   : {n_stopped_biased:,}")
print(f"     Insufficient data      : {n_skipped:,}")
print(f"  ── Not in pipeline ({', '.join(sorted(SKIP_GROUPS))})")
print(f"     Countries              : {len(skipgrp_df):,}")
print(f"     With sufficient data   : {n_with_data_sg:,}  (stats computed, p-vals set to NA in CSV)")

# Checks apply only to in-pipeline countries
stopped_check = pipeline_df[pipeline_df["can_stop_biased"] == True]
bad_stop = stopped_check[stopped_check["p_biased"] > P_STOP]
if not bad_stop.empty:
    print(f"  FAIL: {len(bad_stop)} stopped countries have p_biased > {P_STOP}:")
    print(bad_stop[["country", "p_biased"]].to_string(index=False))
    sys.exit(1)
print(f"  OK  All {len(stopped_check)} stopped pipeline countries have p_biased <= {P_STOP}")

for col in ["p_conservative", "p_biased"]:
    bad = pipeline_df[pipeline_df[col].notna() &
                      ((pipeline_df[col] < 0) | (pipeline_df[col] > 1))]
    if not bad.empty:
        print(f"  FAIL: {col} out of [0,1] for: {bad['country'].tolist()}")
        sys.exit(1)
print(f"  OK  All pipeline p-values in [0, 1]")

bad_omega = results_df[results_df["omega"].notna() & (results_df["omega"] < 1)]
if not bad_omega.empty:
    print(f"  FAIL: omega < 1 for: {bad_omega['country'].tolist()}")
    sys.exit(1)
print(f"  OK  All omega >= 1")

omega_check = pipeline_df[
    pipeline_df["p_biased"].notna() & pipeline_df["p_conservative"].notna() &
    (pipeline_df["p_biased"] > pipeline_df["p_conservative"] + 0.01)
]
if not omega_check.empty:
    print(f"  WARNING: {len(omega_check)} pipeline countries where p_biased > p_conservative:")
    print(omega_check[["country", "omega", "p_conservative", "p_biased"]].to_string(index=False))
else:
    print(f"  OK  p_biased <= p_conservative for all pipeline countries")

results_df["_check_sum"] = results_df["n_fps"] + results_df["n_kept"]
bad_sum = results_df[
    results_df["n_reviewed"].notna() &
    (results_df["_check_sum"] != results_df["n_reviewed"])
]
if not bad_sum.empty:
    print(f"  FAIL: n_fps + n_kept != n_reviewed for: {bad_sum['country'].tolist()}")
    sys.exit(1)
print(f"  OK  n_fps + n_kept = n_reviewed for all countries")
results_df.drop(columns=["_check_sum"], inplace=True)

# omega direction check (all countries that have omega computed)
omega_sanity_fails = []
for country in results_df[results_df["omega"].notna()]["country"]:
    cdf   = df[df["country"] == country]
    c_rev = cdf[cdf["decision"] != ""]
    if c_rev.empty:
        continue
    pct   = c_rev["score"].rank(pct=True)
    fp_mean   = pct[c_rev["decision"] != "keep"].mean()
    kept_mean = pct[c_rev["decision"] == "keep"].mean()
    omega_val = results_df.loc[results_df["country"] == country, "omega"].values[0]
    if omega_val > 1.05 and fp_mean < kept_mean - 0.02:
        omega_sanity_fails.append(
            f"{country}: omega={omega_val} but FP mean rank ({fp_mean:.2f}) "
            f"< kept mean rank ({kept_mean:.2f})"
        )
if omega_sanity_fails:
    print(f"  FAIL: omega direction wrong:")
    for msg in omega_sanity_fails:
        print(f"     {msg}")
    sys.exit(1)
print(f"  OK  omega direction correct for all countries")

# -- Save ----------------------------------------------------------------------

results_df.to_csv(OUT_CSV, index=False, encoding="utf-8")
print(f"\n  Saved: {OUT_CSV}  ({len(results_df)} rows)")

# ==============================================================================
# 3. PLOTS
# ==============================================================================

print("\n[4/4] Generating plots...")

# ── Helper: group-order for consistent bar ordering ───────────────────────────
GROUP_ORDER = ["Low income", "Lower Middle", "Upper Middle", "High income", "-"]

def group_sort_key(g):
    return GROUP_ORDER.index(g) if g in GROUP_ORDER else len(GROUP_ORDER)


# ── Plot A+B: Review coverage and FP share by development group ───────────────
#
#   Panel A: Total cities in UCDB vs reviewed, stacked bars by dev group
#   Panel B: FP share of reviewed cities by dev group
#   Both panels mark High-income groups with hatching ("not in stopping pipeline")

cov_rows = []
for grp in GROUP_ORDER:
    grp_df = results_df[results_df["dev_group"] == grp]
    if grp_df.empty:
        continue
    n_ucdb_grp   = grp_df["n_ucdb_total"].sum()
    n_rev_grp    = grp_df["n_reviewed"].sum()
    n_fps_grp    = grp_df["n_fps"].sum()
    n_kept_grp   = grp_df["n_kept"].sum()
    in_pipeline  = grp not in SKIP_GROUPS
    cov_rows.append({
        "group":        grp,
        "n_ucdb":       int(n_ucdb_grp),
        "n_reviewed":   int(n_rev_grp),
        "n_unreviewed": int(n_ucdb_grp - n_rev_grp),
        "n_fps":        int(n_fps_grp),
        "n_kept":       int(n_kept_grp),
        "fp_share":     n_fps_grp / n_rev_grp if n_rev_grp > 0 else 0,
        "in_pipeline":  in_pipeline,
        "color":        GROUP_COLORS.get(grp, "#888"),
    })

cov_df = pd.DataFrame(cov_rows)

fig_cov, (ax_a, ax_b) = plt.subplots(
    1, 2, figsize=(13, 5),
    gridspec_kw={"width_ratios": [2.2, 1], "wspace": 0.32}
)

y_pos  = np.arange(len(cov_df))
height = 0.55

for i, row in cov_df.iterrows():
    hatch = "//" if not row["in_pipeline"] else None
    color = row["color"]

    # Reviewed bar (solid)
    ax_a.barh(i, row["n_reviewed"],  height=height,
              color=color, alpha=0.85, hatch=hatch, edgecolor="white",
              label=row["group"] if i == 0 else "_nolegend_")
    # Unreviewed extension (lighter)
    ax_a.barh(i, row["n_unreviewed"], height=height,
              left=row["n_reviewed"],
              color=color, alpha=0.25, hatch=hatch, edgecolor="white")

    # Annotation: reviewed count
    ax_a.text(row["n_reviewed"] + row["n_unreviewed"] * 0.02 + 30, i,
              f"{row['n_reviewed']:,} / {row['n_ucdb']:,}",
              va="center", fontsize=8, color="#333")

    # Panel B: FP share bar
    ax_b.barh(i, row["fp_share"] * 100, height=height,
              color=color, alpha=0.85, hatch=hatch, edgecolor="white")
    ax_b.text(row["fp_share"] * 100 + 0.4, i,
              f"{row['fp_share']*100:.1f}%  ({row['n_fps']:,} FP)",
              va="center", fontsize=8, color="#333")

ax_a.set_yticks(y_pos)
ax_a.set_yticklabels(cov_df["group"].tolist(), fontsize=9)
ax_a.set_xlabel("Cities", fontsize=9)
ax_a.set_title("Cities reviewed vs total UCDB\n(solid = reviewed, faded = unreviewed)", fontsize=9)
ax_a.grid(axis="x", alpha=0.2)

ax_b.set_yticks(y_pos)
ax_b.set_yticklabels(cov_df["group"].tolist(), fontsize=9)
ax_b.set_xlabel("False positive share of reviewed (%)", fontsize=9)
ax_b.set_title("FP share of reviewed cities\nby development group", fontsize=9)
ax_b.set_xlim(0, max(cov_df["fp_share"].max() * 130, 5))
ax_b.grid(axis="x", alpha=0.2)

# Shared legend
patch_handles = [
    Patch(facecolor=GROUP_COLORS[g], label=g,
          hatch="//" if g in SKIP_GROUPS else None)
    for g in GROUP_ORDER if g in GROUP_COLORS and g != "-"
]
patch_handles += [
    Patch(facecolor="#aaa", hatch="//", label="Not in stopping pipeline"),
    Patch(facecolor="#aaa", hatch=None,  label="In stopping pipeline"),
]
ax_a.legend(handles=patch_handles, fontsize=7.5, loc="lower right",
            title="Development group", title_fontsize=7.5)

plt.suptitle("Review coverage and false positive rates by development group",
             fontsize=11, fontweight="bold")
plt.tight_layout()

coverage_path = os.path.join(PLOT_DIR, "review_coverage_by_dev_group.png")
plt.savefig(coverage_path, dpi=150, bbox_inches="tight")
plt.close()
print(f"  Saved: {coverage_path}")


# ── Two-panel stopping overview: p-value (left) + omega (right) ───────────────
# Only include in-pipeline countries that have computed omega (sufficient data)

plot_df = (
    results_df[
        (results_df["in_stopping_pipeline"] == True) &
        results_df["omega"].notna()
    ]
    .sort_values("p_biased", na_position="last")
    .reset_index(drop=True)
)
n = len(plot_df)
fig_h = max(5, n * 0.38 + 1.5)

fig, (ax_p, ax_w) = plt.subplots(
    1, 2,
    figsize=(16, fig_h),
    sharey=True,
    gridspec_kw={"width_ratios": [2, 1], "wspace": 0.04}
)
y = np.arange(n)

for i, row in plot_df.iterrows():
    color = GROUP_COLORS.get(row["dev_group"], "#888888")
    ax_p.barh(i, row["p_biased"], height=0.65, color=color, alpha=0.85, align="center")
    ax_w.barh(i, row["omega"],    height=0.65, color=color, alpha=0.75, align="center")
    ax_p.text(
        min(row["p_biased"] + 0.01, 1.02), i,
        f"{int(row['n_fps'])} FP / {int(row['n_reviewed'])} rev",
        va="center", fontsize=6.5, color="#444"
    )

ax_p.axvline(P_STOP, color="black", linestyle="--", linewidth=1.3)
ax_w.axvline(1.0,    color="black", linestyle=":",  linewidth=0.9)

ax_p.set_yticks(y)
ax_p.set_yticklabels(
    [f"{r['country']}  ({r['dev_group']})" for _, r in plot_df.iterrows()],
    fontsize=8
)
ax_p.set_xlabel(
    "p-value  (biased urn)  <--  lower = more confident recall achieved",
    fontsize=9
)
ax_p.set_xlim(0, 1.35)
ax_p.grid(axis="x", alpha=0.2)
ax_p.set_title(
    f"Stopping criterion  |  {RECALL_TARGET*100:.0f}% recall  |  "
    f"{CONFIDENCE*100:.0f}% confidence\n"
    f"p={P_STOP} threshold (dashed)  |  {n_stopped_biased} countries stopped  "
    f"|  High-income countries excluded",
    fontsize=9
)

ax_w.set_xlabel("omega  (within-country odds ratio: scorer advantage for FPs)", fontsize=9)
ax_w.set_xlim(0, max(plot_df["omega"].max() * 1.2, 3))
ax_w.grid(axis="x", alpha=0.2)
ax_w.set_title(
    "Scorer bias per country\nomega > 1 = FPs concentrated at top of score ranking",
    fontsize=9
)

handles = [Patch(facecolor=c, label=g) for g, c in GROUP_COLORS.items()
           if g not in ("-", "High income")]
handles += [
    plt.Line2D([0], [0], color="black", linestyle="--", label=f"p={P_STOP} (stop)"),
    plt.Line2D([0], [0], color="black", linestyle=":",  label="omega=1 (no bias)")
]
ax_p.legend(handles=handles, fontsize=7.5, loc="lower right")

plt.suptitle("Per-country buscar analysis — stopping pipeline countries only",
             fontsize=11, fontweight="bold")
plt.tight_layout()

overview_path = os.path.join(PLOT_DIR, "overview_stopping_criteria.png")
plt.savefig(overview_path, dpi=150, bbox_inches="tight")
plt.close()
print(f"  Saved: {overview_path}")

# ── Overview stopping criteria — split by development group ───────────────────
# Same two-panel layout as above but one file per dev group so panels are
# legible even when a group has many countries.

overview_groups = [g for g in GROUP_ORDER
                   if g not in SKIP_GROUPS and g != "-"
                   and results_df[
                       (results_df["in_stopping_pipeline"] == True) &
                       (results_df["dev_group"] == g) &
                       results_df["omega"].notna()
                   ].shape[0] > 0]

for grp in overview_groups:
    grp_df = (
        results_df[
            (results_df["in_stopping_pipeline"] == True) &
            (results_df["dev_group"] == grp) &
            results_df["omega"].notna()
        ]
        .sort_values("p_biased", na_position="last")
        .reset_index(drop=True)
    )
    ng = len(grp_df)
    if ng == 0:
        continue

    fig_h_g = max(4, ng * 0.42 + 1.5)
    fig_g, (ax_pg, ax_wg) = plt.subplots(
        1, 2, figsize=(16, fig_h_g), sharey=True,
        gridspec_kw={"width_ratios": [2, 1], "wspace": 0.04}
    )
    yg = np.arange(ng)
    color_g = GROUP_COLORS.get(grp, "#888")
    n_stopped_g = (grp_df["can_stop_biased"] == True).sum()

    for i, row in grp_df.iterrows():
        ax_pg.barh(i, row["p_biased"], height=0.65, color=color_g, alpha=0.85)
        ax_wg.barh(i, row["omega"],    height=0.65, color=color_g, alpha=0.75)
        ax_pg.text(
            min(row["p_biased"] + 0.01, 1.02), i,
            f"{int(row['n_fps'])} FP / {int(row['n_reviewed'])} rev of {int(row['n_queue'])} q / {int(row['n_ucdb_total'])} UCDB",
            va="center", fontsize=6.5, color="#444"
        )

    ax_pg.axvline(P_STOP, color="black", linestyle="--", linewidth=1.3)
    ax_wg.axvline(1.0,    color="black", linestyle=":",  linewidth=0.9)
    ax_pg.set_yticks(yg)
    ax_pg.set_yticklabels(grp_df["country"].tolist(), fontsize=8)
    ax_pg.set_xlabel("p-value  (biased urn)  <--  lower = more confident", fontsize=9)
    ax_pg.set_xlim(0, 1.35)
    ax_pg.grid(axis="x", alpha=0.2)
    ax_pg.set_title(
        f"Stopping criterion  ·  {grp}\n"
        f"{RECALL_TARGET*100:.0f}% recall  ·  {CONFIDENCE*100:.0f}% confidence  ·  "
        f"p={P_STOP} threshold  ·  {n_stopped_g}/{ng} stopped",
        fontsize=9
    )
    ax_wg.set_xlabel("omega  (within-country odds ratio)", fontsize=9)
    ax_wg.set_xlim(0, max(grp_df["omega"].max() * 1.2, 3))
    ax_wg.grid(axis="x", alpha=0.2)
    ax_wg.set_title("Scorer bias\nomega > 1 = FPs at top of ranking", fontsize=9)

    plt.suptitle(f"Buscar analysis — {grp}", fontsize=11, fontweight="bold")
    plt.tight_layout()

    grp_slug = grp.lower().replace(" ", "_").replace("/", "_")
    grp_path = os.path.join(PLOT_DIR, f"overview_stopping_{grp_slug}.png")
    plt.savefig(grp_path, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"  Saved: {grp_path}")


# ── Recall frontier grid — countries with n_fps > 0 only ─────────────────────

valid = [r for r in results
         if r["omega"] is not None
         and r.get("in_stopping_pipeline", True)
         and r["n_fps"] > 0]
ncols = 4
nrows = -(-len(valid) // ncols)

fig, axes = plt.subplots(nrows, ncols,
                          figsize=(ncols * 3.5, nrows * 3),
                          constrained_layout=True)
axes_flat = axes.flatten() if hasattr(axes, "flatten") else [axes]

for idx, row in enumerate(valid):
    ax      = axes_flat[idx]
    country = row["country"]
    omega   = row["omega"]
    n_ucdb  = row["n_ucdb_total"]
    cdf     = df[df["country"] == country]
    labels  = make_labels(cdf[cdf["decision"] != ""])

    try:
        fc = recall_frontier(labels, N=n_ucdb, bias=1,     plot=False)
        fb = recall_frontier(labels, N=n_ucdb, bias=omega,  plot=False)
        ax.plot(fc["recall_target"], fc["p"], "-",
                color="#4f8eff", linewidth=1.2, alpha=0.7, label="omega=1")
        if omega > 1.01:
            ax.plot(fb["recall_target"], fb["p"], "-",
                    color="#ff6b35", linewidth=1.5, label=f"omega={omega:.1f}")
        ax.axhline(P_STOP, color="black", linestyle="--", linewidth=0.8)
        ax.set_ylim(0, 1)
        ax.set_xlim(0.7, 1.0)
        ax.grid(alpha=0.15)
        ax.legend(fontsize=6, loc="upper left")
    except Exception as e:
        ax.text(0.5, 0.5, str(e)[:60], ha="center", va="center",
                fontsize=6, transform=ax.transAxes, wrap=True)

    color = GROUP_COLORS.get(row["dev_group"], "#888")
    ax.set_title(
        f"{country}\n{row['dev_group']}  |  {row['n_fps']} FPs / {row['n_reviewed']} rev",
        fontsize=7, color=color, fontweight="bold"
    )
    ax.set_xlabel("Recall target", fontsize=6)
    ax.set_ylabel("p-value", fontsize=6)
    ax.tick_params(labelsize=6)

for idx in range(len(valid), len(axes_flat)):
    axes_flat[idx].set_visible(False)

fig.suptitle(
    f"Recall frontiers by country  |  {RECALL_TARGET*100:.0f}% recall target  |  "
    f"countries with ≥1 FP only\n"
    f"dashed = p={P_STOP}  |  blue = conservative (omega=1)  |  orange = biased urn",
    fontsize=10
)
frontier_path = os.path.join(PLOT_DIR, "recall_frontiers_by_country.png")
plt.savefig(frontier_path, dpi=150)
plt.close()
print(f"  Saved: {frontier_path}")

print("\n-- E1 complete -------------------------------------------------------")
print(f"  Output CSV  : {OUT_CSV}")
print(f"  Next step   : python E2_extrapolate_stopping.py")