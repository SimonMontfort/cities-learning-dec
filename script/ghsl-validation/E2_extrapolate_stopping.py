"""
E2_extrapolate_stopping.py
===========================
For each country not yet stopped, reconstructs the full buscarpy p-value
trajectory by truncating the label sequence at each step and calling
calculate_h0. Fits a linear trend through the last WINDOW steps and
extrapolates to p=P_STOP to estimate how many more cities to review.

Pipeline position: SECOND — run after E1, before E3.

Why this is conservative:
  1. Trajectory is computed from the actual buscarpy sequential test —
     no approximation or simulation
  2. The last WINDOW cities are the lowest-scoring reviewed (cleanest
     tail of the queue) — new cities to add will be at least as clean
  3. Linear extrapolation undersells the steepening typical as the
     sequential test accumulates clean evidence
  4. Countries where slope ≥ 0 are flagged "indeterminate" — honest,
     not optimistic; E3 will use a fallback heuristic for these

Inputs:
  data/ghsl_appraisal/cities_review.csv
  data/ghsl_appraisal/country_stopping_summary.csv   ← from E1

Outputs:
  data/ghsl_appraisal/extrapolation_summary.csv       ← consumed by E3
  plots/buscar/extrapolation_to_stopping.png
  plots/buscar/sanity_stopped_trajectories.png

Run:
    python E2_extrapolate_stopping.py
"""

import sys
import os
import warnings
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import linregress
from buscarpy import calculate_h0

warnings.filterwarnings("ignore")

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from config import BASE_DIR

os.chdir(BASE_DIR)

# ── Config ─────────────────────────────────────────────────────────────────────

CSV_PATH    = "data/ghsl_appraisal/cities_review.csv"
SUMMARY_CSV = "data/ghsl_appraisal/country_stopping_summary.csv"
PLOT_DIR    = "plots/buscar"
TABLE_DIR   = "data/ghsl_appraisal"
OUT_CSV     = os.path.join(TABLE_DIR, "extrapolation_summary.csv")

RECALL_TARGET = 0.90   # ← must match E1 and E3
CONFIDENCE    = 0.90   # ← must match E1 and E3
P_STOP        = round(1 - CONFIDENCE, 10)   # 0.10

WINDOW     = 50    # last N reviewed cities used for linear fit
MIN_WINDOW = None  # set dynamically below as max(MIN_FPS * 2, 10)
MIN_FPS    = 3     # skip countries below this — trajectory is noisy

GROUP_COLORS = {
    "Low income":    "#e05c3a",
    "Lower Middle":  "#f0a030",
    "Upper Middle":  "#4f8eff",
    "High income":   "#a0a0c0",
    "-":             "#888888",
}

os.makedirs(PLOT_DIR, exist_ok=True)
os.makedirs(TABLE_DIR, exist_ok=True)

# MIN_WINDOW scales with MIN_FPS so countries like Sudan (5 FPs, 16 reviewed)
# aren't skipped just because the trajectory starts late.
MIN_WINDOW = max(MIN_FPS * 2, 10)

# ── Helpers ────────────────────────────────────────────────────────────────────

from pipeline_utils import p_trajectory, fit_linear_trend, extrapolate_to_stop

# Wrap fit_linear_trend with E2's configured defaults so call sites are unchanged
def _fit_trend(x, p_vals):
    return fit_linear_trend(x, p_vals, window=WINDOW, min_window=MIN_WINDOW)

def _extrapolate(slope, intercept, x_last):
    return extrapolate_to_stop(slope, intercept, x_last, p_stop=P_STOP)

def _trajectory(labels, n_ucdb, omega):
    return p_trajectory(labels, n_ucdb, omega,
                        recall_target=RECALL_TARGET, min_fps=MIN_FPS)


# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD + VALIDATE INPUTS
# ══════════════════════════════════════════════════════════════════════════════

print("=" * 60)
print("E2  Extrapolate stopping criteria")
print("=" * 60)

# CHECK: E1 output must exist and be fresh
if not os.path.exists(SUMMARY_CSV):
    print(f"✗  {SUMMARY_CSV} not found — run E1 first")
    sys.exit(1)

summary_mtime = os.path.getmtime(SUMMARY_CSV)
review_mtime  = os.path.getmtime(CSV_PATH)
if review_mtime > summary_mtime:
    print(f"⚠  WARNING: cities_review.csv is newer than country_stopping_summary.csv")
    print(f"   Consider re-running E1 before E2")

print(f"\n[1/3] Loading data…")
df = pd.read_csv(CSV_PATH)
df["decision"]  = df["decision"].fillna("").str.strip()
df["score"]     = pd.to_numeric(df["score"], errors="coerce")
df["score_pct"] = df["score"].rank(pct=True)

summary = pd.read_csv(SUMMARY_CSV)

# CHECK: required columns present in summary
required_cols = ["country", "dev_group", "n_ucdb_total", "n_reviewed",
                 "n_fps", "omega", "p_biased", "can_stop_biased"]
missing = [c for c in required_cols if c not in summary.columns]
if missing:
    print(f"✗  country_stopping_summary.csv missing columns: {missing}")
    sys.exit(1)
print(f"  ✓  Summary CSV columns OK")

# CHECK: RECALL_TARGET / CONFIDENCE in sync with E1
# (inferred from can_stop_biased vs p_biased)
stopped_rows = summary[summary["can_stop_biased"]]
not_stopped  = summary[
    summary["omega"].notna() &
    (summary["p_biased"] > P_STOP)
].copy()

print(f"  Countries with data        : {summary['omega'].notna().sum()}")
print(f"  Already stopped (biased)   : {len(stopped_rows)}")
print(f"  Not yet stopped            : {len(not_stopped)}")

if len(not_stopped) == 0:
    print("\n  All countries stopped — nothing to extrapolate. Done.")
    sys.exit(0)

# ══════════════════════════════════════════════════════════════════════════════
# 2. PER-COUNTRY TRAJECTORY + EXTRAPOLATION
# ══════════════════════════════════════════════════════════════════════════════

print(f"\n[2/3] Computing trajectories (window={WINDOW}, min_fps={MIN_FPS})…")
print(f"  {'Country':<35} {'p_now':>6}  {'slope':>8}  {'R²':>5}  {'n_more':>7}  status")
print(f"  {'-'*35} {'-'*6}  {'-'*8}  {'-'*5}  {'-'*7}  {'-'*15}")

extrap_results = []
country_data   = {}

for _, row in not_stopped.sort_values("p_biased", ascending=False).iterrows():
    country = row["country"]
    n_ucdb  = int(row["n_ucdb_total"])
    omega   = float(row["omega"])
    n_fps   = int(row["n_fps"])
    dev     = row["dev_group"]
    p_now   = float(row["p_biased"])

    cdf        = df[df["country"] == country]
    c_reviewed = cdf[cdf["decision"] != ""].sort_values("score", ascending=False)
    n_rev      = len(c_reviewed)

    # CHECK: reviewed count matches summary
    if abs(n_rev - int(row["n_reviewed"])) > 2:
        print(f"  ⚠  {country}: reviewed mismatch "
              f"(CSV={n_rev}, summary={row['n_reviewed']}) — summary may be stale")

    if n_rev < MIN_WINDOW:
        print(f"  {'  '+country:<35} — skipped: only {n_rev} reviewed (need ≥ {MIN_WINDOW})")
        extrap_results.append({
            "country": country, "dev_group": dev,
            "n_ucdb_total": n_ucdb, "n_reviewed": n_rev, "n_fps": n_fps,
            "omega": omega, "p_biased_now": p_now,
            "trend_slope": None, "trend_r2": None,
            "trend_window": WINDOW, "n_more_estimated": None,
            "status": "insufficient_data",
        })
        continue

    labels = (c_reviewed["decision"] != "keep").astype(int).values
    x      = np.arange(1, len(labels) + 1)

    ps = _trajectory(labels, n_ucdb, omega)

    # CHECK: final p should match E1's p_biased (within rounding)
    final_p = ps[~np.isnan(ps)][-1] if np.any(~np.isnan(ps)) else None
    if final_p is not None and abs(final_p - p_now) > 0.02:
        print(f"  ⚠  {country}: trajectory final p={final_p:.4f} "
              f"vs E1 p_biased={p_now:.4f} — check label construction")

    trend = _fit_trend(x, ps)

    if trend is None:
        slope = r_sq = n_more = None
        status = "insufficient_data"
        print(f"  {'  '+country:<35} {'p='+str(p_now)[:5]:>7}  "
              f"{'—':>8}  {'—':>5}  {'—':>7}  insufficient_data")
    else:
        slope, intercept, x_win, p_win, r_sq = trend
        x_stop, n_more = _extrapolate(slope, intercept, x[-1])

        if n_more is None:
            status = "indeterminate"
            print(f"  {'  '+country:<35} {p_now:>6.3f}  "
                  f"{slope:>+8.5f}  {r_sq:>5.2f}  {'—':>7}  ⚠ slope ≥ 0")
        else:
            status = "extrapolated"
            print(f"  {'  '+country:<35} {p_now:>6.3f}  "
                  f"{slope:>+8.5f}  {r_sq:>5.2f}  {n_more:>7}  extrapolated")

    extrap_results.append({
        "country":          country,
        "dev_group":        dev,
        "n_ucdb_total":     n_ucdb,
        "n_reviewed":       n_rev,
        "n_fps":            n_fps,
        "omega":            omega,
        "p_biased_now":     p_now,
        "trend_slope":      round(slope, 6) if slope is not None else None,
        "trend_r2":         r_sq,
        "trend_window":     WINDOW,
        "n_more_estimated": n_more,
        "status":           status,
    })

    country_data[country] = {
        "x": x, "ps": ps, "trend": trend, "dev": dev,
        "n_ucdb": n_ucdb, "omega": omega, "n_fps": n_fps,
        "n_reviewed": n_rev, "p_now": p_now,
    }

# ── Save ───────────────────────────────────────────────────────────────────────

extrap_df = pd.DataFrame(extrap_results).sort_values(
    ["status", "n_more_estimated"], ascending=[True, True], na_position="last"
)
extrap_df.to_csv(OUT_CSV, index=False)

# ── Summary checks ─────────────────────────────────────────────────────────────

n_extrap  = (extrap_df["status"] == "extrapolated").sum()
n_indet   = (extrap_df["status"] == "indeterminate").sum()
n_insuff  = (extrap_df["status"] == "insufficient_data").sum()

print()
print("── E2 Summary checks ─────────────────────────────────────────")
print(f"  Extrapolated (have estimate) : {n_extrap}")
print(f"  Indeterminate (slope ≥ 0)    : {n_indet}")
print(f"  Insufficient data            : {n_insuff}")

# CHECK: n_more should be non-negative
bad_nmore = extrap_df[
    extrap_df["n_more_estimated"].notna() & (extrap_df["n_more_estimated"] < 0)
]
if not bad_nmore.empty:
    print(f"  ✗  Negative n_more_estimated for: {bad_nmore['country'].tolist()}")
    sys.exit(1)
print(f"  ✓  All n_more_estimated ≥ 0")

# CHECK: n_more vs remaining UCDB cities — three tiers
extrap_df2 = extrap_df.merge(
    summary[["country", "n_queue"]], on="country", how="left"
)
extrap_df2["n_unreviewed"] = extrap_df2["n_ucdb_total"] - extrap_df2["n_reviewed"]

# Tier 1: n_more > unreviewed — impossible to satisfy with available cities
full_tail = extrap_df2[
    extrap_df2["n_more_estimated"].notna() &
    (extrap_df2["n_more_estimated"] > extrap_df2["n_unreviewed"])
]
# Tier 2: n_more > 2× unreviewed — very unlikely even with full tail
very_large = extrap_df2[
    extrap_df2["n_more_estimated"].notna() &
    (extrap_df2["n_more_estimated"] > extrap_df2["n_unreviewed"] * 2)
]

if not full_tail.empty:
    print(f"  ⚠  n_more_estimated EXCEEDS available unreviewed cities for:")
    for _, r in full_tail.iterrows():
        print(f"     {r['country']}: n_more={int(r['n_more_estimated'])}, "
              f"unreviewed={int(r['n_unreviewed'])}, total_ucdb={int(r['n_ucdb_total'])}")
        print(f"     → The linear trend is too shallow to reach p={P_STOP} within")
        print(f"       the remaining cities. However, reviewing the full UCDB tail")
        print(f"       WILL eventually reach p=0 (degenerate case: N=reviewed).")
        print(f"       This country needs more reviews than a single batch can provide.")
    # Write the flag into extrap_df so E3 can act on it
    full_tail_countries = set(full_tail["country"])
    extrap_df.loc[
        extrap_df["country"].isin(full_tail_countries), "status"
    ] = "full_tail_required"
    extrap_df.to_csv(OUT_CSV, index=False)   # re-save with updated status
    print(f"  ↳  Status set to 'full_tail_required' — E3 will add all remaining cities up to cap")
elif not very_large.empty:
    print(f"  ⚠  n_more_estimated > 2× unreviewed for:")
    for _, r in very_large.iterrows():
        print(f"     {r['country']}: n_more={int(r['n_more_estimated'])}, "
              f"unreviewed={int(r['n_unreviewed'])}")
else:
    print(f"  ✓  n_more_estimated within available unreviewed cities for all countries")

# CHECK: R² — warn if low (extrapolation unreliable)
low_r2 = extrap_df[
    (extrap_df["status"] == "extrapolated") &
    extrap_df["trend_r2"].notna() &
    (extrap_df["trend_r2"] < 0.3)
]
if not low_r2.empty:
    print(f"  ⚠  Low R² (< 0.3) — extrapolation noisy for:")
    for _, r in low_r2.iterrows():
        print(f"     {r['country']}: R²={r['trend_r2']:.2f}  "
              f"(add buffer to n_more_estimated)")
else:
    print(f"  ✓  R² ≥ 0.3 for all extrapolated countries")

# Print final table
print()
print("  Final estimates:")
print(f"  {'Country':<35} {'p_now':>6}  {'n_more':>7}  {'R²':>5}  status")
for _, r in extrap_df.sort_values("n_more_estimated", na_position="last").iterrows():
    nm = f"{int(r['n_more_estimated'])}" if pd.notna(r["n_more_estimated"]) else "—"
    r2 = f"{r['trend_r2']:.2f}"          if pd.notna(r["trend_r2"])         else "—"
    print(f"  {r['country']:<35} {r['p_biased_now']:>6.3f}  {nm:>7}  {r2:>5}  {r['status']}")

print(f"\n  Saved: {OUT_CSV}")

# ══════════════════════════════════════════════════════════════════════════════
# 3. PLOTS
# ══════════════════════════════════════════════════════════════════════════════

print("\n[3/3] Generating plots…")

# ── Main extrapolation plot ────────────────────────────────────────────────────

valid_countries = [c for c in country_data if country_data[c]["trend"] is not None]
ncols = 4
nrows = -(-len(valid_countries) // ncols)

fig, axes = plt.subplots(nrows, ncols,
                          figsize=(ncols * 3.8, nrows * 3.4),
                          constrained_layout=True)
axes_flat = axes.flatten() if nrows * ncols > 1 else [axes]

for idx, country in enumerate(valid_countries):
    ax    = axes_flat[idx]
    d     = country_data[country]
    x, ps = d["x"], d["ps"]
    color = GROUP_COLORS.get(d["dev"], "#888")
    slope, intercept, x_win, p_win, r_sq = d["trend"]

    # Actual trajectory
    mask = ~np.isnan(ps)
    ax.plot(x[mask], ps[mask], color=color, linewidth=1.5,
            alpha=0.8, zorder=3, label="Actual p (buscarpy)")

    # Scatter the window used for fitting
    ax.scatter(x_win, p_win, s=14, color=color, zorder=4,
               alpha=0.5, label=f"Fit window (last {len(x_win)})")

    x_last = x[-1]
    x_stop, n_more = _extrapolate(slope, intercept, x_last)

    if x_stop is not None:
        # Extend fit line from start of window to projected stop
        x_fit = np.array([x_win[0], x_stop])
        p_fit = np.clip(slope * x_fit + intercept, 0, 1)
        ax.plot(x_fit, p_fit, "--", color=color, linewidth=1.2,
                alpha=0.5, zorder=2)
        ax.axvline(x_stop, color=color, linewidth=0.8, linestyle=":", alpha=0.5)
        ax.text(x_stop, P_STOP + 0.05, f"+{n_more}",
                fontsize=7, color=color, ha="center", va="bottom", fontweight="bold")
        ax.axvspan(x_last, x_stop, alpha=0.07, color=color)
    else:
        x_fit = np.array([x_win[0], x_last * 1.3])
        ax.plot(x_fit, np.clip(slope * x_fit + intercept, 0, 1),
                "--", color="#aaa", linewidth=1.0, alpha=0.5)
        ax.text(0.97, 0.55, "⚠ slope ≥ 0\nindeterminate",
                transform=ax.transAxes, ha="right", va="top",
                fontsize=6.5, color="#999", style="italic")

    ax.axhline(P_STOP, color="black", linestyle="--", linewidth=0.9, zorder=1)
    ax.scatter([x_last], [d["p_now"]], color=color, s=50,
               zorder=6, edgecolors="white", linewidths=0.8)

    ax.set_ylim(-0.02, 1.02)
    ax.set_xlim(max(1, x_win[0] - 3),
                max(x_stop or x_last, x_last) * 1.15)
    ax.grid(alpha=0.12)
    ax.set_xlabel("Cities reviewed (cumulative)", fontsize=6.5)
    ax.set_ylabel("p-value (biased urn)", fontsize=6.5)
    ax.tick_params(labelsize=6)

    er = extrap_df[extrap_df["country"] == country].iloc[0]
    nm_label = (f"~{int(er['n_more_estimated'])} more"
                if pd.notna(er["n_more_estimated"]) else "indeterminate")
    ax.set_title(
        f"{country}\n"
        f"{d['dev']}  ·  ω={d['omega']:.1f}  ·  "
        f"{d['n_fps']} FPs / {d['n_reviewed']} rev / {d['n_ucdb']} total\n"
        f"p={d['p_now']:.3f}  →  {nm_label}  "
        f"(slope={slope:+.4f}, R²={r_sq:.2f})",
        fontsize=6.5, color=color, fontweight="bold"
    )

for idx in range(len(valid_countries), len(axes_flat)):
    axes_flat[idx].set_visible(False)

fig.suptitle(
    f"Extrapolation to stopping criterion  (p ≤ {P_STOP})  ·  "
    f"{RECALL_TARGET*100:.0f}% recall target\n"
    f"Solid line = actual buscarpy p  ·  Dots = fit window (last {WINDOW})  ·  "
    f"Dashed = linear extrapolation  ·  '+N' = cities still needed\n"
    f"Conservative: new cities will be at least as clean as the last {WINDOW} reviewed",
    fontsize=9
)
out_plot = os.path.join(PLOT_DIR, "extrapolation_to_stopping.png")
plt.savefig(out_plot, dpi=150)
plt.close()
print(f"  Saved: {out_plot}")

# ── Sanity check: stopped countries should reach p ≤ P_STOP ───────────────────

print(f"  Generating sanity check (stopped countries)…")

stopped_rows_df = summary[summary["can_stop_biased"]]
if len(stopped_rows_df) > 0:
    ncols2 = min(4, len(stopped_rows_df))
    nrows2 = -(-len(stopped_rows_df) // ncols2)
    fig2, axes2 = plt.subplots(nrows2, ncols2,
                                figsize=(ncols2 * 3.8, nrows2 * 3.2),
                                constrained_layout=True)
    axes2_flat = (axes2.flatten() if hasattr(axes2, "flatten") else [axes2])

    sanity_pass = True
    for idx, (_, row) in enumerate(stopped_rows_df.iterrows()):
        ax      = axes2_flat[idx]
        country = row["country"]
        n_ucdb  = int(row["n_ucdb_total"])
        omega   = float(row["omega"])
        dev     = row["dev_group"]
        color   = GROUP_COLORS.get(dev, "#888")

        cdf        = df[df["country"] == country]
        c_reviewed = cdf[cdf["decision"] != ""].sort_values("score", ascending=False)
        labels     = (c_reviewed["decision"] != "keep").astype(int).values
        x          = np.arange(1, len(labels) + 1)
        ps         = _trajectory(labels, n_ucdb, omega)

        # CHECK: trajectory should end at or below P_STOP
        valid_ps = ps[~np.isnan(ps)]
        if len(valid_ps) > 0 and valid_ps[-1] > P_STOP + 0.01:
            print(f"  ✗  SANITY FAIL: {country} marked stopped but "
                  f"trajectory ends at p={valid_ps[-1]:.4f} > {P_STOP}")
            sanity_pass = False

        # CHECK: trajectory should be generally decreasing
        if len(valid_ps) >= 5:
            first_half  = valid_ps[:len(valid_ps)//2].mean()
            second_half = valid_ps[len(valid_ps)//2:].mean()
            if second_half > first_half + 0.1:
                print(f"  ⚠  {country}: p-value rising over time "
                      f"(first half mean={first_half:.3f}, second={second_half:.3f})")

        mask = ~np.isnan(ps)
        ax.plot(x[mask], ps[mask], color=color, linewidth=1.8)
        ax.axhline(P_STOP, color="black", linestyle="--", linewidth=0.9)
        ax.set_ylim(-0.02, 1.02)
        ax.grid(alpha=0.12)
        ax.set_title(
            f"{country}  ✓ STOPPED\n"
            f"{dev}  ·  ω={omega:.1f}  ·  {row['n_fps']} FPs / {row['n_reviewed']} rev",
            fontsize=7.5, color=color, fontweight="bold"
        )
        ax.set_xlabel("Cities reviewed", fontsize=6.5)
        ax.set_ylabel("p-value", fontsize=6.5)
        ax.tick_params(labelsize=6)

    for idx in range(len(stopped_rows_df), len(axes2_flat)):
        axes2_flat[idx].set_visible(False)

    if sanity_pass:
        print(f"  ✓  All {len(stopped_rows_df)} stopped-country trajectories confirmed")
    
    fig2.suptitle(
        f"Sanity check — countries already stopped  (p ≤ {P_STOP})\n"
        "Trajectories should be generally decreasing and end below the dashed line",
        fontsize=9
    )
    out_sanity = os.path.join(PLOT_DIR, "sanity_stopped_trajectories.png")
    plt.savefig(out_sanity, dpi=150)
    plt.close()
    print(f"  Saved: {out_sanity}")
else:
    print("  No stopped countries yet — skipping sanity plot")

print("\n── E2 complete ───────────────────────────────────────────────")
print(f"  Output CSV  : {OUT_CSV}")
print(f"  Next step   : python E3_expand_queue.py")
