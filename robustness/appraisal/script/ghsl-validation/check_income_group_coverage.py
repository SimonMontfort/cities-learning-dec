"""
check_income_group_coverage.py
================================
Verifies that every non-high-income country in country_stopping_summary.csv
is accounted for in the stopping-criteria bar charts: either it appears in the
bar chart (has omega + p_biased) or it is listed in the exclusion note
(insufficient data / no false positives / population exhausted).

Also prints per-group country counts so you can cross-check against any
external reference list (e.g. World Bank income classification).

Run:
    python check_income_group_coverage.py

Exits non-zero if any country is unaccounted for.
"""

import sys
import os
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from config import BASE_DIR
os.chdir(BASE_DIR)

RESULTS_CSV = "data/ghsl_appraisal/country_stopping_summary.csv"
SKIP_GROUPS = {"High income"}
MIN_REVIEWED = 10
MIN_FPS      = 1

df = pd.read_csv(RESULTS_CSV)

# Normalise boolean columns that may read back as strings/floats
for col in ["in_stopping_pipeline", "can_stop_biased"]:
    if col in df.columns:
        df[col] = df[col].map(
            {True: True, False: False, "True": True, "False": False,
             1: True, 0: False, 1.0: True, 0.0: False}
        )

groups = [g for g in ["Low income", "Lower Middle", "Upper Middle", "High income", "-"]
          if g in df["dev_group"].unique()]

print("=" * 70)
print("Coverage check: non-high-income countries by income group")
print("=" * 70)

all_ok = True

for grp in groups:
    grp_df = df[df["dev_group"] == grp].copy()
    total  = len(grp_df)

    in_plot = grp_df[
        (grp_df["in_stopping_pipeline"] == True) &
        grp_df["omega"].notna() &
        grp_df["p_biased"].notna()
    ]

    excluded = grp_df[~grp_df.index.isin(in_plot.index)]

    accounted = len(in_plot) + len(excluded)
    ok        = accounted == total

    flag = "OK" if ok else "FAIL"
    skip = " (skipped — not in stopping pipeline)" if grp in SKIP_GROUPS else ""
    print(f"\n[{flag}]  {grp}{skip}")
    print(f"  Total countries   : {total}")
    print(f"  In bar chart      : {len(in_plot)}")
    print(f"  Excluded (noted)  : {len(excluded)}")
    print(f"  Accounted for     : {accounted} / {total}")

    if not ok:
        all_ok = False
        missing = grp_df[~grp_df.index.isin(in_plot.index) &
                         ~grp_df.index.isin(excluded.index)]
        print(f"  UNACCOUNTED ({len(missing)}):")
        for _, row in missing.iterrows():
            print(f"    {row['country']}")

    if len(excluded) > 0:
        print(f"  Excluded countries:")
        for _, row in excluded.sort_values("country").iterrows():
            fps   = int(row["n_fps"])      if pd.notna(row.get("n_fps"))      else "?"
            n_rev = int(row["n_reviewed"]) if pd.notna(row.get("n_reviewed")) else "?"
            if row.get("in_stopping_pipeline") == False:
                reason = "not in pipeline"
            elif pd.isna(row.get("omega")):
                reason = f"< {MIN_REVIEWED} reviewed"
            elif pd.isna(row.get("p_biased")):
                reason = "population exhausted (p=NaN)"
            else:
                reason = "insufficient data"
            print(f"    {row['country']:<40} {fps:>4} FP / {n_rev:>4} rev  —  {reason}")

print("\n" + "=" * 70)
if all_ok:
    print("All non-high-income countries accounted for.")
else:
    print("FAIL: some countries unaccounted for — see above.")
    sys.exit(1)
