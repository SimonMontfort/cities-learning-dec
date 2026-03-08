"""
stage4_export.py
----------------
Generate final outputs from completed review decisions:

  1. flagged_cities.csv / flagged_cities.gpkg  — excluded cities with metadata
  2. exclusion_report.md                        — methods-ready summary

Usage:
    python stage4_export.py \
        --csv data/cities_review.csv \
        --ucdb path/to/GHS_STAT_UCDB2015MT_GLOBE_R2019A.gpkg \
        --out-dir outputs/
"""

import argparse
import csv
from collections import Counter
from datetime import datetime
from pathlib import Path

import geopandas as gpd
import pandas as pd
import os

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")


def load_decisions(csv_path: str) -> pd.DataFrame:
    return pd.read_csv(csv_path)


def generate_report(df: pd.DataFrame, out_dir: Path) -> str:
    total = len(df)
    reviewed = df[df["decision"].notna() & (df["decision"] != "")].shape[0]
    excluded = df[df["decision"] == "exclude"]
    uncertain = df[df["decision"] == "uncertain"]
    kept = df[df["decision"] == "keep"]

    by_cat = Counter(excluded["category"].dropna())
    by_country = excluded.groupby("country").size().sort_values(ascending=False).head(20)

    # Score stats
    excl_scores = excluded["score"].describe()
    all_scores = df["score"].describe()

    lines = [
        "# UCDB Urban Centre Review — Exclusion Report",
        f"\n_Generated: {datetime.utcnow().strftime('%Y-%m-%d %H:%M UTC')}_\n",
        "## Overview\n",
        f"| Metric | Value |",
        f"|--------|-------|",
        f"| Cities in review queue | {total} |",
        f"| Cities reviewed | {reviewed} ({reviewed/total*100:.1f}%) |",
        f"| Excluded (false positives) | {len(excluded)} ({len(excluded)/reviewed*100:.1f}% of reviewed) |",
        f"| Uncertain (second pass) | {len(uncertain)} |",
        f"| Kept | {len(kept)} |",
        f"\n## Exclusion Categories\n",
        "| Category | Count | Share of excluded |",
        "|----------|-------|-------------------|",
    ]
    for cat, n in by_cat.most_common():
        lines.append(f"| {cat} | {n} | {n/len(excluded)*100:.1f}% |")

    lines += [
        "\n## Geographic Distribution (Top 20 countries by exclusions)\n",
        "| Country | Excluded cities |",
        "|---------|----------------|",
    ]
    for country, n in by_country.items():
        lines.append(f"| {country} | {n} |")

    lines += [
        "\n## Suspicion Score Distribution\n",
        "Score = 0.5 × (1 − NTL rank) + 0.5 × rural share rank. "
        "Higher = more suspicious.\n",
        "| Statistic | All reviewed | Excluded |",
        "|-----------|-------------|----------|",
        f"| Mean      | {df['score'].mean():.3f} | {excluded['score'].mean():.3f} |",
        f"| Median    | {df['score'].median():.3f} | {excluded['score'].median():.3f} |",
        f"| Min       | {df['score'].min():.3f} | {excluded['score'].min():.3f} |",
        f"| Max       | {df['score'].max():.3f} | {excluded['score'].max():.3f} |",
        "\n## Methodology Notes\n",
        "Urban centres were flagged as potential false positives based on two "
        "independent indicators:\n",
        "1. **Nighttime lights (NTL):** Mean radiance per polygon from NASA VIIRS "
        "Black Marble VNP46A4 annual composite (2020). Low NTL relative to the "
        "global UCDB distribution indicates absence of economic activity consistent "
        "with urban status.",
        "",
        "2. **Land cover (MODIS):** Urban pixel share per polygon from MODIS MCD12Q1 "
        "LC_Type1 (class 13, 2020). High rural share (low urban pixel fraction) "
        "relative to the global distribution indicates absence of built-up surface "
        "consistent with urban status.",
        "",
        "Both indicators were rank-normalised and combined into a composite suspicion "
        "score. The top 1,000 scoring urban centres were manually reviewed using a "
        "custom web interface that presented OpenStreetMap imagery, the UCDB polygon "
        "boundary, and the corresponding Wikipedia article for each candidate.",
        "",
        "Exclusion decisions were recorded in one of four prespecified categories "
        "(see table above) plus a free-text 'Other' category.",
    ]

    report = "\n".join(lines)
    report_path = out_dir / "exclusion_report.md"
    report_path.write_text(report, encoding="utf-8")
    return str(report_path)


def export(csv_path: str, ucdb_path: str | None, out_dir_str: str):
    out_dir = Path(out_dir_str)
    out_dir.mkdir(parents=True, exist_ok=True)

    df = load_decisions(csv_path)

    # Export flagged CSV
    excluded = df[df["decision"] == "exclude"].copy()
    uncertain = df[df["decision"] == "uncertain"].copy()
    flagged = pd.concat([excluded, uncertain])

    flagged_csv = out_dir / "flagged_cities.csv"
    flagged.to_csv(flagged_csv, index=False)
    print(f"✓ Flagged cities CSV: {flagged_csv} ({len(flagged)} rows)")

    # Export GeoPackage if UCDB available
    if ucdb_path:
        try:
            gdf = gpd.read_file(ucdb_path)
            id_col = "ID_HDC_G0" if "ID_HDC_G0" in gdf.columns else None
            if id_col and "ucdb_id" in flagged.columns:
                flagged_ids = set(flagged["ucdb_id"].astype(str))
                gdf_flagged = gdf[gdf[id_col].astype(str).isin(flagged_ids)].copy()
                # Merge decision columns
                gdf_flagged = gdf_flagged.merge(
                    flagged[["ucdb_id", "decision", "category", "note", "score"]],
                    left_on=id_col, right_on="ucdb_id", how="left"
                )
                gpkg_path = out_dir / "flagged_cities.gpkg"
                gdf_flagged.to_file(gpkg_path, driver="GPKG")
                print(f"✓ Flagged cities GeoPackage: {gpkg_path}")
        except Exception as e:
            print(f"  Could not write GeoPackage: {e}")

    # Generate report
    report_path = generate_report(df, out_dir)
    print(f"✓ Exclusion report: {report_path}")

    # Print summary
    print(f"\n── Summary ──────────────────────────")
    print(f"  Total reviewed:  {(df['decision'] != '').sum()}")
    print(f"  Excluded:        {(df['decision'] == 'exclude').sum()}")
    print(f"  Uncertain:       {(df['decision'] == 'uncertain').sum()}")
    print(f"  Kept:            {(df['decision'] == 'keep').sum()}")
    for cat, n in Counter(excluded["category"].dropna()).most_common():
        print(f"    [{cat}]: {n}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--csv", default="data/ghsl_appraisal/cities_review.csv")
    parser.add_argument("--ucdb", default=None, help="Path to UCDB GeoPackage (optional, for .gpkg export)")
    parser.add_argument("--out-dir", default="outputs/")
    args = parser.parse_args()
    export(args.csv, args.ucdb, args.out_dir)
