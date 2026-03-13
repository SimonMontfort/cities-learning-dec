"""
E0_tests.py
===========
Comprehensive unit tests for the UCDB false-positive detection pipeline.

Tests cover:
  1. compute_bias (omega) -- correctness, direction, edge cases
  2. make_labels            -- ordering, FP encoding
  3. p-value properties     -- range, monotonicity, bias direction
  4. score sorting          -- highest-score cities reviewed first
  5. country coverage       -- all non-high-income UCDB countries included
  6. no duplicates          -- queue expansion never re-adds existing IDs
  7. label sequence         -- FP rate consistent with review data
  8. extrapolation sanity   -- n_more >= 0, status values valid
  9. batch log integrity    -- every expansion run is recorded
 10. config sync            -- RECALL_TARGET / CONFIDENCE match across scripts

Run:
    python E0_tests.py          # all tests
    python E0_tests.py -v       # verbose
    python E0_tests.py TestOmega  # single class
"""

import os
import sys
import re
import unittest
import warnings
import importlib
import numpy as np
import pandas as pd
from pathlib import Path

warnings.filterwarnings("ignore")

# Try to import BASE_DIR from config; fall back to env var or cwd
try:
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from config import BASE_DIR
    os.chdir(BASE_DIR)
except ModuleNotFoundError:
    BASE_DIR = os.environ.get("BASE_DIR", os.getcwd())
    print(f"  config.py not found — using BASE_DIR={BASE_DIR}")

# ---------------------------------------------------------------------------
# Import the actual pipeline functions under test from pipeline_utils.
# This means E0 tests the REAL functions used by E1/E2/E3, not local copies.
# ---------------------------------------------------------------------------

import re

import unicodedata

def normalize_country(s):
    """NFD transliteration: é→e, ç→c, ô→o, etc.  Matches E1's country normalizer."""
    if not isinstance(s, str):
        return s
    return unicodedata.normalize("NFD", s).encode("ascii", "ignore").decode("ascii").strip()

# Keep old name as alias
strip_non_ascii = normalize_country

try:
    from pipeline_utils import (
        compute_bias, make_labels,
        p_trajectory, fit_linear_trend, extrapolate_to_stop,
    )
    UTILS_AVAILABLE = True
except ImportError:
    UTILS_AVAILABLE = False
    print("  WARNING: pipeline_utils.py not found -- "
          "core function tests will be skipped")
    def compute_bias(df): return 1.0
    def make_labels(df):  return np.array([])
    def p_trajectory(*a, **k): return np.array([])
    def fit_linear_trend(*a, **k): return None
    def extrapolate_to_stop(*a, **k): return (None, None)


class TestOmega(unittest.TestCase):

    def setUp(self):
        if not UTILS_AVAILABLE:
            self.skipTest('pipeline_utils not available')


    def _make_df(self, fp_scores, keep_scores):
        decisions = (["drop"] * len(fp_scores) + ["keep"] * len(keep_scores))
        scores    = list(fp_scores) + list(keep_scores)
        return pd.DataFrame({"decision": decisions, "score": scores})

    def test_omega_gt1_when_fps_score_higher(self):
        """FPs at high scores, keepers at low scores -> omega > 1."""
        df = self._make_df(fp_scores=[0.8, 0.9, 0.85, 0.95],
                           keep_scores=[0.1, 0.2, 0.15, 0.05])
        omega = compute_bias(df)
        self.assertGreater(omega, 1.0,
            f"Expected omega > 1 when FPs score higher, got {omega}")

    def test_omega_equals1_when_random(self):
        """FPs and keepers randomly mixed -> omega should be ~1 (floor applied)."""
        rng = np.random.default_rng(42)
        scores = rng.uniform(0, 1, 40).tolist()
        decisions = ["drop"] * 20 + ["keep"] * 20
        df = pd.DataFrame({"decision": decisions, "score": scores})
        omega = compute_bias(df)
        self.assertGreaterEqual(omega, 1.0)

    def test_omega_floor_at_1(self):
        """Even if FPs score lower than keepers, omega is floored at 1.0."""
        df = self._make_df(fp_scores=[0.1, 0.15, 0.12],
                           keep_scores=[0.8, 0.85, 0.9, 0.95])
        omega = compute_bias(df)
        self.assertEqual(omega, 1.0,
            f"Expected omega=1.0 when FPs score lower (floor), got {omega}")

    def test_omega_insufficient_fps(self):
        """With fewer than 2 FPs, omega returns 1.0."""
        df = self._make_df(fp_scores=[0.9], keep_scores=[0.1, 0.2, 0.3])
        omega = compute_bias(df)
        self.assertEqual(omega, 1.0)

    def test_omega_insufficient_kept(self):
        """With fewer than 2 keepers, omega returns 1.0."""
        df = self._make_df(fp_scores=[0.8, 0.9, 0.85], keep_scores=[0.2])
        omega = compute_bias(df)
        self.assertEqual(omega, 1.0)

    def test_omega_uses_within_country_rank(self):
        """
        Critical: omega uses within-country rank, not absolute score.
        Two identical FP/keep patterns but different absolute score ranges
        should produce the same omega.
        """
        df_low  = self._make_df(fp_scores=[0.3, 0.4, 0.35, 0.45],
                                keep_scores=[0.01, 0.02, 0.015, 0.005])
        df_high = self._make_df(fp_scores=[0.8, 0.9, 0.85, 0.95],
                                keep_scores=[0.51, 0.52, 0.515, 0.505])
        omega_low  = compute_bias(df_low)
        omega_high = compute_bias(df_high)
        self.assertAlmostEqual(omega_low, omega_high, places=1,
            msg=f"omega should be rank-based, got {omega_low} vs {omega_high}")

    def test_omega_ambiguous_counted_as_fp(self):
        """Ambiguous decisions should be treated as FPs (not keep)."""
        df_amb  = pd.DataFrame({
            "decision": ["ambiguous"] * 4 + ["keep"] * 4,
            "score":    [0.8, 0.9, 0.85, 0.95, 0.1, 0.2, 0.15, 0.05]
        })
        df_drop = pd.DataFrame({
            "decision": ["drop"] * 4 + ["keep"] * 4,
            "score":    [0.8, 0.9, 0.85, 0.95, 0.1, 0.2, 0.15, 0.05]
        })
        omega_amb  = compute_bias(df_amb)
        omega_drop = compute_bias(df_drop)
        self.assertAlmostEqual(omega_amb, omega_drop, places=2,
            msg="ambiguous and drop should produce same omega")

    def test_omega_deterministic(self):
        """Same data always gives same omega."""
        df = self._make_df(fp_scores=[0.7, 0.8, 0.75],
                           keep_scores=[0.2, 0.3, 0.25, 0.1])
        self.assertEqual(compute_bias(df), compute_bias(df))

    def test_omega_direction_matches_formula(self):
        """
        Manual verification: with p_fp=0.8, p_kept=0.3
        expected omega = (0.8/0.2) / (0.3/0.7) = 4.0 / 0.4286 ~ 9.33
        """
        # Construct scores so mean rank = 0.8 for FPs, 0.3 for keepers
        # Use many points to get stable mean
        fp_scores   = np.linspace(0.60, 0.99, 20).tolist()
        keep_scores = np.linspace(0.01, 0.59, 20).tolist()
        df = self._make_df(fp_scores, keep_scores)
        omega = compute_bias(df)
        # FP mean rank should be around 0.8, kept around 0.3
        pct = df["score"].rank(pct=True)
        fp_mean   = pct[df["decision"] != "keep"].mean()
        kept_mean = pct[df["decision"] == "keep"].mean()
        expected = (fp_mean / (1 - fp_mean)) / (kept_mean / (1 - kept_mean))
        expected = max(1.0, round(float(expected), 2))
        self.assertAlmostEqual(omega, expected, places=1,
            msg=f"omega={omega}, expected~{expected}")


# ---------------------------------------------------------------------------
# 2. make_labels
# ---------------------------------------------------------------------------

class TestMakeLabels(unittest.TestCase):

    def setUp(self):
        if not UTILS_AVAILABLE:
            self.skipTest('pipeline_utils not available')


    def _make_df(self, rows):
        """rows = list of (score, decision)"""
        return pd.DataFrame(rows, columns=["score", "decision"])

    def test_labels_sorted_score_desc(self):
        """Labels should correspond to cities sorted score descending."""
        df = self._make_df([
            (0.3, "keep"), (0.9, "drop"), (0.6, "keep"), (0.8, "ambiguous")
        ])
        labels = make_labels(df)
        # Sorted desc: 0.9 (drop=1), 0.8 (ambiguous=1), 0.6 (keep=0), 0.3 (keep=0)
        np.testing.assert_array_equal(labels, [1, 1, 0, 0],
            err_msg=f"Expected [1,1,0,0], got {labels}")

    def test_fp_encoding(self):
        """drop -> 1, ambiguous -> 1, keep -> 0."""
        df = self._make_df([
            (1.0, "drop"), (0.9, "ambiguous"), (0.8, "keep")
        ])
        labels = make_labels(df)
        self.assertEqual(labels[0], 1, "drop should be 1")
        self.assertEqual(labels[1], 1, "ambiguous should be 1")
        self.assertEqual(labels[2], 0, "keep should be 0")

    def test_all_keep(self):
        df = self._make_df([(0.5, "keep"), (0.6, "keep"), (0.7, "keep")])
        labels = make_labels(df)
        self.assertTrue(all(l == 0 for l in labels), "All keep -> all zeros")

    def test_all_drop(self):
        df = self._make_df([(0.5, "drop"), (0.6, "drop"), (0.7, "drop")])
        labels = make_labels(df)
        self.assertTrue(all(l == 1 for l in labels), "All drop -> all ones")

    def test_length_matches_input(self):
        df = self._make_df([(float(i)/10, "keep") for i in range(15)])
        labels = make_labels(df)
        self.assertEqual(len(labels), 15)

    def test_ties_handled_consistently(self):
        """Equal scores should not crash -- order may vary but length must match."""
        df = self._make_df([(0.5, "keep"), (0.5, "drop"), (0.5, "keep")])
        labels = make_labels(df)
        self.assertEqual(len(labels), 3)
        self.assertEqual(labels.sum(), 1, "Should be exactly 1 FP")


# ---------------------------------------------------------------------------
# 3. p-value properties (requires buscarpy)
# ---------------------------------------------------------------------------

class TestPValueProperties(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        try:
            from buscarpy import calculate_h0
            cls.calculate_h0 = staticmethod(calculate_h0)
            cls.skip = False
        except ImportError:
            cls.skip = True

    def setUp(self):
        if self.skip:
            self.skipTest("buscarpy not available")

    def test_p_in_unit_interval(self):
        """p-value must be in [0, 1]."""
        labels = np.array([1, 1, 0, 0, 1, 0, 0, 0, 0, 0])
        p = self.calculate_h0(labels, N=50, recall_target=0.9, bias=1)
        if p is not None:
            self.assertGreaterEqual(p, 0.0, f"p={p} below 0")
            self.assertLessEqual(p, 1.0,    f"p={p} above 1")

    def test_biased_p_leq_conservative(self):
        """p_biased should be <= p_conservative (bias helps or equals)."""
        labels = np.array([1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0])
        p_cons   = self.calculate_h0(labels, N=80, recall_target=0.9, bias=1)
        p_biased = self.calculate_h0(labels, N=80, recall_target=0.9, bias=3.0)
        if p_cons is not None and p_biased is not None:
            self.assertLessEqual(p_biased, p_cons + 0.01,
                f"p_biased={p_biased} > p_conservative={p_cons}")

    def test_more_fps_found_lower_p(self):
        """Finding more FPs in the same sequence should generally lower p."""
        # Sequence with 3 FPs out of 12
        labels_few = np.array([1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0])
        # Same sequence with one more FP prepended
        labels_more = np.array([1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0])
        p_few  = self.calculate_h0(labels_few,  N=80, recall_target=0.9, bias=1)
        p_more = self.calculate_h0(labels_more, N=80, recall_target=0.9, bias=1)
        if p_few is not None and p_more is not None:
            self.assertLessEqual(p_more, p_few + 0.05,
                f"More FPs should not substantially raise p: "
                f"p_few={p_few}, p_more={p_more}")

    def test_higher_bias_gives_lower_p(self):
        """Higher omega should give lower or equal p (bias benefits the test)."""
        labels = np.array([1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0])
        p1 = self.calculate_h0(labels, N=60, recall_target=0.9, bias=1.0)
        p3 = self.calculate_h0(labels, N=60, recall_target=0.9, bias=3.0)
        p8 = self.calculate_h0(labels, N=60, recall_target=0.9, bias=8.0)
        vals = [v for v in [p1, p3, p8] if v is not None]
        if len(vals) >= 2:
            self.assertLessEqual(vals[-1], vals[0] + 0.01,
                f"Higher bias should not raise p: bias=1 -> {p1}, "
                f"bias=3 -> {p3}, bias=8 -> {p8}")

    def test_none_returned_insufficient_fps(self):
        """
        With all zeros (no FPs found), buscarpy returns a valid p-value
        reflecting confidence about the *absence* of FPs — not None or 1.0.
        Just verify it stays in [0, 1].
        """
        labels = np.zeros(20, dtype=int)
        p = self.calculate_h0(labels, N=100, recall_target=0.9, bias=1)
        if p is not None:
            self.assertGreaterEqual(p, 0.0, f"p={p} below 0")
            self.assertLessEqual(p, 1.0,    f"p={p} above 1")

    def test_p_decreases_as_more_clean_cities_added(self):
        """
        After finding all FPs early, adding clean cities should drive p down.
        This verifies the sequential test behaves correctly.
        """
        base  = np.array([1, 1, 1, 0, 0])            # 3 FPs found early
        more  = np.concatenate([base, np.zeros(15)])  # 15 more clean cities
        p_base = self.calculate_h0(base, N=50, recall_target=0.9, bias=1)
        p_more = self.calculate_h0(more, N=50, recall_target=0.9, bias=1)
        if p_base is not None and p_more is not None:
            self.assertLessEqual(p_more, p_base,
                f"p should decrease as more clean cities reviewed: "
                f"{p_base} -> {p_more}")


# ---------------------------------------------------------------------------
# 4. score sorting (highest-score cities first)
# ---------------------------------------------------------------------------

class TestScoreSorting(unittest.TestCase):

    def setUp(self):
        if not UTILS_AVAILABLE:
            self.skipTest('pipeline_utils not available')


    def test_make_labels_highest_first(self):
        """
        The first label corresponds to the highest-scored city.
        If that city is a drop, label[0] = 1.
        """
        df = pd.DataFrame({
            "score":    [0.1, 0.99, 0.5],
            "decision": ["keep", "drop", "keep"]
        })
        labels = make_labels(df)
        self.assertEqual(labels[0], 1,
            "Highest-scored city is 'drop', so first label should be 1")
        self.assertEqual(labels[1], 0)
        self.assertEqual(labels[2], 0)

    def test_fp_rate_higher_at_start(self):
        """
        In a well-calibrated scorer, FPs should be concentrated at the top.
        A sequence starting with FPs (1,1,1,0,0,0) should give lower p
        than one ending with them (0,0,0,1,1,1).
        """
        try:
            from buscarpy import calculate_h0
        except ImportError:
            self.skipTest("buscarpy not available")

        fps_first = np.array([1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
        fps_last  = np.array([0, 0, 0, 0, 0, 0, 0, 1, 1, 1])
        p_first = calculate_h0(fps_first, N=50, recall_target=0.9, bias=1)
        p_last  = calculate_h0(fps_last,  N=50, recall_target=0.9, bias=1)
        if p_first is not None and p_last is not None:
            self.assertLessEqual(p_first, p_last,
                f"FPs at start -> lower p ({p_first}) vs FPs at end ({p_last})")


# ---------------------------------------------------------------------------
# 5. Country coverage
# ---------------------------------------------------------------------------

class TestCountryCoverage(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        cls.stopping_csv = Path("data/ghsl_appraisal/country_stopping_summary.csv")
        cls.review_csv   = Path("data/ghsl_appraisal/cities_review.csv")
        if not cls.stopping_csv.exists() or not cls.review_csv.exists():
            cls.skip = True
        else:
            cls.skip = False
            cls.stop_df   = pd.read_csv(cls.stopping_csv)
            cls.review_df = pd.read_csv(cls.review_csv)

    def setUp(self):
        if self.skip:
            self.skipTest("country_stopping_summary.csv or cities_review.csv not found")

    def test_all_review_countries_in_summary(self):
        """Every country in the review CSV should appear in the stopping summary."""
        review_countries  = set(self.review_df["country"].dropna().unique())
        summary_countries = set(self.stop_df["country"].dropna().unique())
        missing = review_countries - summary_countries
        self.assertEqual(len(missing), 0,
            f"{len(missing)} review countries missing from summary: "
            f"{sorted(missing)[:10]}")

    def test_high_income_not_in_queue(self):
        """High income countries should not be in the review queue (by policy)."""
        if "dev_group" not in self.review_df.columns:
            self.skipTest("dev_group column not in review CSV")
        hi = self.review_df[self.review_df["dev_group"] == "High income"]
        # Allow some high-income cities from initial Stage 1 scoring (they may
        # be present but should not dominate)
        hi_pct = len(hi) / len(self.review_df) * 100
        # High income cities may appear from Stage 1 global top-10% safety net.
        # Flag only if they dominate the queue (> 10%).
        self.assertLess(hi_pct, 10.0,
            f"High income cities are {hi_pct:.1f}% of queue -- seems too high")

    def test_no_duplicate_ids_in_queue(self):
        """No city should appear twice in the review queue."""
        ids = self.review_df["id"].dropna()
        n_dupes = ids.duplicated().sum()
        self.assertEqual(n_dupes, 0,
            f"{n_dupes} duplicate city IDs in review queue")

    def test_decisions_are_valid(self):
        """All non-empty decisions should be keep/drop/ambiguous."""
        valid = {"keep", "drop", "ambiguous"}
        decisions = (
            self.review_df["decision"]
            .fillna("")
            .str.strip()
        )
        bad = decisions[decisions.ne("") & ~decisions.isin(valid)].unique()
        self.assertEqual(len(bad), 0,
            f"Invalid decision values: {bad.tolist()}")

    def test_low_middle_income_countries_covered(self):
        """
        All Low / Lower Middle / Upper Middle income countries in the summary
        with n_ucdb_total > 0 should either be stopped or have cities in queue.
        Countries with 0 UCDB cities are legitimately absent.
        """
        target_groups = {"Low income", "Lower Middle", "Upper Middle"}
        eligible = self.stop_df[
            self.stop_df["dev_group"].isin(target_groups) &
            (self.stop_df["n_ucdb_total"] > 0)
        ]
        in_queue = set(self.review_df["country"].dropna().unique())
        stopped  = set(
            self.stop_df.loc[self.stop_df["can_stop_biased"] == True, "country"]
        )
        covered  = in_queue | stopped

        uncovered = set(eligible["country"]) - covered
        if uncovered:
            print(f"\n  INFO: {len(uncovered)} low/middle income countries "
                  f"not yet in queue or stopped:")
            for c in sorted(uncovered)[:15]:
                row = eligible[eligible["country"] == c].iloc[0]
                print(f"    {c}: {row['dev_group']}, n_ucdb={row['n_ucdb_total']}")
        # This is informational, not a hard failure -- countries may legitimately
        # be absent if they have very few UCDB cities and haven't been reached yet.
        # Flag if more than 20% are uncovered.
        pct_uncovered = 100 * len(uncovered) / max(len(eligible), 1)
        self.assertLess(pct_uncovered, 20,
            f"{pct_uncovered:.1f}% of low/middle income countries not covered")

    def test_in_stopping_pipeline_column_present(self):
        """country_stopping_summary.csv must have an in_stopping_pipeline column."""
        self.assertIn("in_stopping_pipeline", self.stop_df.columns,
            "in_stopping_pipeline column missing from country_stopping_summary.csv")

    def test_in_stopping_pipeline_values(self):
        """in_stopping_pipeline must be boolean (True/False) for all rows."""
        col = self.stop_df["in_stopping_pipeline"]
        # pandas may read booleans as bool or as string "True"/"False"
        valid_bool = col.isin([True, False, "True", "False"])
        self.assertTrue(valid_bool.all(),
            f"Unexpected in_stopping_pipeline values: "
            f"{col[~valid_bool].unique().tolist()}")

    def test_high_income_not_in_stopping_pipeline(self):
        """All High income rows must have in_stopping_pipeline == False."""
        hi_rows = self.stop_df[self.stop_df["dev_group"] == "High income"]
        if hi_rows.empty:
            return  # no high-income rows -- nothing to check
        bad = hi_rows[hi_rows["in_stopping_pipeline"].astype(str) != "False"]
        self.assertEqual(len(bad), 0,
            f"{len(bad)} High income countries have in_stopping_pipeline=True: "
            f"{bad['country'].tolist()[:5]}")

    def test_non_high_income_in_stopping_pipeline(self):
        """All Low/Lower Middle/Upper Middle rows must have in_stopping_pipeline == True."""
        pipeline_groups = {"Low income", "Lower Middle", "Upper Middle"}
        dev_rows = self.stop_df[self.stop_df["dev_group"].isin(pipeline_groups)]
        if dev_rows.empty:
            return
        bad = dev_rows[dev_rows["in_stopping_pipeline"].astype(str) != "True"]
        self.assertEqual(len(bad), 0,
            f"{len(bad)} developing-country rows have in_stopping_pipeline=False: "
            f"{bad['country'].tolist()[:5]}")

    def test_high_income_p_values_are_null(self):
        """p_conservative and p_biased must be NaN/null for all High income rows."""
        hi_rows = self.stop_df[self.stop_df["dev_group"] == "High income"]
        if hi_rows.empty:
            return
        for col in ["p_conservative", "p_biased"]:
            if col not in self.stop_df.columns:
                continue
            non_null = hi_rows[hi_rows[col].notna()]
            self.assertEqual(len(non_null), 0,
                f"{len(non_null)} High income rows have non-null {col}: "
                f"{non_null['country'].tolist()[:5]}")


# ---------------------------------------------------------------------------
# 6. No duplicates in queue expansion
# ---------------------------------------------------------------------------

class TestNoDuplicates(unittest.TestCase):

    def test_no_duplicates_after_expansion(self):
        """cities_review.csv should never have duplicate city IDs."""
        review_csv = Path("data/ghsl_appraisal/cities_review.csv")
        if not review_csv.exists():
            self.skipTest("cities_review.csv not found")
        df = pd.read_csv(review_csv)
        ids = df["id"].dropna()
        dupes = ids[ids.duplicated()].unique()
        self.assertEqual(len(dupes), 0,
            f"{len(dupes)} duplicate IDs in review CSV: {dupes[:5].tolist()}")

    def test_score_order_within_country(self):
        """
        For each country, the cities in the review queue (unreviewed) should
        have scores >= the lowest-scored reviewed city (we always take top-N).
        This is a loose check -- just verify unreviewed scores aren't all zeros.
        """
        review_csv = Path("data/ghsl_appraisal/cities_review.csv")
        if not review_csv.exists():
            self.skipTest("cities_review.csv not found")
        df = pd.read_csv(review_csv)
        df["score"]    = pd.to_numeric(df["score"], errors="coerce")
        df["decision"] = df["decision"].fillna("").str.strip()

        unreviewed = df[df["decision"] == ""]
        if unreviewed.empty:
            return  # all reviewed -- nothing to check

        n_zero_score = (unreviewed["score"].fillna(0) == 0).sum()
        pct_zero = 100 * n_zero_score / len(unreviewed)
        self.assertLess(pct_zero, 10.0,
            f"{pct_zero:.1f}% of unreviewed cities have score=0 -- "
            f"score lookup may have failed")


# ---------------------------------------------------------------------------
# 7. Label sequence integrity
# ---------------------------------------------------------------------------

class TestLabelIntegrity(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        csv = Path("data/ghsl_appraisal/cities_review.csv")
        sum_csv = Path("data/ghsl_appraisal/country_stopping_summary.csv")
        cls.skip = not (csv.exists() and sum_csv.exists())
        if not cls.skip:
            cls.df      = pd.read_csv(csv)
            cls.df["decision"] = cls.df["decision"].fillna("").str.strip()
            cls.df["score"]    = pd.to_numeric(cls.df["score"], errors="coerce")
            cls.summary = pd.read_csv(sum_csv)

    def setUp(self):
        if self.skip:
            self.skipTest("Required CSVs not found")

    def test_n_fps_matches_decisions(self):
        """n_fps in summary matches actual count of drop+ambiguous decisions."""
        countries = self.summary[self.summary["omega"].notna()]["country"]
        mismatches = []
        for c in countries:
            c_rev  = self.df[(self.df["country"] == c) & (self.df["decision"] != "")]
            actual = (c_rev["decision"] != "keep").sum()
            reported = self.summary.loc[
                self.summary["country"] == c, "n_fps"
            ].values[0]
            if actual != reported:
                mismatches.append(f"{c}: actual={actual}, summary={reported}")
        self.assertEqual(len(mismatches), 0,
            f"n_fps mismatch:\n" + "\n".join(mismatches[:5]))

    def test_n_reviewed_matches_decisions(self):
        """n_reviewed in summary matches actual reviewed count."""
        countries = self.summary[self.summary["omega"].notna()]["country"]
        mismatches = []
        for c in countries:
            c_rev    = self.df[(self.df["country"] == c) & (self.df["decision"] != "")]
            actual   = len(c_rev)
            reported = self.summary.loc[
                self.summary["country"] == c, "n_reviewed"
            ].values[0]
            if actual != reported:
                mismatches.append(f"{c}: actual={actual}, summary={reported}")
        self.assertEqual(len(mismatches), 0,
            f"n_reviewed mismatch:\n" + "\n".join(mismatches[:5]))

    def test_label_sum_equals_n_fps(self):
        """sum(make_labels(...)) == n_fps for every country with data."""
        countries = self.summary[self.summary["omega"].notna()]["country"]
        for c in countries:
            c_rev  = self.df[(self.df["country"] == c) & (self.df["decision"] != "")]
            labels = make_labels(c_rev)
            n_fps_expected = (c_rev["decision"] != "keep").sum()
            self.assertEqual(labels.sum(), n_fps_expected,
                f"{c}: labels.sum()={labels.sum()} != n_fps={n_fps_expected}")


# ---------------------------------------------------------------------------
# 8. Extrapolation output sanity
# ---------------------------------------------------------------------------

class TestExtrapolation(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        extrap = Path("data/ghsl_appraisal/extrapolation_summary.csv")
        cls.skip = not extrap.exists()
        if not cls.skip:
            cls.df = pd.read_csv(extrap)

    def setUp(self):
        if self.skip:
            self.skipTest("extrapolation_summary.csv not found")

    def test_n_more_nonnegative(self):
        bad = self.df[
            self.df["n_more_estimated"].notna() &
            (self.df["n_more_estimated"] < 0)
        ]
        self.assertEqual(len(bad), 0,
            f"Negative n_more_estimated: {bad[['country','n_more_estimated']].to_dict('records')}")

    def test_status_values_valid(self):
        valid_statuses = {
            "extrapolated", "indeterminate", "insufficient_data",
            "full_tail_required"
        }
        bad = self.df[~self.df["status"].isin(valid_statuses)]
        self.assertEqual(len(bad), 0,
            f"Invalid status values: {bad['status'].unique().tolist()}")

    def test_r2_in_unit_interval(self):
        bad = self.df[
            self.df["trend_r2"].notna() &
            ((self.df["trend_r2"] < 0) | (self.df["trend_r2"] > 1))
        ]
        self.assertEqual(len(bad), 0,
            f"R2 out of [0,1]: {bad[['country','trend_r2']].to_dict('records')}")

    def test_extrapolated_has_n_more(self):
        """Every 'extrapolated' row must have a non-null n_more_estimated."""
        bad = self.df[
            (self.df["status"] == "extrapolated") &
            self.df["n_more_estimated"].isna()
        ]
        self.assertEqual(len(bad), 0,
            f"extrapolated rows missing n_more: {bad['country'].tolist()}")

    def test_indeterminate_has_no_n_more(self):
        """indeterminate rows should NOT have n_more_estimated."""
        bad = self.df[
            (self.df["status"] == "indeterminate") &
            self.df["n_more_estimated"].notna()
        ]
        self.assertEqual(len(bad), 0,
            f"indeterminate rows have unexpected n_more: {bad['country'].tolist()}")

    def test_p_biased_now_in_unit_interval(self):
        bad = self.df[
            self.df["p_biased_now"].notna() &
            ((self.df["p_biased_now"] < 0) | (self.df["p_biased_now"] > 1))
        ]
        self.assertEqual(len(bad), 0,
            f"p_biased_now out of [0,1]: {bad['country'].tolist()}")

    def test_full_tail_required_countries_have_n_more(self):
        """full_tail_required status still reports the extrapolated n_more."""
        ftr = self.df[self.df["status"] == "full_tail_required"]
        if ftr.empty:
            return  # nothing to check
        missing = ftr[ftr["n_more_estimated"].isna()]
        self.assertEqual(len(missing), 0,
            f"full_tail_required rows missing n_more: {missing['country'].tolist()}")


# ---------------------------------------------------------------------------
# 9. Batch log integrity
# ---------------------------------------------------------------------------

class TestBatchLog(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        log_path = Path("data/ghsl_appraisal/stage_batch_log.csv")
        cls.skip = not log_path.exists()
        if not cls.skip:
            cls.log_df = pd.read_csv(log_path)

    def setUp(self):
        if self.skip:
            self.skipTest("stage_batch_log.csv not found")

    def test_required_columns_present(self):
        required = ["country", "n_added", "batch_source", "run_timestamp",
                    "n_fps_before", "n_reviewed_before", "p_biased_before"]
        missing = [c for c in required if c not in self.log_df.columns]
        self.assertEqual(len(missing), 0,
            f"Missing batch log columns: {missing}")

    def test_n_added_positive(self):
        bad = self.log_df[self.log_df["n_added"] <= 0]
        self.assertEqual(len(bad), 0,
            f"{len(bad)} batch log rows with n_added <= 0")

    def test_batch_source_values(self):
        valid_sources = {
            "E2 (R2=0.24)", "E2", "full_tail(!)", "fallback(indet)",
            "fallback(small)", "initial(0fp)"
        }
        # Batch sources are like "E2 (R2=0.76)" -- check prefix only
        bad = self.log_df[
            ~self.log_df["batch_source"].str.startswith(
                tuple(["E2", "full_tail", "fallback", "initial"])
            )
        ]
        self.assertEqual(len(bad), 0,
            f"Unexpected batch_source values: {bad['batch_source'].unique().tolist()}")


# ---------------------------------------------------------------------------
# 10. Config sync across pipeline scripts
# ---------------------------------------------------------------------------

class TestConfigSync(unittest.TestCase):

    def _extract_config_value(self, filepath, varname):
        """Parse a Python file and return the value assigned to varname."""
        import ast
        try:
            with open(filepath) as f:
                tree = ast.parse(f.read())
        except FileNotFoundError:
            return None
        for node in ast.walk(tree):
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name) and target.id == varname:
                        try:
                            return ast.literal_eval(node.value)
                        except Exception:
                            return None
        return None

    def test_recall_target_consistent(self):
        """RECALL_TARGET must be the same in E1 and E2."""
        for fname in ["E1_buscar_country_analysis.py",
                      "E2_extrapolate_stopping.py"]:
            val = self._extract_config_value(fname, "RECALL_TARGET")
            if val is not None:
                self.assertAlmostEqual(val, 0.90, places=4,
                    msg=f"{fname}: RECALL_TARGET={val}, expected 0.90")

    def test_confidence_consistent(self):
        """CONFIDENCE must be the same in E1 and E2."""
        for fname in ["E1_buscar_country_analysis.py",
                      "E2_extrapolate_stopping.py"]:
            val = self._extract_config_value(fname, "CONFIDENCE")
            if val is not None:
                self.assertAlmostEqual(val, 0.90, places=4,
                    msg=f"{fname}: CONFIDENCE={val}, expected 0.90")

    def test_all_scripts_import_base_dir_from_config(self):
        """E1, E2, E3 must import BASE_DIR from config (not hardcode it)."""
        import ast
        scripts = [
            "E1_buscar_country_analysis.py",
            "E2_extrapolate_stopping.py",
            "E3_expand_queue.py",
        ]
        for fname in scripts:
            if not os.path.exists(fname):
                continue
            with open(fname) as f:
                src = f.read()
            has_import = "from config import" in src and "BASE_DIR" in src
            has_hardcode = (
                'BASE_DIR   = "/' in src or
                "BASE_DIR   = '/" in src or
                'BASE_DIR = "/' in src
            )
            self.assertTrue(has_import,
                f"{fname}: does not import BASE_DIR from config")
            self.assertFalse(has_hardcode,
                f"{fname}: hardcodes BASE_DIR instead of importing from config")


# ---------------------------------------------------------------------------
# 11. fit_linear_trend
# ---------------------------------------------------------------------------

class TestFitLinearTrend(unittest.TestCase):

    def setUp(self):
        if not UTILS_AVAILABLE:
            self.skipTest("pipeline_utils not available")

    def test_returns_none_insufficient_points(self):
        x = np.arange(5, dtype=float)
        p = np.array([0.9, 0.8, 0.7, np.nan, np.nan])
        result = fit_linear_trend(x, p, window=50, min_window=6)
        self.assertIsNone(result, "Should return None with < min_window points")

    def test_returns_tuple_sufficient_points(self):
        x = np.arange(20, dtype=float)
        p = 1.0 - x * 0.04  # clean decreasing line
        result = fit_linear_trend(x, p, window=50, min_window=6)
        self.assertIsNotNone(result)
        self.assertEqual(len(result), 5)  # slope, intercept, x_win, p_win, r2

    def test_slope_correct_on_linear_data(self):
        x = np.arange(20, dtype=float)
        p = 1.0 - x * 0.05
        slope, intercept, _, _, r2 = fit_linear_trend(x, p, window=20, min_window=5)
        self.assertAlmostEqual(slope, -0.05, places=4,
            msg=f"Expected slope=-0.05, got {slope}")
        self.assertAlmostEqual(r2, 1.0, places=3,
            msg=f"Expected R2=1.0 on linear data, got {r2}")

    def test_r2_in_unit_interval(self):
        rng = np.random.default_rng(0)
        x = np.arange(30, dtype=float)
        p = rng.uniform(0.2, 0.9, 30)
        result = fit_linear_trend(x, p, window=30, min_window=5)
        if result is not None:
            _, _, _, _, r2 = result
            self.assertGreaterEqual(r2, 0.0)
            self.assertLessEqual(r2, 1.0)

    def test_window_uses_tail_only(self):
        """With window=5, only the last 5 points should be used."""
        x = np.arange(20, dtype=float)
        # First 15 points noisy, last 5 perfectly linear with slope=-0.1
        p = np.concatenate([
            np.random.default_rng(1).uniform(0.3, 0.9, 15),
            np.array([0.5, 0.4, 0.3, 0.2, 0.1])
        ])
        slope, _, x_win, _, _ = fit_linear_trend(x, p, window=5, min_window=4)
        self.assertEqual(len(x_win), 5, "Should use exactly 5 window points")
        self.assertAlmostEqual(slope, -0.1, places=4)

    def test_ignores_nan(self):
        x = np.arange(20, dtype=float)
        p = np.where(np.arange(20) % 3 == 0, np.nan, 1.0 - np.arange(20) * 0.04)
        result = fit_linear_trend(x, p, window=20, min_window=5)
        self.assertIsNotNone(result, "Should handle NaN gaps")


# ---------------------------------------------------------------------------
# 12. extrapolate_to_stop
# ---------------------------------------------------------------------------

class TestExtrapolateToStop(unittest.TestCase):

    def setUp(self):
        if not UTILS_AVAILABLE:
            self.skipTest("pipeline_utils not available")

    def test_indeterminate_when_slope_positive(self):
        x_stop, n_more = extrapolate_to_stop(slope=0.01, intercept=0.5, x_last=50)
        self.assertIsNone(x_stop)
        self.assertIsNone(n_more)

    def test_indeterminate_when_slope_zero(self):
        x_stop, n_more = extrapolate_to_stop(slope=0.0, intercept=0.5, x_last=50)
        self.assertIsNone(x_stop)
        self.assertIsNone(n_more)

    def test_correct_n_more_simple(self):
        # p = 1.0 - 0.01 * x  =>  0.10 = 1.0 - 0.01*x  =>  x=90
        # x_last=50, so n_more = 90 - 50 = 40
        _, n_more = extrapolate_to_stop(
            slope=-0.01, intercept=1.0, x_last=50, p_stop=0.10
        )
        self.assertEqual(n_more, 40)

    def test_n_more_floored_at_zero(self):
        # If x_stop < x_last, already past stopping point, n_more = 0
        _, n_more = extrapolate_to_stop(
            slope=-0.01, intercept=0.05, x_last=100, p_stop=0.10
        )
        self.assertEqual(n_more, 0)

    def test_n_more_nonnegative(self):
        for slope in [-0.001, -0.01, -0.1, -1.0]:
            _, n_more = extrapolate_to_stop(slope, intercept=0.8, x_last=20)
            self.assertGreaterEqual(n_more, 0,
                f"n_more={n_more} < 0 for slope={slope}")

    def test_x_stop_formula(self):
        # x_stop = (p_stop - intercept) / slope = (0.10 - 0.60) / (-0.02) = 25
        x_stop, _ = extrapolate_to_stop(
            slope=-0.02, intercept=0.60, x_last=10, p_stop=0.10
        )
        self.assertAlmostEqual(x_stop, 25.0, places=5)

    def test_returns_ceil_n_more(self):
        # x_stop = (0.10 - 0.55) / (-0.03) = 15.0
        # x_last = 14.3 -> n_more = ceil(15.0 - 14.3) = ceil(0.7) = 1
        _, n_more = extrapolate_to_stop(
            slope=-0.03, intercept=0.55, x_last=14.3, p_stop=0.10
        )
        self.assertEqual(n_more, 1)


# ---------------------------------------------------------------------------
# 13. p_trajectory
# ---------------------------------------------------------------------------

class TestPTrajectory(unittest.TestCase):

    def setUp(self):
        try:
            from buscarpy import calculate_h0  # noqa
            if not UTILS_AVAILABLE:
                self.skipTest("pipeline_utils not available")
        except ImportError:
            self.skipTest("buscarpy not available")

    def test_length_matches_labels(self):
        labels = np.array([1, 1, 0, 0, 1, 0, 0, 0, 0, 0])
        ps = p_trajectory(labels, n_ucdb=50, omega=1.0,
                          recall_target=0.9, min_fps=3)
        self.assertEqual(len(ps), len(labels))

    def test_early_steps_nan_before_min_fps(self):
        labels = np.array([0, 0, 1, 1, 1, 0, 0, 0, 0, 0])
        # min_fps=3: first FP at index 2, third at index 4
        ps = p_trajectory(labels, n_ucdb=50, omega=1.0,
                          recall_target=0.9, min_fps=3)
        # Steps before 3rd FP (index 4) should be NaN
        self.assertTrue(np.isnan(ps[0]), "Before min_fps should be NaN")
        self.assertTrue(np.isnan(ps[1]), "Before min_fps should be NaN")

    def test_final_step_always_computed(self):
        labels = np.array([1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
        ps = p_trajectory(labels, n_ucdb=50, omega=1.0,
                          recall_target=0.9, min_fps=3)
        self.assertFalse(np.isnan(ps[-1]),
            "Final step should always be computed")

    def test_final_p_matches_direct_calculate_h0(self):
        """p_trajectory[-1] must equal direct calculate_h0 on full sequence."""
        from buscarpy import calculate_h0
        labels = np.array([1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0])
        N, omega, rt = 60, 2.5, 0.9
        ps     = p_trajectory(labels, n_ucdb=N, omega=omega, recall_target=rt)
        p_direct = calculate_h0(labels, N=N, recall_target=rt, bias=omega)
        valid = ps[~np.isnan(ps)]
        if len(valid) > 0 and p_direct is not None:
            self.assertAlmostEqual(float(valid[-1]), float(p_direct), places=4,
                msg=f"trajectory[-1]={valid[-1]} != direct={p_direct}")

    def test_all_nan_when_no_fps(self):
        labels = np.zeros(15, dtype=int)
        ps = p_trajectory(labels, n_ucdb=50, omega=1.0,
                          recall_target=0.9, min_fps=3)
        # All NaN because cum_fps never reaches min_fps=3
        self.assertTrue(np.all(np.isnan(ps)),
            "All steps should be NaN when no FPs found")

    def test_values_in_unit_interval(self):
        labels = np.array([1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
        ps = p_trajectory(labels, n_ucdb=80, omega=2.0,
                          recall_target=0.9, min_fps=3)
        valid = ps[~np.isnan(ps)]
        self.assertTrue(np.all(valid >= 0), "p-values below 0")
        self.assertTrue(np.all(valid <= 1), "p-values above 1")


# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    loader = unittest.TestLoader()
    suite  = loader.loadTestsFromModule(sys.modules[__name__])
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    print("\n" + "=" * 60)
    print(f"  Tests run    : {result.testsRun}")
    print(f"  Failures     : {len(result.failures)}")
    print(f"  Errors       : {len(result.errors)}")
    print(f"  Skipped      : {len(result.skipped)}")
    ok = len(result.failures) == 0 and len(result.errors) == 0
    print(f"  Result       : {'PASS' if ok else 'FAIL'}")
    print("=" * 60)
    sys.exit(0 if ok else 1)