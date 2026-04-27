"""
pipeline_utils.py
=================
Shared pure functions for the UCDB false-positive detection pipeline.

Imported by E1, E2, E3 and tested directly by E0_tests.py.
No side effects at import time (no os.chdir, no file I/O, no plots).

Functions
---------
  compute_bias(country_reviewed)          -> float   (E1)
  make_labels(country_reviewed)           -> np.ndarray  (E1)
  p_trajectory(labels, n_ucdb, omega, ...) -> np.ndarray  (E2)
  fit_linear_trend(x, p_vals, window)     -> tuple | None  (E2)
  extrapolate_to_stop(slope, intercept, x_last, p_stop) -> tuple  (E2)
"""

import numpy as np
from scipy.stats import linregress

# ── E1 helpers ─────────────────────────────────────────────────────────────────

def compute_bias(country_reviewed):
    """
    Compute omega = [p_fp/(1-p_fp)] / [p_kept/(1-p_kept)]

    Uses within-country score percentile rank so the odds ratio reflects
    how well the scorer separates FPs from keepers *within* this country,
    not relative to the global score distribution.

    Parameters
    ----------
    country_reviewed : DataFrame
        Rows for one country with columns 'score' and 'decision'.
        Only reviewed rows (decision != "") should be passed.

    Returns
    -------
    float
        omega >= 1.0. Returns 1.0 if fewer than 2 FPs or 2 keepers.
    """
    pct  = country_reviewed["score"].rank(pct=True)
    fps  = pct[country_reviewed["decision"] != "keep"].dropna()
    kept = pct[country_reviewed["decision"] == "keep"].dropna()

    if len(fps) < 2 or len(kept) < 2:
        return 1.0

    p_fp   = fps.mean().clip(0.01, 0.99)
    p_kept = kept.mean().clip(0.01, 0.99)
    omega  = (p_fp / (1 - p_fp)) / (p_kept / (1 - p_kept))
    return max(1.0, round(float(omega), 2))


def make_labels(country_reviewed):
    """
    Build the buscarpy label sequence for one country.

    1 = FP (drop or ambiguous), 0 = keep.
    Sorted by score descending = highest-risk cities reviewed first.

    Parameters
    ----------
    country_reviewed : DataFrame
        Reviewed rows for one country with columns 'score' and 'decision'.

    Returns
    -------
    np.ndarray of int (0/1), length = len(country_reviewed)
    """
    s = country_reviewed.sort_values("score", ascending=False)
    return (s["decision"] != "keep").astype(int).values


# ── E2 helpers ─────────────────────────────────────────────────────────────────

def p_trajectory(labels, n_ucdb, omega, recall_target, min_fps=3):
    """
    Compute buscarpy p_biased at every step of the label sequence.

    Returns array of length len(labels):
      ps[i] = p after reviewing the first i+1 cities (score desc order)
      ps[i] = NaN if fewer than min_fps FPs have been found by step i

    Uses an adaptive stride (~80 evaluations) for speed, but always
    computes the final step so p_now matches E1's calculate_h0 exactly.

    Parameters
    ----------
    labels       : array-like of 0/1
    n_ucdb       : int   — total cities in UCDB for this country
    omega        : float — scorer bias (>= 1)
    recall_target: float — e.g. 0.90
    min_fps      : int   — skip steps with fewer accumulated FPs

    Returns
    -------
    np.ndarray of float, length = len(labels), NaN where not computed
    """
    from buscarpy import calculate_h0

    labels = np.asarray(labels)
    n      = len(labels)
    ps     = np.full(n, np.nan)
    cum    = np.cumsum(labels)
    stride = max(1, n // 80)

    for i in range(n):
        if cum[i] < min_fps:
            continue
        if i % stride != 0 and i != n - 1:
            continue
        p = calculate_h0(labels[:i+1], N=n_ucdb,
                         recall_target=recall_target, bias=omega)
        ps[i] = float(p) if p is not None else np.nan

    return ps


def fit_linear_trend(x, p_vals, window=50, min_window=None):
    """
    Fit an OLS line through the last `window` non-NaN points of p_vals.

    Parameters
    ----------
    x         : array-like of float — x-axis (cumulative reviews)
    p_vals    : array-like of float — p-value trajectory (NaN where missing)
    window    : int — number of tail points to fit
    min_window: int — minimum non-NaN points required; defaults to
                      max(6, window // 5)

    Returns
    -------
    (slope, intercept, x_window, p_window, r_squared) if enough data,
    None otherwise.
    """
    x      = np.asarray(x,      dtype=float)
    p_vals = np.asarray(p_vals, dtype=float)

    if min_window is None:
        min_window = max(6, window // 5)

    mask = ~np.isnan(p_vals)
    x_ok = x[mask]
    p_ok = p_vals[mask]

    if len(p_ok) < min_window:
        return None

    x_win = x_ok[-window:]
    p_win = p_ok[-window:]
    slope, intercept, r, _, _ = linregress(x_win, p_win)
    return slope, intercept, x_win, p_win, round(r**2, 3)


def extrapolate_to_stop(slope, intercept, x_last, p_stop=0.10):
    """
    Solve p = slope*x + intercept for x where p = p_stop.

    Parameters
    ----------
    slope, intercept : float — from fit_linear_trend
    x_last           : float — index of last reviewed city
    p_stop           : float — target p-value (default 0.10)

    Returns
    -------
    (x_stop, n_more) if slope < 0 (trend falling),
    (None, None)     if slope >= 0 (indeterminate).
    n_more = max(0, ceil(x_stop - x_last))
    """
    if slope >= 0:
        return None, None
    x_stop = (p_stop - intercept) / slope
    n_more = max(0, int(np.ceil(x_stop - x_last)))
    return x_stop, n_more
