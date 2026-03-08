"""
buscarpy examples for UCDB false positive detection
=====================================================
Install:
    pip install buscarpy matplotlib

Run:
    python buscar_examples.py

Key API:
    calculate_h0(labels, N, recall_target, bias)

    labels        = ordered list of 1s and 0s — in the order you reviewed cities,
                    1 = false positive (drop), 0 = kept (genuine city)
    N             = total cities in your dataset (not just the queue)
    recall_target = e.g. 0.95
    bias          = scorer quality (1 = conservative/random, >1 = biased urn)

    Returns p-value. Stop when p <= 0.05.
"""

import os
import numpy as np
import matplotlib.pyplot as plt
from buscarpy import calculate_h0, retrospective_h0, recall_frontier, generate_dataset

# Save plots next to this script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# ── Scenario 1: Manually constructed review sequence ─────────────────────────
# Imagine you reviewed 300 cities from your queue in score order.
# False positives (1s) are concentrated at the start — good scorer behaviour.
# After ~200 reviews, almost nothing but 0s.

print("=" * 60)
print("Scenario 1: Manually constructed sequence (N=5,000)")
print("=" * 60)

np.random.seed(42)
# 40 FPs in first 150 reviews, then sparse
early  = np.random.choice([1, 0], size=150, p=[0.27, 0.73])
late   = np.random.choice([1, 0], size=150, p=[0.03, 0.97])
labels = np.concatenate([early, late])

p = calculate_h0(labels, N=5_000, recall_target=0.95, bias=1)
fps_found = labels.sum()
print(f"  Cities reviewed : {len(labels)}")
print(f"  False positives : {fps_found}")
print(f"  p-value         : {p:.4f}")
print(f"  Can stop (95%)? : {'YES' if p <= 0.05 else 'NO — keep reviewing'}")

# ── Scenario 2: Using generate_dataset (built-in simulator) ──────────────────
# generate_dataset simulates a scored + ranked dataset.
# bias = how much better your scorer is vs random (10 = strong scorer)

print("\n" + "=" * 60)
print("Scenario 2: Simulated dataset — strong scorer (bias=10)")
print("=" * 60)

# Note: calculate_h0 works on the *reviewed* sequence only, not the full dataset.
# N is the full dataset size. We simulate reviewing the top 2,000 from a 10,000 city dataset.
df = generate_dataset(N=10_000, prevalence=0.05, bias=10, random_seed=1)
labels_sim_full = df['relevant'].values
labels_sim = labels_sim_full[:2000]  # simulate reviewing top 2,000 by score

p = calculate_h0(labels_sim, N=10_000, recall_target=0.95, bias=1)
print(f"  Total cities    : 10,000")
print(f"  True FPs        : {labels_sim_full.sum()} ({labels_sim_full.mean()*100:.1f}%)")
print(f"  FPs in reviewed : {labels_sim.sum()}")
print(f"  Cities reviewed : {len(labels_sim)} (top 20% of dataset)")
print(f"  p-value (full)  : {p:.4f}")
print(f"  Can stop (95%)? : {'YES' if p <= 0.05 else 'NO'}")

# ── Scenario 3: Accounting for scorer bias (biased urn) ──────────────────────
# Same data, but now we tell calculate_h0 our scorer has bias=10
# This should let us stop earlier with the same confidence

print("\n" + "=" * 60)
print("Scenario 3: Same data — biased urn (bias=10)")
print("=" * 60)

p_biased = calculate_h0(labels_sim, N=10_000, recall_target=0.95, bias=10)
print(f"  p-value (bias=10) : {p_biased:.4f}  <- accounts for scorer quality")
print(f"  Can stop (95%)?   : {'YES' if p_biased <= 0.05 else 'NO'}")

# ── Scenario 4: How early can you stop? — retrospective scan ─────────────────
# retrospective_h0 re-calculates p every `batch_size` reviews
# Shows you at what point in your queue you could have stopped

print("\n" + "=" * 60)
print("Scenario 4: Retrospective — when could you have stopped?")
print("=" * 60)

result = retrospective_h0(
    labels_sim,
    N=10_000,
    recall_target=0.95,
    bias=10,
    batch_size=100,
    confidence_level=0.95,
    plot=True,
)
plt.suptitle("Retrospective stopping — simulated UCDB-scale dataset (bias=10)")
plt.tight_layout()
plt.savefig(os.path.join(SCRIPT_DIR, "retrospective_stopping.png"), dpi=150)
plt.close()
print("  Plot saved: retrospective_stopping.png")

stop_idx = int(np.argmax(np.array(result['p']) <= 0.05))
if result['p'][stop_idx] <= 0.05:
    stop_at = result['batch_sizes'][stop_idx]
    print(f"  Could have stopped after reviewing: {stop_at:,} cities")
    print(f"  That is {stop_at/10_000*100:.1f}% of the full dataset")
    print(f"  p at stopping point: {result['p'][stop_idx]:.4f}")

# ── Scenario 5: Recall frontier ───────────────────────────────────────────────
# Shows the p-value across a range of recall targets at your current point
# Useful for reporting: "we achieved X% recall with p=0.05"

print("\n" + "=" * 60)
print("Scenario 5: Recall frontier — what recall can you claim?")
print("=" * 60)

frontier = recall_frontier(labels_sim, N=10_000, bias=10, plot=True)
plt.title("Recall frontier — p-value by recall target")
plt.xlabel("Recall target")
plt.ylabel("p-value")
plt.axhline(0.05, color='red', linestyle='--', label='p=0.05')
plt.legend()
plt.tight_layout()
plt.savefig(os.path.join(SCRIPT_DIR, "recall_frontier.png"), dpi=150)
plt.close()
print("  Plot saved: recall_frontier.png")

# Find highest recall where p <= 0.05
pairs = list(zip(frontier['recall_target'], frontier['p']))
achieved = [(r, p) for r, p in pairs if p is not None and p <= 0.05]
if achieved:
    best_recall = max(r for r, _ in achieved)
    print(f"  Highest achievable recall at p<=0.05: {best_recall*100:.1f}%")

# ── How to use with your real cities_review.csv ───────────────────────────────
print("\n" + "=" * 60)
print("Using with your real cities_review.csv")
print("=" * 60)
print("""
    import pandas as pd
    from buscarpy import calculate_h0

    df = pd.read_csv('cities_review.csv')

    # Only use reviewed rows, in the order they appear (score descending)
    reviewed = df[df['decision'].str.strip() != ''].copy()

    # 1 = false positive (dropped), 0 = kept
    labels = (reviewed['decision'] == 'drop').astype(int).values

    N = len(df)  # total cities in your full dataset (not just queue)

    p = calculate_h0(labels, N=N, recall_target=0.95, bias=1)
    print(f"p = {p:.4f} -- {'can stop' if p <= 0.05 else 'keep reviewing'}")

    # Once you have an estimate of your scorer's bias, pass it in:
    # p = calculate_h0(labels, N=N, recall_target=0.95, bias=8)
""")