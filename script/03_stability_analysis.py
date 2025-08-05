import os
import numpy as np
import pandas as pd
from collections import defaultdict
from scipy.optimize import linear_sum_assignment
from sklearn.metrics import adjusted_rand_score
from scipy.stats import entropy
from sklearn.preprocessing import normalize
from sklearn.metrics import pairwise_distances
import joblib
from sklearn.metrics import adjusted_rand_score
import numpy as np
import pandas as pd
import random
from sklearn.cluster import KMeans, AgglomerativeClustering


def align_clusters(reference, pred, verbose=False):
    """
    Align cluster labels of pred to reference using the Hungarian algorithm.
    Returns aligned labels.
    """
    reference = np.array(reference)
    pred = np.array(pred)

    D = max(reference.max(), pred.max()) + 1
    cost = np.zeros((D, D), dtype=int)
    for i in range(reference.size):
        cost[reference[i], pred[i]] += 1
    row_ind, col_ind = linear_sum_assignment(-cost)
    mapping = {col: row for row, col in zip(row_ind, col_ind)}

    if verbose:
        print(f"Alignment mapping: {mapping}")
        for pred_cluster, ref_cluster in mapping.items():
            pred_indices = np.where(pred == pred_cluster)[0]
            ref_indices = np.where(reference == ref_cluster)[0]
            overlap = len(set(pred_indices) & set(ref_indices))
            print(f"  pred {pred_cluster} → ref {ref_cluster}, overlap = {overlap}")

    return np.array([mapping[label] for label in pred])


def compute_weighted_majority_vote(labels_subset, weights=None):
    """
    Compute weighted majority vote consensus.
    labels_subset: list of aligned label arrays (runs), shape = (n_runs, n_samples)
    weights: array-like of length n_runs, normalized to sum to 1 (or None for equal)
    """
    labels_array = np.vstack(labels_subset)  # shape = (n_runs, n_samples)
    n_runs, n_samples = labels_array.shape

    if weights is None:
        weights = np.ones(n_runs) / n_runs

    consensus = []
    for i in range(n_samples):  # iterate over samples
        column = labels_array[:, i]
        scores = defaultdict(float)
        for run_idx, label in enumerate(column):
            scores[label] += weights[run_idx]
        consensus.append(max(scores.items(), key=lambda x: x[1])[0])
    return np.array(consensus)


def incremental_stability(all_labels, cluster_range, n_permutations=50, random_state=42):
    """
    Compute incremental stability using Evidence Accumulation Clustering (EAC).
    Builds co-association matrix incrementally and checks ARI stability.
    """
    random.seed(random_state)
    n_runs = len(all_labels)
    results = []

    for n_clusters in cluster_range:
        print(f"\n=== EAC for n_clusters = {n_clusters} ===")
        labels_for_k = [labels_dict[n_clusters] for labels_dict in all_labels]

        for perm_id in range(n_permutations):
            print(f"\n--- Permutation {perm_id+1}/{n_permutations} ---")
            perm_indices = list(range(n_runs))
            random.shuffle(perm_indices)
            shuffled_labels = [labels_for_k[i] for i in perm_indices]

            prev_consensus = None
            aligned_runs = []

            for ensemble_size in range(1, n_runs + 1):
                run_labels = shuffled_labels[ensemble_size - 1]

                if ensemble_size == 1:
                    aligned_runs = [np.array(run_labels)]
                    prev_consensus = np.array(run_labels)
                else:
                    current_aligned = align_clusters(prev_consensus, run_labels)
                    aligned_runs.append(current_aligned)

                # Compute consensus
                consensus = compute_weighted_majority_vote(aligned_runs)

                if prev_consensus is not None:
                    ari = adjusted_rand_score(prev_consensus, consensus)
                    print(f"Ensemble size {ensemble_size}: ARI = {ari:.4f}")
                    results.append({
                        "n_clusters": n_clusters,
                        "ensemble_size": ensemble_size,
                        "perm_id": perm_id,
                        "ari_vs_prev": ari
                    })

                prev_consensus = consensus

    return pd.DataFrame(results)



if __name__ == "__main__":
    os.chdir("/Users/simon/Documents/repo/cities-learning-DEC")

    cluster_range = range(4,5)
    n_runs = 30
    n_permutations = 100

    raw_clustering_scores = pd.read_csv("data/clustering_results/raw_clustering_scores.csv")
    dec_scores = raw_clustering_scores[raw_clustering_scores['method'].str.contains('dec', case=False)]

    all_labels = []  # Same structure: list of dicts per run

    # Group by run_id (so each run has its own dict of n_clusters → labels)
    for run_id, run_df in dec_scores.groupby('run_id'):
        labels_dict = {}
        for _, row in run_df.iterrows():
            k = row['n_clusters']
            # Parse labels string into a list of integers
            labels = list(map(int, row['labels'].split(',')))
            labels_dict[k] = labels
        all_labels.append(labels_dict)

    ensemble_stability_result = incremental_stability(all_labels, cluster_range, n_permutations=n_permutations)

    ensemble_stability_result.to_csv("data/clustering_results/ensemble_stability_analysis.csv", index=False)

    print("Stability and final assignment computed.")
