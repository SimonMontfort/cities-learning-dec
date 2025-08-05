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
from sklearn.cluster import KMeans, AgglomerativeClustering

def align_clusters(reference, pred):
    """
    Align cluster labels of pred to reference using the Hungarian algorithm.
    Prints the mapping and Jaccard overlaps.
    """
    D = max(reference.max(), pred.max()) + 1
    cost = np.zeros((D, D), dtype=int)
    for i in range(reference.size):
        cost[reference[i], pred[i]] += 1
    row_ind, col_ind = linear_sum_assignment(-cost)
    mapping = {col: row for row, col in zip(row_ind, col_ind)}

    print(f"Aligned clusters mapping: { {int(k): int(v) for k, v in mapping.items()} }")
    print("Jaccard overlap between matched clusters:")
    for pred_cluster, ref_cluster in mapping.items():
        pred_indices = set(np.where(pred == pred_cluster)[0])
        ref_indices = set(np.where(reference == ref_cluster)[0])
        intersection = pred_indices & ref_indices
        union = pred_indices | ref_indices
        jaccard = 100 * len(intersection) / len(union) if len(union) > 0 else 0
        print(f"  Pred cluster {pred_cluster} ↔ Ref cluster {ref_cluster} | Jaccard overlap: {jaccard:.2f}%")

    return np.array([mapping[label] for label in pred])


def parse_labels(label_str):
    """Convert comma-separated label string to numpy array of ints."""
    return np.array(list(map(int, label_str.split(','))))


def get_preds_and_silhouettes(results, n_clusters, method):
    """Extract predictions and silhouette scores for a specific method and cluster count."""
    filtered = results[(results['n_clusters'] == n_clusters) & (results['method'] == method)]
    preds = [parse_labels(row['labels']) for _, row in filtered.iterrows()]
    silhouettes = filtered['silhouette'].tolist()
    return preds, silhouettes

def compute_representative_scores(features_df, assigned_clusters, n_clusters, ghs_ids):
    """
    Compute representativeness for each city based on its average distance
    to other cities in the same cluster using original scaled covariates.
    Lower score = more representative.
    """
    feature_matrix = normalize(features_df.values, norm='l2')  # Normalize to avoid scale bias
    rep_scores = []

    for cluster_id in range(n_clusters):
        cluster_indices = np.where(assigned_clusters == cluster_id)[0]
        if len(cluster_indices) == 0:
            continue

        cluster_features = feature_matrix[cluster_indices]
        dists = pairwise_distances(cluster_features)
        avg_dists = dists.mean(axis=1)

        for idx, city_idx in enumerate(cluster_indices):
            rep_scores.append({
                'GHS_urban_area_id': ghs_ids[city_idx],
                'similarity': avg_dists[idx]
            })

    return pd.DataFrame(rep_scores)


def compute_assignment_eac(all_labels_for_k, n_clusters=6):
    n_runs = len(all_labels_for_k)
    n_samples = len(all_labels_for_k[0])

    coassoc = np.zeros((n_samples, n_samples))
    for labels in all_labels_for_k:
        for c in np.unique(labels):
            idx = np.where(labels == c)[0]
            coassoc[np.ix_(idx, idx)] += 1

    coassoc /= n_runs

    # Hierarchical clustering on 1 - coassoc
    consensus = AgglomerativeClustering(
        n_clusters=n_clusters, metric="precomputed", linkage="complete"
    ).fit_predict(1 - coassoc)

    return consensus

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


def summarize_clustering_results(results, ghs_ids, latent_runs, n_clusters):
    df_summary = pd.DataFrame({'GHS_urban_area_id': ghs_ids})

    # Select all DEC runs for this cluster count
    filtered = results[
        (results['n_clusters'] == n_clusters) &
        (results['method'] == 'dec')
    ]

    if filtered.empty:
        print(f"No DEC results for {n_clusters} clusters.")
        return None

    # Extract predictions and silhouette scores
    all_preds, silhouettes = get_preds_and_silhouettes(results, n_clusters, 'dec')

    # Reference run is the first run (index 0)
    ref_idx = 0
    ref_pred = all_preds[ref_idx]

    # Align clusters of all runs to the first run
    aligned_preds = [ref_pred]
    for i, pred in enumerate(all_preds):
        if i != ref_idx:
            aligned_preds.append(align_clusters(ref_pred, pred))

    # Normalize scores first
    weights = np.array(silhouettes)
    weights = (weights - weights.min()) / (weights.max() - weights.min() + 1e-8)

    # Apply sigmoid
    alpha = 10  # adjust steepness
    beta = 0.6   # midpoint
    weights = 1 / (1 + np.exp(-alpha * (weights - beta)))

    # Normalize to sum to 1
    weights /= weights.sum()

    # Compute consensus clusterings
    consensus_labels_eac = compute_assignment_eac(aligned_preds, n_clusters=n_clusters)
    consensus_labels_maj = compute_weighted_majority_vote(aligned_preds, weights)

    # Create consensus dataframe
    consensus_df = pd.DataFrame({
        'GHS_urban_area_id': ghs_ids,
        'consensus_label_eac': consensus_labels_eac,
        'consensus_label_majority': consensus_labels_maj
    })

    # Use latent representation for the first run (as reference)
    best_run_id = filtered.iloc[ref_idx]['run_id']
    print(f"Using run_id {best_run_id} as reference")
    latent_best = latent_runs[best_run_id]

    # Compute representative scores using original covariates
    rep_df = compute_representative_scores(
        cities_clean_scaled_df.drop(columns=['GHS_urban_area_id']),
        consensus_labels_eac, n_clusters, ghs_ids
    )

    # Merge all results
    final_df = (
        df_summary
        .merge(rep_df, on='GHS_urban_area_id', how='left')
        .merge(consensus_df, on='GHS_urban_area_id', how='left')
    )

    return final_df


if __name__ == "__main__":
    os.chdir("/Users/simon/Documents/repo/cities-learning-DEC")

    n_runs = 10
    cities_clean_scaled_df = pd.read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")
    ghs_ids = cities_clean_scaled_df["GHS_urban_area_id"].values
    performance_scores = pd.read_csv("data/clustering_results/raw_clustering_scores.csv")

    latent_runs = [
        joblib.load(f"clustering_models/latent_representation/latent_run_{run_id}.pkl").drop(columns=['GHS_urban_area_id']).values
        for run_id in range(n_runs)
    ]

    cluster_range = range(4,7)

    for n_clusters in cluster_range:
        df_final = summarize_clustering_results(performance_scores, ghs_ids, latent_runs, n_clusters)
        if df_final is None:
            continue
        out_path = f"data/clustering_results/dec_clusters_k{n_clusters}.csv"
        df_final.to_csv(out_path, index=False)
        print(f"Saved: {out_path}")
