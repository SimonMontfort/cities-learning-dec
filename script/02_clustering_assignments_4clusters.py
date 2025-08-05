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


def align_soft_labels(reference_probs, target_probs):
    """
    Align cluster columns in target_probs to reference_probs using Hungarian algorithm.
    Returns the aligned target_probs and the mapping.
    """
    n_clusters = reference_probs.shape[1]
    cost_matrix = np.zeros((n_clusters, n_clusters))

    for i in range(n_clusters):
        for j in range(n_clusters):
            cost_matrix[i, j] = np.linalg.norm(reference_probs[:, i] - target_probs[:, j])

    row_ind, col_ind = linear_sum_assignment(cost_matrix)
    mapping = {j: i for i, j in zip(col_ind, row_ind)}
    aligned_probs = target_probs[:, col_ind]

    return aligned_probs, mapping


def load_and_align_soft_probs(n_clusters, n_runs, soft_dir):
    """
    Loads and aligns soft assignments across runs for given cluster count.
    Returns mean, std, and entropy of aligned soft probabilities.
    """
    soft_files = [
        os.path.join(soft_dir, f"dec_soft_assignments_run{run}_clusters{n_clusters}.csv")
        for run in range(n_runs)
    ]

    soft_probs_list = []
    for f in soft_files:
        df = pd.read_csv(f)
        probs = df.filter(like="cluster_").values
        soft_probs_list.append(probs)

    ref_probs = soft_probs_list[0]
    aligned = [ref_probs]

    for probs in soft_probs_list[1:]:
        aligned_probs, _ = align_soft_labels(ref_probs, probs)
        aligned.append(aligned_probs)

    aligned_stack = np.stack(aligned)  # (n_runs, n_samples, n_clusters)
    print("aligned_stack.shape:", aligned_stack.shape)
    mean_probs = aligned_stack.mean(axis=0)
    std_probs = aligned_stack.std(axis=0)
    entropies = -np.sum(mean_probs * np.log(mean_probs + 1e-10), axis=1)
    final_labels = mean_probs.argmax(axis=1)

    return final_labels, aligned_stack, std_probs, entropies



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

def summarize_clustering_results(results, ghs_ids, n_clusters, use_soft=False, soft_dir=None):
    df_summary = pd.DataFrame({'GHS_urban_area_id': ghs_ids})

    if use_soft:
        if soft_dir is None:
            raise ValueError("Must provide soft_dir when use_soft=True")

        # Load and align soft assignments from all runs
        final_labels, aligned_stack, std_probs, entropies = load_and_align_soft_probs(n_clusters, n_runs, soft_dir)

        # You need silhouettes to weight runs
        _, silhouettes = get_preds_and_silhouettes(results, n_clusters, 'dec')

        # Compute weights from silhouettes
        weights = np.array(silhouettes)
        weights = (weights - weights.min()) / (weights.max() - weights.min() + 1e-8)
        weights = 1 / (1 + np.exp(-12 * (weights - 0.4)))
        weights /= weights.sum()

        # Weighted average of soft probabilities
        mean_probs = np.tensordot(weights, aligned_stack, axes=([0], [0]))  # (n_samples, n_clusters)
        final_labels = mean_probs.argmax(axis=1)

        consensus_labels_maj = final_labels

        consensus_df = pd.DataFrame({
            'GHS_urban_area_id': ghs_ids,
            'consensus_label_majority': consensus_labels_maj,
            'entropy': entropies
        })

        for c in range(n_clusters):
            consensus_df[f"mean_prob_cluster_{c}"] = mean_probs[:, c]

    else:
        # Filter DEC results for this k
        filtered = results[
            (results['n_clusters'] == n_clusters) &
            (results['method'] == 'dec')
        ]

        if filtered.empty:
            print(f"No DEC results for {n_clusters} clusters.")
            return None

        all_preds, silhouettes = get_preds_and_silhouettes(results, n_clusters, 'dec')
        ref_pred = all_preds[0]
        aligned_preds = [ref_pred if i == 0 else align_clusters(ref_pred, pred) for i, pred in enumerate(all_preds)]

        weights = np.array(silhouettes)
        weights = (weights - weights.min()) / (weights.max() - weights.min() + 1e-8)
        weights = 1 / (1 + np.exp(-15 * (weights - 0.5)))
        weights /= weights.sum()

        consensus_labels_maj = compute_weighted_majority_vote(aligned_preds, weights)
        consensus_labels_eac = compute_assignment_eac(aligned_preds, n_clusters=n_clusters)

        consensus_df = pd.DataFrame({
            'GHS_urban_area_id': ghs_ids,
            'consensus_label_eac': consensus_labels_eac,
            'consensus_label_majority': consensus_labels_maj
        })

    # Representative scores (same regardless of soft/hard)
    rep_df = compute_representative_scores(
        cities_clean_scaled_df.drop(columns=['GHS_urban_area_id']),
        consensus_labels_maj, n_clusters, ghs_ids
    )

    final_df = (
        df_summary
        .merge(rep_df, on='GHS_urban_area_id', how='left')
        .merge(consensus_df, on='GHS_urban_area_id', how='left')
    )

    return final_df




if __name__ == "__main__":
    os.chdir("/Users/simon/Documents/repo/cities-learning-DEC")

    n_runs = 30
    cities_clean_scaled_df = pd.read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")
    ghs_ids = cities_clean_scaled_df["GHS_urban_area_id"].values
    performance_scores = pd.read_csv("data/clustering_results/raw_clustering_scores.csv")

    '''
    latent_runs = [
        joblib.load(f"clustering_models/latent_representation/latent_run_{run_id}.pkl").drop(columns=['GHS_urban_area_id']).values
        for run_id in range(n_runs)
    ]
    '''

    cluster_range = range(4,5)

    for n_clusters in cluster_range:
        df_final = summarize_clustering_results(performance_scores, ghs_ids, n_clusters, use_soft=True, soft_dir="data/clustering_results")
        if df_final is None:
            continue
        out_path = f"data/clustering_results/dec_clusters_k{n_clusters}.csv"
        df_final.to_csv(out_path, index=False)
        print(f"Saved: {out_path}")
