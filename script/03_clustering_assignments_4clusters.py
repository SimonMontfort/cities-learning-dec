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
import re
import glob

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


def detect_small_clusters(pattern="data/clustering_results/dec_soft_assignments_run*_cluster_4.csv", threshold=10):

    problem_runs = {}
    files = glob.glob(pattern)

    for f in files:
        # Extract run index
        filename = os.path.basename(f)
        match = re.search(r"run(\d+)", filename)
        run_id = int(match.group(1))

        df = pd.read_csv(f)

        # Identify prob columns
        prob_cols = [c for c in df.columns if c.startswith("cluster_") and c.endswith("_prob")]

        # Assign the cluster with max probability
        df["assigned_cluster"] = df[prob_cols].idxmax(axis=1).str.replace("_prob", "")

        # Count per-cluster assignments
        counts = df["assigned_cluster"].value_counts().to_dict()

        # Check if any cluster < threshold
        small = {cl: n for cl, n in counts.items() if n < threshold}

        if small:
            problem_runs[run_id] = small

    return problem_runs

def summarize_clustering_results(results, ghs_ids, n_clusters, dir=None):
    df_summary = pd.DataFrame({'GHS_urban_area_id': ghs_ids})

    if dir is None:
        raise ValueError("Must provide dir")

    # 1. Load hard predictions and silhouettes for all runs
    all_preds, silhouettes = get_preds_and_silhouettes(results, n_clusters, 'dec')

    n_runs = len(all_preds)
    print(f"Loaded {n_runs} DEC runs")

    # 2. exclude problematic runs + silhouette weighting
    manual_excluded = detect_small_clusters(pattern=f"data/clustering_results/dec_soft_assignments_run*_clusters4.csv", threshold=10)
    print(manual_excluded)
    manual_excluded ={}

    # Step 1: start with all runs except manually excluded
    candidate_indices = [i for i in range(n_runs) if i not in manual_excluded]

    # Step 2: apply silhouette-based filtering
    silhouettes_array = np.array(silhouettes)[candidate_indices]
    silhouette_threshold = np.percentile(silhouettes_array, 0)

    silhouette_mask = silhouettes_array >= silhouette_threshold
    included_indices = [candidate_indices[i] for i in range(len(candidate_indices)) if silhouette_mask[i]]

    print(f"Initially had {n_runs} runs")
    print(f"Excluded manually: {manual_excluded}")
    print(f"After silhouette filtering: {len(included_indices)} runs kept")

    # Filter predictions and silhouettes using final indices
    all_preds = [all_preds[i] for i in included_indices]
    silhouettes = np.array([silhouettes[i] for i in included_indices])

    # 3. Choose the best run as reference
    best_idx = np.argmax(silhouettes)
    ref_pred = all_preds[best_idx]
    print(f"Using run {included_indices[best_idx]} as reference for alignment")

    # 4. HARD-LABEL alignment
    aligned_hard = []
    for pred in all_preds:
        aligned = align_clusters(ref_pred, pred)
        aligned_hard.append(aligned)

    aligned_hard = np.vstack(aligned_hard)   # (runs, samples)
    print("Aligned hard labels shape:", aligned_hard.shape)

    # 5. Convert aligned hard labels to one-hot matrices
    one_hot = np.eye(n_clusters)[aligned_hard]  # (runs, samples, clusters)
    print(one_hot)

    # 6. Unweighted consensus (simple average of one-hot vectors)
    w = np.ones(len(silhouettes)) / len(silhouettes)

    mean_probs = np.tensordot(w, one_hot, axes=([0], [0]))  # (samples, clusters)
    final_labels = mean_probs.argmax(axis=1)
    consensus_labels_maj = final_labels

    # 7. Compute entropies of the consensus probabilities
    entropies = -np.sum(mean_probs * np.log(mean_probs + 1e-12), axis=1)

    # 8. Output final consensus
    consensus_df = pd.DataFrame({
        "GHS_urban_area_id": ghs_ids,
        "consensus_label_majority": final_labels,
        "entropy": entropies
    })

    for c in range(n_clusters):
        consensus_df[f"mean_prob_cluster_{c}"] = mean_probs[:, c]

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
    os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

    n_runs = 50
    cities_clean_scaled_df = pd.read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")
    ghs_ids = cities_clean_scaled_df["GHS_urban_area_id"].values
    performance_scores = pd.read_csv("data/clustering_results/raw_clustering_scores.csv")

    cluster_range = range(3,12)

    for n_clusters in cluster_range:
        df_final = summarize_clustering_results(performance_scores, ghs_ids, n_clusters, dir="data/clustering_results")
        if df_final is None:
            continue
        out_path = f"data/clustering_results/dec_clusters_k{n_clusters}.csv"
        df_final.to_csv(out_path, index=False)
        print(f"Saved: {out_path}")
