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


def calc_ari_entropy(all_preds, n_clusters):
    """
    Calculate ARI and entropy statistics over multiple clustering label sets.
    Returns mean ARI, mean entropy, probability matrix, aligned predictions, per-run ARI.
    """
    reference = all_preds[0]
    aligned_preds = [reference]
    per_run_ari = [1.0]  # ARI(reference, reference) = 1.0
    # per_run_entropy = []

    for pred in all_preds[1:]:
        aligned = align_clusters(reference, pred)
        aligned_preds.append(aligned)
        per_run_ari.append(adjusted_rand_score(reference, aligned))

    aligned_preds = np.array(aligned_preds)

    mean_ari = np.mean(per_run_ari)
    per_run_entropy = [
        entropy(np.bincount(pred, minlength=n_clusters) / len(pred) + 1e-10, base=2)
        for pred in aligned_preds
    ]
    mean_entropy = np.mean(per_run_entropy)

    prob_matrix = np.zeros((len(reference), n_clusters))
    for i in range(len(aligned_preds)):
        for idx in range(len(reference)):
            prob_matrix[idx][aligned_preds[i][idx]] += 1
    prob_matrix /= len(aligned_preds)

    return mean_ari, mean_entropy, prob_matrix, aligned_preds, per_run_ari, per_run_entropy


def parse_labels(label_str):
    """Convert comma-separated label string to numpy array of ints."""
    return np.array(list(map(int, label_str.split(','))))


def get_preds_and_silhouettes(results, n_clusters, method):
    """Extract predictions and silhouette scores for a specific method and cluster count."""
    filtered = results[(results['n_clusters'] == n_clusters) & (results['method'] == method)]
    preds = [parse_labels(row['labels']) for _, row in filtered.iterrows()]
    silhouettes = filtered['silhouette'].tolist()
    return preds, silhouettes


def compute_representative_scores(latent_reps, assigned_clusters, n_clusters, ghs_ids):
    """
    Compute representative score for each city in each cluster as the average distance
    to other cities in the same cluster in latent space.
    """
    latent_unitnorm = normalize(latent_reps, norm='l2')
    rep_scores = []

    for cluster_id in range(n_clusters):
        cluster_indices = np.where(assigned_clusters == cluster_id)[0]
        if len(cluster_indices) == 0:
            continue
        cluster_latents = latent_unitnorm[cluster_indices]
        dists = pairwise_distances(cluster_latents)
        avg_dists = dists.mean(axis=1)

        for idx, city_idx in enumerate(cluster_indices):
            rep_scores.append({
                'GHS_urban_area_id': ghs_ids[city_idx],
                'similarity': avg_dists[idx]
            })

    return pd.DataFrame(rep_scores)

# Add this helper function to compute uncertainty from soft assignments:
def compute_uncertainty_from_soft_assignments(Q_all):
    # Q_all: shape (T, N, K) - soft assignments for ensemble runs
    sim_matrices = np.einsum('tnk,tmk->tnm', Q_all, Q_all)  # similarity matrices per run
    sim_var = np.var(sim_matrices, axis=0)                   # variance over runs, shape (N, N)
    uncertainty = np.mean(sim_var, axis=1)                   # average variance per sample, shape (N,)
    return uncertainty

def load_soft_assignments_for_runs(top_indices, n_clusters, data_dir="data/clustering_results"):
    Q_all = []
    for run_id in top_indices:
        path = f"{data_dir}/dec_soft_assignments_run{run_id}_clusters{n_clusters}.csv"
        df = pd.read_csv(path)
        # Drop the ID column, keep only cluster probability columns
        cluster_cols = [f"cluster_{i}_prob" for i in range(n_clusters)]
        q = df[cluster_cols].values  # shape (N, K)
        Q_all.append(q)
    Q_all = np.array(Q_all)  # shape (top_n, N, K)
    return Q_all

def summarize_clustering_results(results, n_runs, ghs_ids, latent_runs, n_clusters, top_percents=[0.2, 0.3, 0.4]):
    consistency = defaultdict(lambda: defaultdict(list))

    all_preds_kmeans, silhouettes_kmeans = get_preds_and_silhouettes(results, n_clusters, 'kmeans')
    all_preds_dec, silhouettes_dec = get_preds_and_silhouettes(results, n_clusters, 'dec')

    for method, all_preds, silhouettes in zip(['KMeans', 'DEC'],
                                             [all_preds_kmeans, all_preds_dec],
                                             [silhouettes_kmeans, silhouettes_dec]):

        if len(all_preds) == 0:
            print(f"No results found for method {method} with {n_clusters} clusters.")
            continue

        ari, ent, prob_matrix, aligned_preds, per_run_ari, per_run_entropy = calc_ari_entropy(all_preds, n_clusters)
        consistency[method]['ARI'].append(ari)
        consistency[method]['entropy'].append(ent)
        consistency[method]['per_run_ARI'] = per_run_ari
        consistency[method]['per_run_entropy'] = per_run_entropy

        silhouettes_arr = np.array(silhouettes)

        for top_percent in top_percents:
            top_n = max(1, int(n_runs * top_percent))
            top_indices = np.argsort(silhouettes_arr)[-top_n:]
            top_preds = [aligned_preds[i] for i in top_indices]

            # Find best reference run for alignment (same as before)
            n_top = len(top_preds)
            ari_matrix = np.zeros((n_top, n_top))

            for i in range(n_top):
                for j in range(n_top):
                    ari_matrix[i, j] = adjusted_rand_score(top_preds[i], top_preds[j])

            best_ref_idx = np.argmax(ari_matrix.sum(axis=1))
            best_ref = top_preds[best_ref_idx]

            realigned_top_preds = [best_ref]
            for i, pred in enumerate(top_preds):
                if i == best_ref_idx:
                    continue
                aligned_pred = align_clusters(best_ref, pred)
                realigned_top_preds.append(aligned_pred)
            realigned_top_preds = np.array(realigned_top_preds)

            # Compute probability matrix from aligned labels (same as before)
            prob_matrix_top = np.zeros((len(realigned_top_preds[0]), n_clusters))
            for i in range(len(realigned_top_preds)):
                for idx in range(len(realigned_top_preds[i])):
                    prob_matrix_top[idx][realigned_top_preds[i][idx]] += 1
            prob_matrix_top /= len(realigned_top_preds)

            # ===== REPLACE ENTROPY UNCERTAINTY WITH SOFT ASSIGNMENT-BASED UNCERTAINTY (DEC ONLY) =====
            if method == 'DEC':
                Q_all = load_soft_assignments_for_runs(top_indices, n_clusters)

                for i, run_idx in enumerate(top_indices):
                    for sample_idx in range(len(ghs_ids)):
                        cluster_id = aligned_preds[run_idx][sample_idx]
                        Q_all[i, sample_idx, cluster_id] = 1.0  # one-hot placeholder

                uncertainty_scores = compute_uncertainty_from_soft_assignments(Q_all)

                df_summary = pd.DataFrame({
                    'GHS_urban_area_id': ghs_ids,
                    'most_probable_cluster': np.argmax(prob_matrix_top, axis=1),
                    'assignment_probability': np.max(prob_matrix_top, axis=1),
                    'uncertainty_score': uncertainty_scores  # replaces entropy here
                })
            else:
                # For KMeans or others, fallback to entropy on label probabilities
                uncertainty = [entropy(p + 1e-10, base=2) for p in prob_matrix_top]
                df_summary = pd.DataFrame({
                    'GHS_urban_area_id': ghs_ids,
                    'most_probable_cluster': np.argmax(prob_matrix_top, axis=1),
                    'assignment_probability': np.max(prob_matrix_top, axis=1),
                    'entropy': uncertainty
                })

            latent_for_representatives = latent_runs[top_indices[best_ref_idx]]
            assigned_clusters = realigned_top_preds[0]
            rep_df = compute_representative_scores(latent_for_representatives, assigned_clusters, n_clusters, ghs_ids)

            df_summary['GHS_urban_area_id'] = df_summary['GHS_urban_area_id'].astype(int)
            rep_df['GHS_urban_area_id'] = rep_df['GHS_urban_area_id'].astype(int)

            df_summary = df_summary.merge(rep_df, on='GHS_urban_area_id', how='left')
            df_summary['similarity'] = df_summary['similarity'].fillna(np.inf)

            output_path = f"data/clustering_results/cluster_assignments_{method}_{n_clusters}_top{int(top_percent*100)}pct.csv"
            df_summary.to_csv(output_path, index=False)
            print(f"Saved clustering summary for {method} with {n_clusters} clusters (top {int(top_percent*100)}%) to {output_path}")

    return consistency



if __name__ == "__main__":
    os.chdir("/Users/simon/Documents/repo/cities-learning-DEC")

    cluster_range = range(3,8)
    n_runs = 30

    # Load data
    cities_clean_scaled_df = pd.read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")
    ghs_ids = cities_clean_scaled_df["GHS_urban_area_id"].values

    performance_scores = pd.read_csv("data/clustering_results/raw_clustering_scores.csv")

    latent_runs = [
        joblib.load(f"clustering_models/latent_representation/latent_run_{run_id}.pkl").drop(columns=['GHS_urban_area_id']).values
        for run_id in range(n_runs)
    ]

    all_ari_entropy_records = []

    top_percents = [0.2, 0.3, 0.4]  # Sensitivity analysis for top 20%, 30%, 40%

    for n_clusters in cluster_range:
        consistency = summarize_clustering_results(performance_scores, n_runs, ghs_ids, latent_runs, n_clusters, top_percents=top_percents)

        # Append ARI results per run as before
        for method, stats in consistency.items():
            for run_idx in range(len(stats['per_run_ARI'])):
                all_ari_entropy_records.append({
                    'method': method,
                    'run': run_idx,
                    'ARI': stats['per_run_ARI'][run_idx],
                    'entropy': stats['per_run_entropy'][run_idx],
                    'n_clusters': n_clusters
                })

    df_ari_entr = pd.DataFrame(all_ari_entropy_records)
    df_ari_entr.to_csv("data/clustering_results/all_per_run_ari_entropy.csv", index=False)

    print("Clustering labels assigned successfully.")
