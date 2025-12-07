#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
03_gmm_hparam_search_k4.py

Hyperparameter search for GMM on DEC embeddings (k=4):

- Load scaled covariates and latent embeddings latent_run_{run_id}.pkl
- Build mean embedding across runs
- Run a hyperparameter grid over:
    * covariance_type ∈ {full, diag, tied}
    * reg_covar ∈ {1e-6, 1e-5, 1e-4}
    * temperature T ∈ {1.0, 1.5, 2.0, 3.0}
- For each config, compute:
    * ARI w.r.t. DEC consensus_label_majority (from dec_clusters_k4.csv)
    * penalty = average soft membership of African cities
      in clusters that are majority-European
    * combined_score = ARI - LAMBDA * penalty
- Select config with max combined_score
- Save best config's:
    * gmm_soft_assignments_k4.csv
    * gmm_clusters_k4.csv
- Save search summary to gmm_hparam_search_k4.csv
"""

import os
import gc
import numpy as np
import pandas as pd

from sklearn.preprocessing import normalize
from sklearn.metrics import (
    pairwise_distances,
    silhouette_score,
    calinski_harabasz_score,
    davies_bouldin_score,
    adjusted_rand_score,
)
from sklearn.mixture import GaussianMixture
import joblib

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")
print("Current working directory:", os.getcwd())

SEED = 30
np.random.seed(SEED)

SCALED_DATA_PATH = (
    "data/clustering_data_clean/"
    "GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included_scaled.parquet"
)
LATENT_DIR = "clustering_models/latent_representation"
RESULTS_DIR = "data/clustering_results"

DEC_K4_PATH = os.path.join(RESULTS_DIR, "dec_clusters_k4.csv")
REGION_PATH = "data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv"

N_RUNS = 30          # must match Script 1
K = 4                # we compare to dec_clusters_k4.csv
LAMBDA_PENALTY = 3.0 # weight for Africa-in-Europe penalty

COV_TYPES = ["full", "diag", "tied"]
REG_COVARS = [1e-6, 1e-5, 1e-4]
TEMPS = [1.0, 1.5, 2.0, 3.0]

os.makedirs(RESULTS_DIR, exist_ok=True)

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

def load_scaled_features():
    df = pd.read_parquet(SCALED_DATA_PATH)
    variables = [c for c in df.columns if c != "GHS_urban_area_id"]
    ghs_ids = df["GHS_urban_area_id"].values
    X_features = df[variables].values
    return df, variables, ghs_ids, X_features


def load_mean_embedding(ghs_ids):
    """
    Load latent_run_{run_id}.pkl for run_id in [0, N_RUNS),
    align by GHS_urban_area_id, and return the mean embedding.

    Output:
        Z_mean: (n_samples, latent_dim)
    """
    latent_list = []
    n_samples = len(ghs_ids)

    for run_id in range(N_RUNS):
        latent_path = os.path.join(LATENT_DIR, f"latent_run_{run_id}.pkl")
        if not os.path.exists(latent_path):
            raise FileNotFoundError(f"Missing latent file for run_id {run_id}: {latent_path}")

        df_lat = joblib.load(latent_path)
        df_lat = df_lat.set_index("GHS_urban_area_id").reindex(ghs_ids)

        latent_cols = [c for c in df_lat.columns if c.startswith("latent_")]
        Z = df_lat[latent_cols].values

        if Z.shape[0] != n_samples:
            raise ValueError(f"Run {run_id}: latent size {Z.shape[0]} != n_samples {n_samples}")

        latent_list.append(Z)

    Z_mean = np.mean(latent_list, axis=0)
    print("Mean embedding shape:", Z_mean.shape)
    return Z_mean


def soften_probs(probs, T=2.0):
    logits = np.log(probs + 1e-12)
    scaled = logits / T
    scaled = np.exp(scaled)
    scaled /= scaled.sum(axis=1, keepdims=True)
    return scaled


def compute_representative_scores(X_features, assigned_clusters, n_clusters, ghs_ids):
    """
    Representativeness score based on original scaled features.
    Lower 'similarity' = more representative.
    """
    feature_matrix = normalize(X_features, norm="l2")
    rep_records = []

    for c in range(n_clusters):
        idx = np.where(assigned_clusters == c)[0]
        if len(idx) == 0:
            continue
        cluster_features = feature_matrix[idx]
        dists = pairwise_distances(cluster_features)
        avg_dists = dists.mean(axis=1)
        for local_i, global_i in enumerate(idx):
            rep_records.append({
                "GHS_urban_area_id": ghs_ids[global_i],
                "similarity": avg_dists[local_i]
            })

    return pd.DataFrame(rep_records)


def load_dec_and_regions(ghs_ids):
    """
    Load DEC consensus (k=4) and IPCC regions, aligned to ghs_ids order.
    Returns:
        dec_labels: np.array of consensus_label_majority
        regions: pd.Series of Region aligned with ghs_ids
    """
    # DEC consensus
    dec_df = pd.read_csv(DEC_K4_PATH)
    dec_df = dec_df.set_index("GHS_urban_area_id").reindex(ghs_ids)
    dec_labels = dec_df["consensus_label_majority"].values

    # Regions
    reg_df = pd.read_csv(REGION_PATH)
    reg_df = reg_df[["ID_UC_G0", "Region"]].copy()
    reg_df = reg_df.rename(columns={"ID_UC_G0": "GHS_urban_area_id"})
    reg_df = reg_df.set_index("GHS_urban_area_id").reindex(ghs_ids)

    regions = reg_df["Region"]
    return dec_labels, regions


def compute_africa_in_europe_penalty(labels, soft_probs, regions, n_clusters=4):
    """
    Option B penalty:
    - For each cluster c:
        * determine majority region (by hard labels)
    - Let 'European-dominated clusters' be those whose majority region == 'Europe'
    - Penalty = average soft membership of AFRICAN cities in all European-dominated clusters:
        penalty = (1 / N_Africa) * sum_{i in Africa} sum_{c in EU_dom} soft_probs[i,c]
    """
    # Normalize Region labels a bit
    reg_clean = regions.astype(str).str.strip()
    # Filter masks
    africa_mask = reg_clean.str.contains("Africa", case=False, na=False)
    europe_mask = reg_clean.str.contains("Europe", case=False, na=False)

    n_africa = africa_mask.sum()
    if n_africa == 0:
        return 0.0  # nothing to penalize

    # Determine majority region per cluster based on hard labels
    EU_dominated_clusters = []
    for c in range(n_clusters):
        idx = np.where(labels == c)[0]
        if len(idx) == 0:
            continue
        cluster_regions = reg_clean.iloc[idx]
        # counts by region (including all, we only care who wins)
        counts = cluster_regions.value_counts()
        if counts.empty:
            continue
        majority_region = counts.idxmax()
        if "europe" in majority_region.lower():
            EU_dominated_clusters.append(c)

    if not EU_dominated_clusters:
        return 0.0

    # Sum membership of African cities into EU-dominated clusters
    probs_africa = soft_probs[africa_mask.values]  # shape (N_Africa, K)
    col_idx = np.array(EU_dominated_clusters, dtype=int)
    penalty_mass = probs_africa[:, col_idx].sum()  # total prob mass

    penalty = penalty_mass / n_africa
    return float(penalty)


# ------------------------------------------------------------------------------
# MAIN HYPERPARAMETER SEARCH
# ------------------------------------------------------------------------------

if __name__ == "__main__":
    # 1. Load data
    scaled_df, feature_vars, ghs_ids, X_features = load_scaled_features()
    Z = load_mean_embedding(ghs_ids)
    dec_labels, regions = load_dec_and_regions(ghs_ids)

    all_results = []
    best_config = None
    best_score = -np.inf

    for cov_type in COV_TYPES:
        for reg in REG_COVARS:
            for T in TEMPS:
                print(f"\n=== Trying GMM: cov={cov_type}, reg_covar={reg}, T={T} ===")

                gmm = GaussianMixture(
                    n_components=K,
                    covariance_type=cov_type,
                    reg_covar=reg,
                    random_state=SEED
                )
                gmm.fit(Z)

                raw_probs = gmm.predict_proba(Z)
                soft_probs = soften_probs(raw_probs, T=T)
                labels = soft_probs.argmax(axis=1)

                # Metrics
                ari = adjusted_rand_score(dec_labels, labels)
                sil = silhouette_score(Z, labels)
                cal = calinski_harabasz_score(Z, labels)
                dav = davies_bouldin_score(Z, labels)

                penalty = compute_africa_in_europe_penalty(labels, soft_probs, regions, n_clusters=K)
                combined = ari - LAMBDA_PENALTY * penalty

                print(f"ARI vs DEC: {ari:.4f}, penalty(Africa->EU): {penalty:.4f}, "
                      f"combined: {combined:.4f}, sil={sil:.4f}")

                result_row = {
                    "covariance_type": cov_type,
                    "reg_covar": reg,
                    "T": T,
                    "ARI_DEC": ari,
                    "penalty_africa_in_eu": penalty,
                    "combined_score": combined,
                    "silhouette": sil,
                    "calinski": cal,
                    "davies": dav,
                }
                all_results.append(result_row)

                if combined > best_score:
                    best_score = combined
                    best_config = {
                        "covariance_type": cov_type,
                        "reg_covar": reg,
                        "T": T,
                        "ARI_DEC": ari,
                        "penalty": penalty,
                        "silhouette": sil,
                        "calinski": cal,
                        "davies": dav,
                    }

                gc.collect()

    # Save search summary
    search_df = pd.DataFrame(all_results)
    search_out = os.path.join(RESULTS_DIR, "gmm_hparam_search_k4.csv")
    search_df.to_csv(search_out, index=False)
    print("\nSaved hyperparameter search results to:", search_out)

    print("\nBest configuration:")
    print(best_config)

    # ------------------------------------------------------------------------------
    # RE-FIT BEST CONFIG AND WRITE STANDARD OUTPUT FILES
    # ------------------------------------------------------------------------------

    cov_type_best = best_config["covariance_type"]
    reg_best = best_config["reg_covar"]
    T_best = best_config["T"]

    print(f"\nRefitting best GMM with cov={cov_type_best}, reg_covar={reg_best}, T={T_best}")

    gmm_best = GaussianMixture(
        n_components=K,
        covariance_type=cov_type_best,
        reg_covar=reg_best,
        random_state=SEED
    )
    gmm_best.fit(Z)
    raw_probs_best = gmm_best.predict_proba(Z)
    soft_probs_best = soften_probs(raw_probs_best, T=T_best)
    labels_best = soft_probs_best.argmax(axis=1)

    # Save soft assignments
    soft_df = pd.DataFrame(soft_probs_best, columns=[f"cluster_{i}_prob" for i in range(K)])
    soft_df["GHS_urban_area_id"] = ghs_ids
    soft_out = os.path.join(RESULTS_DIR, "gmm_soft_assignments_k4.csv")
    soft_df.to_csv(soft_out, index=False)
    print("Wrote best soft probabilities to:", soft_out)

    # Entropy
    eps = 1e-12
    ent = -np.sum(soft_probs_best * np.log(soft_probs_best + eps), axis=1)

    # Representativeness (original features)
    rep_df = compute_representative_scores(X_features, labels_best, K, ghs_ids)

    cluster_df = pd.DataFrame({
        "GHS_urban_area_id": ghs_ids,
        "gmm_label": labels_best,
        "entropy": ent
    })
    for i in range(K):
        cluster_df[f"prob_cluster_{i}"] = soft_probs_best[:, i]

    final_df = cluster_df.merge(rep_df, on="GHS_urban_area_id", how="left")
    out_path = os.path.join(RESULTS_DIR, "gmm_clusters_k4.csv")
    final_df.to_csv(out_path, index=False)
    print("Wrote best city-level GMM clusters to:", out_path)

    print("\nDone. Best combined_score =", best_score)
