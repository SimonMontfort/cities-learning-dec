import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import json
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score
from sklearn.manifold import TSNE
from tensorflow import keras

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def plot_latent_space(latent, labels, out_dir, prefix):
    ensure_dir(out_dir)

    # PCA
    pca = PCA(n_components=2)
    latent_pca = pca.fit_transform(latent)
    explained_variance = pca.explained_variance_ratio_.tolist()

    # PCA scatter
    plt.figure(figsize=(8, 6))
    plt.scatter(latent_pca[:, 0], latent_pca[:, 1], c=labels, cmap="tab10", s=10)
    plt.title(f"PCA projection: {prefix}")
    plt.savefig(os.path.join(out_dir, f"{prefix}_pca.png"), dpi=300)
    plt.close()

    # Silhouette score
    try:
        sil_score = silhouette_score(latent, labels)
    except Exception:
        sil_score = None

    # t-SNE
    tsne = TSNE(n_components=2, random_state=42, perplexity=40)
    coords = tsne.fit_transform(latent)

    # Save t-SNE plot
    plt.figure(figsize=(8, 6))
    plt.scatter(coords[:, 0], coords[:, 1], c=labels, cmap="tab10", s=10)
    plt.title(f"t-SNE projection: {prefix}")
    plt.savefig(os.path.join(out_dir, f"{prefix}_tsne.png"), dpi=300)
    plt.close()

    # Save t-SNE coordinates
    tsne_df = pd.DataFrame(coords, columns=["tsne_1", "tsne_2"])
    tsne_df["cluster_label"] = labels
    tsne_df.to_csv(os.path.join(out_dir, f"{prefix}_tsne_coords.csv"), index=False)

    # Save metrics
    metrics = {
        "explained_variance_ratio": [float(x) for x in explained_variance],
        "silhouette_score": float(sil_score) if sil_score is not None else None
    }
    with open(os.path.join(out_dir, f"{prefix}_metrics.json"), "w") as f:
        json.dump(metrics, f, indent=4)

    print(f"Saved PCA, t-SNE, and metrics for {prefix} at {out_dir}")

    return sil_score, explained_variance

if __name__ == "__main__":
    os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

    # Load scaled features once
    print("Loading scaled features...")
    cities_clean_scaled = pd.read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")
    features_cols = ['GHS_population', 'GHS_population_growth', 'GHS_population_density',
                     'GHS_population_density_growth', 'GHS_GDP_PPP', 'GHS_GDP_PPP_growth',
                     'CL_B12_CUR_2010', 'hdd', 'cdd']
    cities_clean_scaled = cities_clean_scaled[features_cols]

    base_model_path = "clustering_models/models"
    base_soft_assign_path = "data/clustering_results"
    base_out_dir = "plots/all_runs_diagnostics"

    summary_records = []

    for run_id in range(50):
        print(f"\nProcessing run {run_id}...")

        model_path = os.path.join(base_model_path, f"model_run_{run_id}.keras")
        soft_assignments_path = os.path.join(base_soft_assign_path, f"dec_soft_assignments_run{run_id}_clusters6.csv")
        out_dir = base_out_dir

        try:
            encoder_model = keras.models.load_model(model_path)
        except Exception as e:
            print(f"Failed to load model for run {run_id}: {e}")
            summary_records.append({
                'run_id': run_id,
                'silhouette_score': None,
                'pca_var_1': None,
                'pca_var_2': None,
                'flag_bad': "model_load_fail"
            })
            continue

        latent = encoder_model.predict(cities_clean_scaled)

        try:
            soft_df = pd.read_csv(soft_assignments_path)
        except Exception as e:
            print(f"Failed to load soft assignments for run {run_id}: {e}")
            summary_records.append({
                'run_id': run_id,
                'silhouette_score': None,
                'pca_var_1': None,
                'pca_var_2': None,
                'flag_bad': "soft_assign_load_fail"
            })
            continue

        cluster_probs = soft_df.filter(like="cluster_").values
        labels = np.argmax(cluster_probs, axis=1)

        sil_score, explained_variance = plot_latent_space(latent, labels, out_dir, prefix=f"dec_run{run_id}")

        # Flag bad runs: silhouette < 0.1 or PCA var sum < 0.15 or silhouette not computable
        flag_bad = None
        if sil_score is None or sil_score < 0.1 or sum(explained_variance[:2]) < 0.15:
            flag_bad = f"silhouette={sil_score}, pca_var_sum={sum(explained_variance[:2]):.3f}"

        summary_records.append({
            'run_id': run_id,
            'silhouette_score': sil_score,
            'pca_var_1': explained_variance[0],
            'pca_var_2': explained_variance[1],
            'flag_bad': flag_bad
        })

    summary_df = pd.DataFrame(summary_records)
    summary_csv_path = os.path.join(base_out_dir, "runs_embedding_quality_summary.csv")
    summary_df.to_csv(summary_csv_path, index=False)

    print(f"\nSummary saved to {summary_csv_path}")

    # Print flagged runs
    bad_runs = summary_df[summary_df['flag_bad'].notna()]
    if not bad_runs.empty:
        print("\nRuns flagged as potentially bad embeddings:")
        for _, row in bad_runs.iterrows():
            print(f"Run {row['run_id']}: {row['flag_bad']}")
    else:
        print("\nNo bad runs detected based on current thresholds.")
