import os
import random
from collections import defaultdict, deque
import numpy as np
import pandas as pd
from sklearn.impute import SimpleImputer
from scipy.stats import entropy
from scipy.optimize import linear_sum_assignment
import geopandas as gpd
from sklearn.preprocessing import StandardScaler, normalize
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.metrics import silhouette_score, calinski_harabasz_score, davies_bouldin_score, adjusted_rand_score, pairwise_distances
import tensorflow as tf
from tensorflow.keras import layers, models, callbacks, backend as K
from tensorflow.keras.layers import Layer
from tensorflow.keras.regularizers import l2
from tensorflow.keras import mixed_precision
import keras_tuner as kt
import joblib
import pathlib
from tensorflow.keras import backend as K
import gc

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

print("Current working directory:", os.getcwd())

print("CUDA_VISIBLE_DEVICES =", os.environ.get("CUDA_VISIBLE_DEVICES"))
print("All physical devices:", tf.config.list_physical_devices())
print("GPUs available:", tf.config.list_physical_devices('GPU'))

# Enable GPU if available (or disable explicitly here if you want)
physical_devices = tf.config.list_physical_devices('GPU')
if physical_devices:
    try:
        for gpu in physical_devices:
            tf.config.experimental.set_memory_growth(gpu, True)
        print("Using GPU")
    except RuntimeError as e:
        print(f"GPU setup failed: {e}")
else:
    os.environ["OMP_NUM_THREADS"] = '20'
    print("GPU not found, using CPU")

seed = 50
os.environ['PYTHONHASHSEED'] = str(seed)
random.seed(seed)
np.random.seed(seed)
tf.random.set_seed(seed)

# Load additional socioeconomic and exposure data
def load_data(file_name, value_col, new_name):
    df = pd.read_csv(f"data/GHS_UCDB_GLOBE_R2024A_V1_0/{file_name}")
    df = df[['ID_UC_G0', value_col]].copy()
    df[value_col] = pd.to_numeric(df[value_col], errors='coerce')
    df = df.rename(columns={value_col: new_name})
    return df

# Load datasets with custom names
gender = load_data("socioeconomic.csv", "SC_SEC_GDF_2020", "GHS_female_gender_index")
hdi = load_data("socioeconomic.csv", "SC_SEC_HDI_2020", "GHS_HDI")
old_pop = load_data("socioeconomic.csv", "SC_SEC_PCO_2020", "GHS_old_pop")
young_pop = load_data("socioeconomic.csv", "SC_SEC_PCY_2020", "GHS_young_pop")
land_cons = load_data("land_cons.csv", "SD_LUE_LPR_2020_2030", "GHS_land_cons")
road_len = load_data("infrastructures.csv", "IN_ROA_DEN_2024", "GHS_road_len")
hosp_pc = load_data("health.csv", "HL_FPC_HOS_2025", "GHS_hosp_pc")
print(old_pop)
print(hdi)

old_pop["GHS_old_pop"] = old_pop["GHS_old_pop"]/young_pop["GHS_young_pop"]

emissions = pd.read_csv("data/emissions/balance_sheet.csv")
emissions = emissions[emissions['Year'] == 2022]
emissions_subset = emissions[['ID_UC_G0', 'ODIAC']].copy()

cities_clean = pd.read_parquet('data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet', engine='pyarrow')

cities_clean = (
    cities_clean
        .merge(gender, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
        .merge(hdi, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
        .merge(land_cons, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
        .merge(old_pop, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
        .merge(emissions_subset, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
        .merge(road_len, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'])
)


cities_clean['odiac_norm'] = cities_clean['ODIAC'] / cities_clean['GHS_population']

variables = [
    'GHS_population', 'GHS_population_growth',
    'GHS_population_density', 'GHS_population_density_growth',
    'GHS_GDP_PPP', 'GHS_GDP_PPP_growth', 'GHS_critical_infra',
    # 'GHS_greenness_index',
    # 'GHS_precipitation',
    'hdd', 'cdd',
    # 'odiac_norm',
    'GHS_HDI', 'GHS_female_gender_index',
    # "GHS_land_cons",
    "GHS_old_pop", # "GHS_road_len"
    # "GHS_hosp_pc"
]

cities_clean_sub = cities_clean[variables + ['GHS_urban_area_id']].copy()

cities_clean_sub.to_parquet('data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet', engine='pyarrow')


# Check for missing values BEFORE imputation
print("\n=== Missing values BEFORE imputation ===")
missing_before = cities_clean_sub[variables].isnull().sum()
print(missing_before[missing_before > 0])
print(f"Total missing values: {cities_clean_sub[variables].isnull().sum().sum()}")

from sklearn.impute import KNNImputer

# Uses nearest neighbors to impute missing values based on similar observations
print("\n=== Performing KNN Imputation ===")
knn_imputer = KNNImputer(n_neighbors=5, weights='distance')
cities_clean_sub[variables] = knn_imputer.fit_transform(cities_clean_sub[variables])

# Check for missing values AFTER imputation
print("\n=== Missing values AFTER imputation ===")
missing_after = cities_clean_sub[variables].isnull().sum()
print(f"Total missing values: {missing_after.sum()}")
print(f"Any NaN remaining: {cities_clean_sub[variables].isnull().any().any()}")

# Scale data
scaler = StandardScaler()
cities_clean_scaled = scaler.fit_transform(cities_clean_sub[variables])

# Convert the scaled array back to a DataFrame with original column names
cities_clean_scaled_df = pd.DataFrame(cities_clean_scaled, columns=variables)

# Attach the 'GHS_urban_area_id' column back to the scaled DataFrame
cities_clean_scaled_df['GHS_urban_area_id'] = cities_clean_sub['GHS_urban_area_id'].values

# Display descriptive statistics of the scaled data (excluding the ID column)
print(cities_clean_scaled_df.describe())

cities_clean_scaled_df.to_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included_scaled.parquet")

print(np.isnan(cities_clean_scaled).any())

del cities_clean
gc.collect()

class ClusteringLayer(Layer):
    '''
    Clustering layer that computes soft assignments of input vectors to clusters
    using the Student's t-distribution, as introduced in DEC (Xie et al., 2016).

    Outputs a (batch_size, n_clusters) tensor where each row sums to 1.
    '''

    def __init__(self, n_clusters, **kwargs):
        '''
        :param n_clusters: Number of clusters to form.
        :param kwargs: Additional layer keyword arguments.
        '''
        super(ClusteringLayer, self).__init__(**kwargs)
        self.n_clusters = n_clusters
        self._initial_weights = None

    def build(self, input_shape):
        '''
        Initializes the cluster centroids as trainable weights of shape (n_clusters, input_dim).
        '''
        input_dim = input_shape[-1]
        self.clusters = self.add_weight(
            shape=(self.n_clusters, input_dim),
            initializer='glorot_uniform',
            name='clusters',
            trainable=True
        )

        if self._initial_weights is not None:
            self.set_weights(self._initial_weights)
            self._initial_weights = None

        super().build(input_shape)

    def call(self, inputs):
        '''
        Computes soft assignments (q_ij) using the Student's t-distribution, see eq 1 in the manuscript

        Returns:
            q: Tensor of shape (batch_size, n_clusters), soft assignments.
        '''
        alpha = 1.0

        # Compute squared distances: ||z_i - μ_j||^2
        dist = tf.reduce_sum(tf.square(tf.expand_dims(inputs, 1) - self.clusters), axis=2)

        # Compute Student's t-distribution similarity
        q = tf.pow(1.0 + dist / alpha, -(alpha + 1) / 2)

        # Normalize across clusters for each sample
        q = q / tf.reduce_sum(q, axis=1, keepdims=True)

        return q

    def set_initial_weights(self, weights):
        self._initial_weights = [weights]


def target_distribution(q, temperature=.5):
    weight = q ** 2 / tf.reduce_sum(q, axis=0)
    weight = tf.transpose(tf.transpose(weight) / tf.reduce_sum(weight, axis=1))

    # Optional temperature scaling:
    return tf.nn.softmax(tf.math.log(weight + 1e-10) / temperature, axis=1)


def build_autoencoder_fixed(encoding_dim=3, l2_reg=0.001, units1=64, units2=24):
    """
    Builds autoencoder with fixed hyperparameters (no search).
    Replace the default values with your best hyperparameters.
    """
    input_dim = cities_clean_scaled.shape[1]
    reg = l2(l2_reg)

    input_layer = layers.Input(shape=(input_dim,))
    x = layers.Dense(units1, activation='relu', kernel_regularizer=reg)(input_layer)
    x = layers.Dense(units2, activation='relu', kernel_regularizer=reg)(x)
    encoded = layers.Dense(encoding_dim, activation='relu', name='encoder_output')(x)

    x = layers.Dense(units2, activation='relu', kernel_regularizer=reg)(encoded)
    x = layers.Dense(units1, activation='relu', kernel_regularizer=reg)(x)
    decoded = layers.Dense(input_dim, activation='linear')(x)

    autoencoder = models.Model(inputs=input_layer, outputs=decoded)
    autoencoder.compile(optimizer='adam', loss='mse')

    return autoencoder


def build_DEC_model(encoder_model, n_clusters, initial_centers=None):
    input_layer = encoder_model.input
    encoded_output = encoder_model.output
    clustering_layer = ClusteringLayer(n_clusters=n_clusters, name='clustering')
    if initial_centers is not None:
        clustering_layer.set_initial_weights(initial_centers)
    clustering_output = clustering_layer(encoded_output)
    dec_model = models.Model(inputs=input_layer, outputs=clustering_output)

    return dec_model


def compute_avg_cluster_dist(X_latent, centroids, assignments):
    distances = []
    for k in range(centroids.shape[0]):
        cluster_points = X_latent[assignments == k]
        if len(cluster_points) == 0:
            continue
        dists = np.linalg.norm(cluster_points - centroids[k], axis=1)
        distances.extend(dists)
    return np.mean(distances)


def train_autoencoder(run_id, model_dir='clustering_models/models'):
    seed_run = seed + run_id * 100
    np.random.seed(seed_run)
    random.seed(seed_run)
    tf.random.set_seed(seed_run)

    # Use fixed architecture (FAST!)
    autoencoder = build_autoencoder_fixed(
        encoding_dim=4,    # best values
        l2_reg=5.086267866771555e-06,      # best values
        units1=80,         # best values
        units2=16          # best values
    )

    # Train with early stopping
    autoencoder.fit(
        cities_clean_scaled, cities_clean_scaled,
        epochs=100,
        batch_size=256,
        validation_split=0.2,
        callbacks=[callbacks.EarlyStopping(monitor='val_loss', patience=15, restore_best_weights=True)],
        verbose=0
    )

    # Extract encoder
    encoder_output = autoencoder.get_layer('encoder_output').output
    encoder_model = models.Model(inputs=autoencoder.input, outputs=encoder_output)

    del autoencoder
    gc.collect()

    return encoder_model


def get_embeddings(encoder_model, run_id, latent_dir='clustering_models/latent_representation'):
    # Check if embeddings already exist
    latent_path = os.path.join(latent_dir, f'latent_run_{run_id}.pkl')
    
    if os.path.exists(latent_path):
        print(f"Loading existing latent representation from {latent_path}")
        latent_df = joblib.load(latent_path)
        # Extract just the embedding columns (exclude GHS_urban_area_id)
        embedding_cols = [col for col in latent_df.columns if col.startswith('latent_')]
        embeddings = latent_df[embedding_cols].values
        return embeddings

    # Compute embeddings if they don't exist
    embeddings = encoder_model.predict(cities_clean_scaled, batch_size=2048, verbose=0)

    latent_df = pd.DataFrame(embeddings, columns=[f"latent_{i}" for i in range(embeddings.shape[1])])
    latent_df["GHS_urban_area_id"] = cities_clean_sub["GHS_urban_area_id"].values
    joblib.dump(latent_df, latent_path)
    print(f"Saved latent representation to {latent_path}")

    return embeddings


def run_kmeans_clustering(embeddings, n_clusters, run_id, seed):
    seed_run = seed + run_id * 100

    kmeans = KMeans(n_clusters=n_clusters, n_init=20, random_state=seed_run)
    labels = kmeans.fit_predict(embeddings)

    scores = {
        'silhouette': silhouette_score(embeddings, labels),
        'calinski': calinski_harabasz_score(embeddings, labels),
        'davies': davies_bouldin_score(embeddings, labels)
    }

    return labels, kmeans.cluster_centers_, scores


def run_hierarchical_clustering(embeddings, n_clusters):
    clustering = AgglomerativeClustering(n_clusters=n_clusters)
    labels = clustering.fit_predict(embeddings)

    scores = {
        'silhouette': silhouette_score(embeddings, labels),
        'calinski': calinski_harabasz_score(embeddings, labels),
        'davies': davies_bouldin_score(embeddings, labels)
    }

    return labels, scores


def run_dec_clustering(encoder_model, n_clusters, initial_centers, run_id):
    dec_model = build_DEC_model(encoder_model, n_clusters, initial_centers)
    dec_model.compile(
        optimizer=tf.keras.optimizers.Adam(learning_rate=1e-3),
        loss=lambda y_true, y_pred: tf.keras.losses.KLD(y_true, y_pred)
    )

    # Initial soft assignments and predicted labels
    q = dec_model(cities_clean_scaled, training=False).numpy()
    y_pred_last = q.argmax(axis=1)

    # Hyperparameters
    maxiter = 500
    update_interval = 30
    tol_silhouette = 1e-3
    tol_label = 5e-3
    tol_distance = 1e-3
    window = 10
    min_iter = 20
    sample_size = 5000
    patience = 5

    # Tracking best model
    best_silhouette = -1.0
    best_model_weights = None
    no_improve_count = 0

    # Histories for convergence monitoring
    silhouette_history = deque(maxlen=window)
    label_delta_history = deque(maxlen=window)
    avg_dist_history = deque(maxlen=window)

    @tf.function
    def dec_forward(x):
        return dec_model(x, training=False)

    def compute_avg_cluster_dist_local(embeddings, centroids, labels):
        distances = np.linalg.norm(embeddings - centroids[labels], axis=1)
        return distances.mean()

    for ite in range(maxiter):
        # Forward pass
        q = dec_forward(cities_clean_scaled).numpy()
        p = target_distribution(q)
        y_pred = q.argmax(axis=1)

        # Label change
        delta_label = np.mean(y_pred != y_pred_last)
        y_pred_last = y_pred

        # Update DEC model
        dec_model.train_on_batch(cities_clean_scaled, p)

        if ite % update_interval == 0:
            # Embeddings for metrics
            embeddings = encoder_model.predict(cities_clean_scaled, batch_size=2048, verbose=0)
            if len(embeddings) > sample_size:
                idx = np.random.choice(len(embeddings), size=sample_size, replace=False)
                sil = silhouette_score(embeddings[idx], y_pred[idx])
            else:
                sil = silhouette_score(embeddings, y_pred)

            # Compute extra metrics
            centroids = dec_model.get_layer("clustering").get_weights()[0]
            avg_dist = compute_avg_cluster_dist_local(embeddings, centroids, y_pred)

            # Update histories
            silhouette_history.append(sil)
            label_delta_history.append(delta_label)
            avg_dist_history.append(avg_dist)

            print(f"[{ite}] Silhouette: {sil:.4f}, ΔLabel: {delta_label:.4f}, AvgDist: {avg_dist:.4f}")

            # Track best model
            if sil > best_silhouette + 1e-4:
                best_silhouette = sil
                best_model_weights = dec_model.get_weights()
                no_improve_count = 0
            else:
                no_improve_count += 1

        # Stop criteria
        if ite >= min_iter and len(silhouette_history) == window:
            avg_sil_change = np.mean(np.abs(np.diff(silhouette_history)))
            avg_label_delta = np.mean(label_delta_history)
            avg_dist_change = np.mean(np.abs(np.diff(avg_dist_history)))

            if (avg_sil_change < tol_silhouette and
                avg_label_delta < tol_label and
                avg_dist_change < tol_distance):
                print(f"DEC converged (stable metrics) at iter {ite}")
                break

        if no_improve_count >= patience:
            print(f"No silhouette improvement for {patience} checks. Stopping at iter {ite}.")
            break

    # Restore best weights
    if best_model_weights:
        dec_model.set_weights(best_model_weights)
        print(f"Restored best model with silhouette: {best_silhouette:.4f}")

    # Final embeddings and soft assignments
    embeddings = encoder_model.predict(cities_clean_scaled, batch_size=2048, verbose=0)
    q_final = dec_model.predict(cities_clean_scaled, batch_size=2048, verbose=0)

    # Save soft assignments to CSV
    soft_df = pd.DataFrame(q_final, columns=[f"cluster_{i}_prob" for i in range(n_clusters)])
    soft_df['GHS_urban_area_id'] = cities_clean_sub['GHS_urban_area_id'].values
    soft_df.to_csv(f"data/clustering_results/dec_soft_assignments_run{run_id}_clusters{n_clusters}.csv", index=False)
    print(f"Saved soft cluster assignments")

    # Final hard cluster labels
    final_labels = q_final.argmax(axis=1)

    # Compute clustering metrics on full data
    scores = {
        'silhouette': float(silhouette_score(embeddings, final_labels)),
        'calinski': float(calinski_harabasz_score(embeddings, final_labels)),
        'davies': float(davies_bouldin_score(embeddings, final_labels))
    }

    # Clean up
    del dec_model
    del q_final
    del embeddings
    K.clear_session()
    gc.collect()

    return final_labels, scores


def train_run(run_id, n_clusters, cities_clean_scaled):

    # FULL RESET for each (run_id, k)
    K.clear_session()
    gc.collect()

    # Train AE fresh for each k
    encoder_model = train_autoencoder(run_id)

    # Compute embeddings fresh for each k
    embeddings = get_embeddings(encoder_model, run_id)

    # Simple K-Means
    kmeans_labels, kmeans_centers, kmeans_scores = run_kmeans_clustering(
        cities_clean_scaled, n_clusters, run_id, seed
    )

    # Hierarchical
    hierarchical_labels, hierarchical_scores = run_hierarchical_clustering(
        cities_clean_scaled, n_clusters
    )

    # Embedded K-Means
    kmeans_emb_labels, kmeans_emb_centers, kmeans_emb_scores = run_kmeans_clustering(
        embeddings, n_clusters, run_id, seed
    )

    # DEC
    dec_labels, dec_scores = run_dec_clustering(
        encoder_model, n_clusters, kmeans_emb_centers, run_id
    )

    # IMPORTANT: return ONLY a dictionary, nothing else
    return {
        "run_id": run_id,
        "n_clusters": n_clusters,
        "kmeans simple": {
            "labels": kmeans_labels,
            "scores": kmeans_scores,
        },
        "hierarchical simple": {
            "labels": hierarchical_labels,
            "scores": hierarchical_scores,
        },
        "kmeans embedded": {
            "labels": kmeans_emb_labels,
            "scores": kmeans_emb_scores,
        },
        "dec": {
            "labels": dec_labels,
            "scores": dec_scores,
        },
    }




def run_experiments(cluster_range, n_runs):
    all_results = []

    for run_id in range(n_runs):
        print(f"\n===== Processing run_id {run_id} =====")

        for n_clusters in cluster_range:
            print(f"  Clustering with {n_clusters} clusters")

            # IMPORTANT: train_run receives NO encoder_model or embeddings
            result = train_run(run_id, n_clusters, cities_clean_scaled)

            all_results.append(result)

            print(f"  KMeans silhouette: {result['kmeans embedded']['scores']['silhouette']:.4f}")
            print(f"  DEC silhouette: {result['dec']['scores']['silhouette']:.4f}")

        # Clear TF session after all k for this run_id
        K.clear_session()
        gc.collect()

    return all_results


def flatten_performance_scores(results):
    flat_rows = []

    for entry in results:
        run_id = entry['run_id']
        n_clusters = entry['n_clusters']

        for method in ['kmeans simple', 'hierarchical simple', 'kmeans embedded', 'dec']:
            labels = entry[method]['labels']
            scores = entry[method]['scores']

            row = {
                'run_id': run_id,
                'n_clusters': n_clusters,
                'method': method,
                'silhouette': float(scores['silhouette']),
                'calinski': float(scores['calinski']),
                'davies': float(scores['davies']),
                'labels': list(labels)
            }

            flat_rows.append(row)

    # Convert to DataFrame
    df_scores = pd.DataFrame(flat_rows)

    # Convert labels list to a string
    df_scores['labels'] = df_scores['labels'].apply(lambda x: ','.join(map(str, x)))

    return df_scores


if __name__ == '__main__':
    cluster_range = [4, 3, 5, 6, 7, 8, 9, 10, 11]
    n_runs = 50

    performance_scores = run_experiments(cluster_range, n_runs)

    performance_scores_df = flatten_performance_scores(performance_scores)

    performance_scores_df.to_csv("data/clustering_results/raw_clustering_scores.csv", index=False)

    print("All models trained and saved successfully.")
