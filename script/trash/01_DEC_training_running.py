import os
import random
from collections import defaultdict, deque
import numpy as np
import pandas as pd
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
from joblib import Parallel, delayed
import pathlib
import multiprocessing



seed = 50
os.environ['PYTHONHASHSEED'] = str(seed)
random.seed(seed)
np.random.seed(seed)
tf.random.set_seed(seed)
'''
os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

'''
# set directory
base_dir = os.environ.get("BASE_DIR")
if base_dir is None:
    raise EnvironmentError("BASE_DIR environment variable is not set.")
else:
    os.chdir(base_dir)
    print(f"changed directory to the parent folder of the repo")



def configure_device(use_gpu=True, cpu_threads=4):
    """
    Configure TensorFlow to use GPU or CPU with thread control.

    Args:
        use_gpu (bool): If True, enable GPU (if available). If False, force CPU.
        cpu_threads (int): Number of threads to use on CPU when GPU is disabled.
    """
    print("Current working directory:", os.getcwd())
    print("Initial CUDA_VISIBLE_DEVICES =", os.environ.get("CUDA_VISIBLE_DEVICES"))

    if not use_gpu:
        # Force CPU mode
        os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
        print("GPU disabled, forcing CPU mode")
    else:
        print("GPU mode requested")

    # Check available devices
    all_devices = tf.config.list_physical_devices()
    gpu_devices = tf.config.list_physical_devices('GPU')
    print("All physical devices:", all_devices)
    print("GPUs available:", gpu_devices)

    if use_gpu and gpu_devices:
        try:
            # Enable dynamic memory growth for each GPU
            for gpu in gpu_devices:
                tf.config.experimental.set_memory_growth(gpu, True)
            print("Using GPU with memory growth enabled")
        except RuntimeError as e:
            print(f"GPU setup failed: {e}. Falling back to CPU.")
            os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
    else:
        # Configure TensorFlow threading for CPU
        tf.config.threading.set_intra_op_parallelism_threads(cpu_threads)
        tf.config.threading.set_inter_op_parallelism_threads(cpu_threads)
        os.environ["OMP_NUM_THREADS"] = str(cpu_threads)
        print(f"Using CPU with {cpu_threads} threads")


# Force CPU
configure_device(use_gpu=False, cpu_threads=6)

# === Load and preprocess data ===
cities_clean = pd.read_parquet('data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation.parquet', engine='pyarrow')
ghsl = gpd.read_file("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")

# Add precipitation data
climate_df = pd.read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_THEME_CLIMATE_GLOBE_R2024A.csv")
climate_df = climate_df[['ID_UC_G0', 'CL_B12_CUR_2010']]
cities_clean = cities_clean.merge(climate_df, left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
cities_clean.drop(columns='ID_UC_G0', inplace=True)

# Add continent dummies
for continent in ['North America', 'South America', 'Europe', 'Africa', 'Asia']:
    cities_clean[continent] = (cities_clean['continent'] == continent).astype(int)
cities_clean['Oceania'] = cities_clean['continent'].isin(['Oceania', 'Australia']).astype(int)

variables = [
    'GHS_population', 'GHS_population_growth',
    'GHS_population_density', 'GHS_population_density_growth',
    'GHS_GDP_PPP', 'GHS_GDP_PPP_growth',
    "CL_B12_CUR_2010", 'hdd', 'cdd'
]

cities_clean_sub = cities_clean[variables + ['GHS_urban_area_id']].copy()

# Scale data
scaler = StandardScaler()
cities_clean_scaled = scaler.fit_transform(cities_clean_sub[variables])

# Convert the scaled array back to a DataFrame with original column names
cities_clean_scaled_df = pd.DataFrame(cities_clean_scaled, columns=variables)

# Attach the 'GHS_urban_area_id' column back to the scaled DataFrame
cities_clean_scaled_df['GHS_urban_area_id'] = cities_clean_sub['GHS_urban_area_id'].values

# Display descriptive statistics of the scaled data (excluding the ID column)
print(cities_clean_scaled_df.describe())

cities_clean_scaled_df.to_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_scaled.parquet")

class ClusteringLayer(Layer):
    '''
    Clustering layer that computes soft assignments of input vectors to clusters
    using the Student’s t-distribution, as introduced in DEC (Xie et al., 2016).

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
        Computes soft assignments (q_ij) using the Student’s t-distribution, see eq 1 in the manuscript

        Returns:
            q: Tensor of shape (batch_size, n_clusters), soft assignments.
        '''
        alpha = 1.0

        # Compute squared distances: ||z_i - μ_j||^2
        dist = tf.reduce_sum(tf.square(tf.expand_dims(inputs, 1) - self.clusters), axis=2)

        # Compute Student’s t-distribution similarity
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


def build_autoencoder(hp):
    """
    Builds a simple fully connected autoencoder model and defines its hyperparameter search space.

    We performed a hyperparameter search over:
    - the number of hidden units in two intermediate layers (16–98 units),
    - the latent dimensionality (2–3),
    - and the L2 regularization strength (ranging from 1e⁻⁶ to 1e⁻²).

    The encoder consists of two ReLU-activated dense layers followed by a bottleneck (latent) layer.
    The decoder mirrors the encoder structure to reconstruct the input.
    """
    input_dim = cities_clean_scaled.shape[1]
    encoding_dim = hp.Int('encoding_dim', min_value=2, max_value=4, step=1)
    reg = l2(hp.Float('l2_reg', min_value=1e-6, max_value=1e-2, sampling='log'))

    units1 = hp.Int('units1', 32, 96, step=16)
    units2 = hp.Int('units2', 16, 32, step=8)

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

    tuner = kt.Hyperband(
        build_autoencoder,
        objective='val_loss',
        max_epochs=30,
        factor=3,
        executions_per_trial=2,
        directory=os.path.join('clustering_models', 'hyperband', f'run_{run_id}'),
        project_name='DEC_model_tuning'
    )

    noise_factor = 0.1
    noisy_input = cities_clean_scaled + noise_factor * np.random.normal(0.0, 1.0, cities_clean_scaled.shape)
    noisy_input = np.clip(noisy_input, -3., 3.)

    tuner.search(
        noisy_input, cities_clean_scaled,
        epochs=5,
        batch_size=128,
        validation_split=0.2,
        callbacks=[callbacks.EarlyStopping(monitor='val_loss', patience=10)],
        verbose=0
    )

    best_model = tuner.get_best_models(num_models=1)[0]
    encoder_output = best_model.get_layer('encoder_output').output
    encoder_model = models.Model(inputs=best_model.input, outputs=encoder_output)

    model_path = os.path.join(model_dir, f'model_run_{run_id}.keras')
    encoder_model.save(model_path, include_optimizer=True)
    print(f"Saved model to {model_path}")

    return encoder_model


def get_embeddings(encoder_model, run_id, latent_dir='clustering_models/latent_representation'):

    embeddings = encoder_model.predict(cities_clean_scaled, batch_size=1024)

    latent_path = os.path.join(latent_dir, f'latent_run_{run_id}.pkl')
    latent_df = pd.DataFrame(embeddings, columns=[f"latent_{i}" for i in range(embeddings.shape[1])])
    latent_df["GHS_urban_area_id"] = cities_clean_sub["GHS_urban_area_id"].values
    joblib.dump(latent_df, latent_path)
    print(f"Saved latent representation to {latent_path}")

    return embeddings


def run_kmeans_clustering(embeddings, n_clusters):
    kmeans = KMeans(n_clusters=n_clusters, n_init=20, random_state=seed)
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
    maxiter = 250
    update_interval = 20
    window = 10
    tol_label = 1e-3
    tol_silhouette = 1e-4
    tol_dist_change = 1e-3
    min_iter = 20
    early_stop_dev = 250
    sample_size = 5000  # for faster silhouette computation

    # Histories for convergence monitoring
    silhouette_history = deque(maxlen=window)
    label_delta_history = deque(maxlen=window)
    avg_dist_history = deque(maxlen=window)

    # TensorFlow function for forward pass (faster graph execution)
    @tf.function
    def dec_forward(x):
        return dec_model(x, training=False)

    # Compute average cluster distance (vectorized)
    def compute_avg_cluster_dist(embeddings, centroids, labels):
        distances = np.linalg.norm(embeddings - centroids[labels], axis=1)
        return distances.mean()

    for ite in range(maxiter):
        # Forward pass to get soft assignments
        q = dec_forward(cities_clean_scaled).numpy()
        p = target_distribution(q)
        y_pred = q.argmax(axis=1)

        # Calculate label changes
        delta_label = np.mean(y_pred != y_pred_last)
        y_pred_last = y_pred

        # Update DEC model weights
        dec_model.train_on_batch(cities_clean_scaled, p)

        if ite % update_interval == 0:
            # Get embeddings for metrics
            embeddings = encoder_model.predict(cities_clean_scaled, batch_size=1024, verbose=0)

            # Subsample embeddings for silhouette score if dataset is large
            if len(embeddings) > sample_size:
                idx = np.random.choice(len(embeddings), size=sample_size, replace=False)
                sil = silhouette_score(embeddings[idx], y_pred[idx])
            else:
                sil = silhouette_score(embeddings, y_pred)

            silhouette_history.append(sil)
            label_delta_history.append(delta_label)

            # Compute centroids and average cluster distance
            centroids = dec_model.get_layer("clustering").get_weights()[0]
            avg_dist = compute_avg_cluster_dist(embeddings, centroids, y_pred)
            avg_dist_history.append(avg_dist)

            print(f"[{ite}] Silhouette: {sil:.4f}, ΔLabel: {delta_label:.4f}, AvgDist: {avg_dist:.4f}")

        # Check convergence criteria after min_iter iterations
        if ite >= min_iter and len(silhouette_history) == window:
            sil_change = silhouette_history[-1] - silhouette_history[0]
            dist_change = avg_dist_history[-1] - avg_dist_history[0] if len(avg_dist_history) == window else 0
            if (abs(sil_change) < tol_silhouette and
                delta_label < tol_label and
                abs(dist_change) < tol_dist_change):
                print(f"DEC converged at iter {ite}")
                break

        # Development early stop
        if ite == early_stop_dev:
            print("Early stopping (dev)")
            break

    # Final embeddings and soft assignments
    embeddings = encoder_model.predict(cities_clean_scaled, batch_size=1024, verbose=0)
    q_final = dec_model.predict(cities_clean_scaled, batch_size=1024)

    # Save soft assignments to CSV
    soft_df = pd.DataFrame(q_final, columns=[f"cluster_{i}_prob" for i in range(n_clusters)])
    soft_df['GHS_urban_area_id'] = cities_clean_sub['GHS_urban_area_id'].values
    soft_df.to_csv(f"data/clustering_results/dec_soft_assignments_run{run_id}_clusters{n_clusters}.csv", index=False)
    print(f"Saved soft cluster assignments")

    # Final hard cluster labels
    final_labels = q_final.argmax(axis=1)

    # Compute clustering metrics on full data
    scores = {
        'silhouette': silhouette_score(embeddings, final_labels),
        'calinski': calinski_harabasz_score(embeddings, final_labels),
        'davies': davies_bouldin_score(embeddings, final_labels)
    }

    return final_labels, scores


def train_run(run_id, n_clusters, cities_clean_scaled):
    encoder_model = train_autoencoder(run_id)
    embeddings = get_embeddings(encoder_model, run_id)

    # Simple K-Means
    kmeans_labels, kmeans_centers, kmeans_scores = run_kmeans_clustering(cities_clean_scaled, n_clusters)

    # Simple Hierarchical Clustering
    hierarchical_labels, hierarchical_scores = run_hierarchical_clustering(cities_clean_scaled, n_clusters)

    # Embedded KMeans
    kmeans_emb_labels, kmeans_emb_centers, kmeans_emb_scores = run_kmeans_clustering(embeddings, n_clusters)

    # DEC
    dec_labels, dec_scores = run_dec_clustering(encoder_model, n_clusters, kmeans_emb_centers, run_id)

    return {
        'run_id': run_id,
        'n_clusters': n_clusters,
        'kmeans simple': {
            'labels': kmeans_labels,
            'scores': kmeans_scores
        },
        'hierarchical simple': {
            'labels': hierarchical_labels,
            'scores': hierarchical_scores
        },
        'kmeans embedded': {
            'labels': kmeans_emb_labels,
            'scores': kmeans_emb_scores
        },
        'dec': {
            'labels': dec_labels,
            'scores': dec_scores
        }
    }



def run_experiments(cluster_range, n_runs, cities_clean_scaled):
    """
    Run clustering experiments.
    - If GPU is active: Run sequentially.
    - If GPU is not active: Run in parallel using all CPU cores.
    """
    all_results = []
    tasks = [(n_clusters, run_id) for n_clusters in cluster_range for run_id in range(n_runs)]

    # Check for GPU
    gpus = tf.config.list_physical_devices('GPU')
    gpu_active = len(gpus) > 0

    if gpu_active:
        print("GPU detected: Running experiments sequentially...")
        for (n_clusters, run_id) in tasks:
            result = train_run(run_id, n_clusters, cities_clean_scaled)
            all_results.append(result)
    else:
        num_cores = round(multiprocessing.cpu_count() * 0.8)
        print(f"No GPU detected: Running experiments in parallel on {num_cores} CPU cores...")
        all_results = Parallel(n_jobs=num_cores)(
            delayed(train_run)(run_id, n_clusters, cities_clean_scaled) for (n_clusters, run_id) in tasks
        )

    return all_results


def flatten_performance_scores(results):
    # Assume 'results' is your list of dictionaries
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
                'labels': list(labels)  # store as list; will be stringified in CSV
            }

            flat_rows.append(row)

    # Convert to DataFrame
    df_scores = pd.DataFrame(flat_rows)

    # Optional: convert labels list to a string
    df_scores['labels'] = df_scores['labels'].apply(lambda x: ','.join(map(str, x)))

    return df_scores


if __name__ == '__main__':
    cluster_range = range(3,16)
    n_runs = 350

    performance_scores = run_experiments(cluster_range, n_runs, cities_clean_scaled)

    performance_scores_df = flatten_performance_scores(performance_scores)

    performance_scores_df.to_csv("data/clustering_results/raw_clustering_scores.csv", index=False)

    print("All models trained and saved successfully.")
