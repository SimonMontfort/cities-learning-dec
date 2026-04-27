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

os.chdir("/Users/simon/Documents/repo/cities-learning-dec/robustness/appraisal")

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

# old_pop["GHS_old_pop"] = old_pop["GHS_old_pop"]/young_pop["GHS_young_pop"]

emissions = pd.read_csv("data/emissions/balance_sheet.csv")
emissions = emissions[emissions['Year'] == 2022]
emissions_subset = emissions[['ID_UC_G0', 'ODIAC']].copy()

cities_clean = pd.read_parquet('data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included_urban.parquet', engine='pyarrow')

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
    'GHS_GDP_PPP', 'GHS_GDP_PPP_growth', 'GHS_critical_infra', 'GHS_greenness_index',
    'GHS_precipitation', 'hdd', 'cdd',
    # 'odiac_norm',
    'GHS_HDI', 'GHS_female_gender_index',
    # "GHS_land_cons",
    "GHS_old_pop", # "GHS_road_len"
    # "GHS_hosp_pc"
]

cities_clean_sub = cities_clean[variables + ['GHS_urban_area_id']].copy()

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

# Run hyperparameter search
tuner = kt.Hyperband(
    hypermodel=build_autoencoder,
    objective='val_loss',
    max_epochs=50,
    factor=3,
    executions_per_trial=2,
    directory='clustering_models/hyperband_search',
    project_name='find_best_hyperparameters',
    overwrite=True
)

tuner.search(
    cities_clean_scaled, cities_clean_scaled,
    epochs=50,
    batch_size=128,
    validation_split=0.2,
    callbacks=[callbacks.EarlyStopping(monitor='val_loss', patience=10)],
    verbose=1
)

# Get best hyperparameters
best_hps = tuner.get_best_hyperparameters(num_trials=1)[0]

print("\n" + "="*50)
print("BEST HYPERPARAMETERS:")
print("="*50)
print(f"encoding_dim: {best_hps.get('encoding_dim')}")
print(f"l2_reg: {best_hps.get('l2_reg')}")
print(f"units1: {best_hps.get('units1')}")
print(f"units2: {best_hps.get('units2')}")
print("="*50)

# Save to file
with open('clustering_models/best_hyperparameters.txt', 'w') as f:
    f.write(f"encoding_dim = {best_hps.get('encoding_dim')}\n")
    f.write(f"l2_reg = {best_hps.get('l2_reg')}\n")
    f.write(f"units1 = {best_hps.get('units1')}\n")
    f.write(f"units2 = {best_hps.get('units2')}\n")

print("\nBest hyperparameters saved to best_hyperparameters.txt")
