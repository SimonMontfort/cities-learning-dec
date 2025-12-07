#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
01_make_embeddings.py

Step 1 of the pipeline:
- Load & preprocess data
- Train autoencoder for each run_id
- Write latent embeddings as latent_run_{run_id}.pkl

Supports:
    - Tuning via KerasTuner (Hyperband)
    - Manual hyperparameters (fast, deterministic)
"""

import os
import random
import gc
from collections import deque

import numpy as np
import pandas as pd
from sklearn.impute import KNNImputer
from sklearn.preprocessing import StandardScaler

import tensorflow as tf
from tensorflow.keras import layers, models, callbacks
from tensorflow.keras.regularizers import l2
import keras_tuner as kt
import joblib

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")
print("Current working directory:", os.getcwd())

print("CUDA_VISIBLE_DEVICES =", os.environ.get("CUDA_VISIBLE_DEVICES"))
print("All physical devices:", tf.config.list_physical_devices())
print("GPUs available:", tf.config.list_physical_devices('GPU'))

physical_devices = tf.config.list_physical_devices('GPU')
if physical_devices:
    try:
        for gpu in physical_devices:
            tf.config.experimental.set_memory_growth(gpu, True)
        print("Using GPU")
    except RuntimeError as e:
        print(f"GPU setup failed: {e}")
else:
    os.environ["OMP_NUM_THREADS"] = "20"
    print("GPU not found, using CPU")

seed = 50
os.environ["PYTHONHASHSEED"] = str(seed)
random.seed(seed)
np.random.seed(seed)
tf.random.set_seed(seed)

# ----- SWITCH: tuning vs manual hyperparameters -----
USE_TUNING = False  # <-- set True to use KerasTuner, False to use manual HPs

# Manual hyperparameters (only used if USE_TUNING = False)
MANUAL_HPARAMS = dict(
    encoding_dim=4,
    l2_reg=6.086267866771555e-06,
    units1=80,
    units2=16
)

# Number of runs for the embedding ensemble
N_RUNS = 30

# Paths
CLUSTERING_DATA_SCALED_OUT = (
    "data/clustering_data_clean/"
    "GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included_scaled.parquet"
)
LATENT_DIR = "clustering_models/latent_representation"


# ------------------------------------------------------------------------------
# DATA LOADING & PREPROCESSING
# ------------------------------------------------------------------------------

def load_data(file_name, value_col, new_name):
    df = pd.read_csv(f"data/GHS_UCDB_GLOBE_R2024A_V1_0/{file_name}")
    df = df[["ID_UC_G0", value_col]].copy()
    df[value_col] = pd.to_numeric(df[value_col], errors="coerce")
    df = df.rename(columns={value_col: new_name})
    return df


def prepare_data():
    # Socioeconomic / exposure data
    gender = load_data("socioeconomic.csv", "SC_SEC_GDF_2020", "GHS_female_gender_index")
    hdi = load_data("socioeconomic.csv", "SC_SEC_HDI_2020", "GHS_HDI")
    old_pop = load_data("socioeconomic.csv", "SC_SEC_PCO_2020", "GHS_old_pop")
    young_pop = load_data("socioeconomic.csv", "SC_SEC_PCY_2020", "GHS_young_pop")
    land_cons = load_data("land_cons.csv", "SD_LUE_LPR_2020_2030", "GHS_land_cons")
    road_len = load_data("infrastructures.csv", "IN_ROA_DEN_2024", "GHS_road_len")
    hosp_pc = load_data("health.csv", "HL_FPC_HOS_2025", "GHS_hosp_pc")

    # Old / young ratio
    old_pop["GHS_old_pop"] = old_pop["GHS_old_pop"] / young_pop["GHS_young_pop"]

    emissions = pd.read_csv("data/emissions/balance_sheet.csv")
    emissions = emissions[emissions["Year"] == 2022]
    emissions_subset = emissions[["ID_UC_G0", "ODIAC"]].copy()

    cities_clean = pd.read_parquet(
        "data/clustering_data_clean/"
        "GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet",
        engine="pyarrow"
    )

    # Merge auxiliary data
    cities_clean = (
        cities_clean
        .merge(gender, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
        .merge(hdi, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
        .merge(land_cons, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
        .merge(old_pop, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
        .merge(emissions_subset, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
        .merge(road_len, left_on="GHS_urban_area_id", right_on="ID_UC_G0", how="left").drop(columns=["ID_UC_G0"])
    )

    # Per capita emissions
    cities_clean["odiac_norm"] = cities_clean["ODIAC"] / cities_clean["GHS_population"]

    # Feature list (same naming style)
    variables = [
        "GHS_population", "GHS_population_growth",
        "GHS_population_density", "GHS_population_density_growth",
        "GHS_GDP_PPP", "GHS_GDP_PPP_growth", "GHS_critical_infra",
        # "GHS_greenness_index",
        # "GHS_precipitation",
        "hdd",
        "cdd",
        # "odiac_norm",
        "GHS_HDI",
        # "GHS_female_gender_index",
        # "GHS_land_cons",
        "GHS_old_pop",
        # "GHS_road_len",
        # "GHS_hosp_pc"
    ]

    cities_clean_sub = cities_clean[variables + ["GHS_urban_area_id"]].copy()
    cities_clean_sub.to_parquet(
        "data/clustering_data_clean/"
        "GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet",
        engine="pyarrow"
    )

    print("\n=== Missing values BEFORE imputation ===")
    missing_before = cities_clean_sub[variables].isnull().sum()
    print(missing_before[missing_before > 0])
    print(f"Total missing values: {cities_clean_sub[variables].isnull().sum().sum()}")

    # KNN imputation
    print("\n=== Performing KNN Imputation ===")
    knn_imputer = KNNImputer(n_neighbors=5, weights="distance")
    cities_clean_sub[variables] = knn_imputer.fit_transform(cities_clean_sub[variables])

    print("\n=== Missing values AFTER imputation ===")
    missing_after = cities_clean_sub[variables].isnull().sum()
    print(f"Total missing values: {missing_after.sum()}")
    print(f"Any NaN remaining: {cities_clean_sub[variables].isnull().any().any()}")

    # Scale
    scaler = StandardScaler()
    cities_clean_scaled = scaler.fit_transform(cities_clean_sub[variables])

    cities_clean_scaled_df = pd.DataFrame(cities_clean_scaled, columns=variables)
    cities_clean_scaled_df["GHS_urban_area_id"] = cities_clean_sub["GHS_urban_area_id"].values

    print(cities_clean_scaled_df.describe())

    os.makedirs(os.path.dirname(CLUSTERING_DATA_SCALED_OUT), exist_ok=True)
    cities_clean_scaled_df.to_parquet(CLUSTERING_DATA_SCALED_OUT)
    print("Scaled data written to:", CLUSTERING_DATA_SCALED_OUT)

    print("Any NaN in scaled array:", np.isnan(cities_clean_scaled).any())

    return cities_clean_sub, cities_clean_scaled, cities_clean_scaled_df, variables


# ------------------------------------------------------------------------------
# AUTOENCODER (tuning + manual)
# ------------------------------------------------------------------------------

def build_autoencoder_fixed(input_dim, encoding_dim=3, l2_reg=0.001, units1=64, units2=24):
    reg = l2(l2_reg)
    inp = layers.Input(shape=(input_dim,))
    x = layers.Dense(units1, activation="relu", kernel_regularizer=reg)(inp)
    x = layers.Dense(units2, activation="relu", kernel_regularizer=reg)(x)
    encoded = layers.Dense(encoding_dim, activation="relu", name="encoder_output")(x)
    x = layers.Dense(units2, activation="relu", kernel_regularizer=reg)(encoded)
    x = layers.Dense(units1, activation="relu", kernel_regularizer=reg)(x)
    out = layers.Dense(input_dim, activation="linear")(x)
    autoencoder = models.Model(inp, out)
    autoencoder.compile(optimizer="adam", loss="mse")
    return autoencoder


def build_autoencoder_tunable(hp, input_dim):
    encoding_dim = hp.Int("encoding_dim", min_value=2, max_value=4, step=1)
    reg = l2(hp.Float("l2_reg", 1e-6, 1e-2, sampling="log"))
    units1 = hp.Int("units1", 32, 96, step=16)
    units2 = hp.Int("units2", 16, 32, step=8)

    inp = layers.Input(shape=(input_dim,))
    x = layers.Dense(units1, activation="relu", kernel_regularizer=reg)(inp)
    x = layers.Dense(units2, activation="relu", kernel_regularizer=reg)(x)
    encoded = layers.Dense(encoding_dim, activation="relu", name="encoder_output")(x)
    x = layers.Dense(units2, activation="relu", kernel_regularizer=reg)(encoded)
    x = layers.Dense(units1, activation="relu", kernel_regularizer=reg)(x)
    out = layers.Dense(input_dim, activation="linear")(x)
    autoencoder = models.Model(inp, out)
    autoencoder.compile(optimizer="adam", loss="mse")
    return autoencoder


def train_autoencoder_for_run(run_id, X_scaled):
    seed_run = seed + run_id * 100
    np.random.seed(seed_run)
    random.seed(seed_run)
    tf.random.set_seed(seed_run)

    input_dim = X_scaled.shape[1]

    if USE_TUNING:
        print(f"Run {run_id}: TUNING autoencoder...")
        tuner = kt.Hyperband(
            hypermodel=lambda hp: build_autoencoder_tunable(hp, input_dim),
            objective="val_loss",
            max_epochs=50,
            factor=3,
            executions_per_trial=2,
            directory=os.path.join("clustering_models", "hyperband", f"run_{run_id}"),
            project_name="DEC_model_tuning",
            overwrite=True
        )
        tuner.search(
            X_scaled, X_scaled,
            epochs=5,
            batch_size=128,
            validation_split=0.2,
            callbacks=[callbacks.EarlyStopping(monitor="val_loss", patience=10)],
            verbose=0
        )
        best_model = tuner.get_best_models(1)[0]
        encoder_output = best_model.get_layer("encoder_output").output
        encoder_model = models.Model(best_model.input, encoder_output)
        del tuner, best_model

    else:
        print(f"Run {run_id}: MANUAL autoencoder with HP:", MANUAL_HPARAMS)
        autoencoder = build_autoencoder_fixed(
            input_dim=input_dim,
            encoding_dim=MANUAL_HPARAMS["encoding_dim"],
            l2_reg=MANUAL_HPARAMS["l2_reg"],
            units1=MANUAL_HPARAMS["units1"],
            units2=MANUAL_HPARAMS["units2"]
        )
        autoencoder.fit(
            X_scaled, X_scaled,
            epochs=100,
            batch_size=256,
            validation_split=0.2,
            callbacks=[callbacks.EarlyStopping(monitor="val_loss", patience=15, restore_best_weights=True)],
            verbose=0
        )
        encoder_output = autoencoder.get_layer("encoder_output").output
        encoder_model = models.Model(autoencoder.input, encoder_output)
        del autoencoder

    gc.collect()
    return encoder_model


def write_embeddings_for_run(run_id, encoder_model, X_scaled, ghs_ids):
    os.makedirs(LATENT_DIR, exist_ok=True)
    latent_path = os.path.join(LATENT_DIR, f"latent_run_{run_id}.pkl")

    if os.path.exists(latent_path):
        print(f"Run {run_id}: latent file already exists at {latent_path}, skipping computation.")
        return

    emb = encoder_model.predict(X_scaled, batch_size=2048, verbose=0)
    latent_df = pd.DataFrame(emb, columns=[f"latent_{i}" for i in range(emb.shape[1])])
    latent_df["GHS_urban_area_id"] = ghs_ids
    joblib.dump(latent_df, latent_path)
    print(f"Run {run_id}: Saved latent representation to {latent_path}")


# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------

if __name__ == "__main__":
    cities_clean_sub, X_scaled, X_scaled_df, variables = prepare_data()
    ghs_ids = cities_clean_sub["GHS_urban_area_id"].values

    for run_id in range(N_RUNS):
        print(f"\n===== Processing embedding run_id {run_id} =====")
        tf.keras.backend.clear_session()
        gc.collect()

        encoder_model = train_autoencoder_for_run(run_id, X_scaled)
        write_embeddings_for_run(run_id, encoder_model, X_scaled, ghs_ids)

        del encoder_model
        gc.collect()

    print("\nAll embeddings written to:", LATENT_DIR)
