import os
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score, KFold
from sklearn.metrics import r2_score, adjusted_rand_score
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.preprocessing import StandardScaler
from scipy.stats import entropy
import matplotlib.pyplot as plt
import seaborn as sns
from collections import defaultdict
import joblib
import warnings
warnings.filterwarnings('ignore')

os.chdir("/Users/simon/Documents/repo/cities-learning-dec")

# ============================================================================
# LOAD VULNERABILITY VARIABLES FROM GHSL-UCDB 2024A
# ============================================================================

def load_vulnerability_data():
    """
    Load vulnerability-related variables from GHSL-UCDB 2024A datasets.

    Returns:
        DataFrame with vulnerability indicators
    """
    print("\n=== Loading Vulnerability Data from GHSL-UCDB 2024A ===")

    # Load base data
    base_path = "data/GHS_UCDB_GLOBE_R2024A_V1_0/"

    # Environmental vulnerabilities
    try:
        env_vuln = pd.read_csv(f"{base_path}environmental.csv")
        env_cols = {
            'ID_UC_G0': 'ID_UC_G0',
            'EX_010_POP_2025': 'flood_prone_land_pct',  # Flood-prone land
            'EX_020_S1P_2025': 'cyclone_exposure'       # Cyclone exposure
        }
        env_vuln = env_vuln[list(env_cols.keys())].rename(columns=env_cols)
        print(f"  ✓ Environmental vulnerabilities loaded: {len(env_vuln)} cities")
    except Exception as e:
        print(f"  ✗ Environmental data failed: {e}")
        env_vuln = None

    # Health vulnerabilities
    try:
        health = pd.read_csv(f"{base_path}health.csv")
        health_cols = {
            'ID_UC_G0': 'ID_UC_G0',
            'HL_FPC_HOS_2025': 'hospital_access',       # Hospital access per capita
            'HL_FAC_HOS_2025': 'hospital_count'         # Number of hospitals
        }
        health = health[list(health_cols.keys())].rename(columns=health_cols)
        print(f"  ✓ Health vulnerabilities loaded: {len(health)} cities")
    except Exception as e:
        print(f"  ✗ Health data failed: {e}")
        health = None

    # Socioeconomic vulnerabilities
    try:
        socio = pd.read_csv(f"{base_path}socioeconomic.csv")
        socio_cols = {
            'ID_UC_G0': 'ID_UC_G0',
            'SC_SEC_GDF_2020': 'gender_dev_index',      # Gender development
            'SC_SEC_HDI_2020': 'hdi',                   # Human development
        }
        # Only select columns that exist
        available_cols = {k: v for k, v in socio_cols.items() if k in socio.columns}
        socio = socio[list(available_cols.keys())].rename(columns=available_cols)
        print(f"  ✓ Socioeconomic vulnerabilities loaded: {len(socio)} cities")
    except Exception as e:
        print(f"  ✗ Socioeconomic data failed: {e}")
        socio = None

    # Merge all vulnerability data
    vuln_data = None
    for df in [env_vuln, health, socio]:
        if df is not None:
            if vuln_data is None:
                vuln_data = df
            else:
                vuln_data = vuln_data.merge(df, on='ID_UC_G0', how='outer')

    if vuln_data is not None:
        print(f"\n  Total vulnerability dataset: {len(vuln_data)} cities, {len(vuln_data.columns)-1} indicators")
        print(f"  Missing data summary:\n{vuln_data.isnull().sum()}")

    return vuln_data


# ============================================================================
# 1A. PREDICTIVE VALIDITY COMPARISON
# ============================================================================

def compare_predictive_validity(clustering_results, outcome_data, feature_data, n_clusters=4):
    """
    Compare predictive power of different clustering methods across multiple outcomes.

    Args:
        clustering_results: DataFrame with cluster assignments from different methods
        outcome_data: DataFrame with outcome variables
        feature_data: Original features (for baseline)
        n_clusters: Number of clusters

    Returns:
        DataFrame with R² scores for each method-outcome pair
    """
    print("\n" + "="*80)
    print("1A. PREDICTIVE VALIDITY COMPARISON")
    print("="*80)

    methods = ['kmeans simple', 'hierarchical simple', 'kmeans embedded', 'dec']

    # Define outcomes to predict
    outcome_vars = {
        'ODIAC': 'CO2 emissions per capita',
        #'flood_prone_land_pct': 'Flood vulnerability',
        # 'hospital_access': 'Health access',
        # 'land_consumption_rate': 'Urban sprawl',
        #'gender_dev_index': 'Social equity'
    }

    # Filter to available outcomes
    available_outcomes = {k: v for k, v in outcome_vars.items() if k in outcome_data.columns}

    if len(available_outcomes) == 0:
        print("⚠️  No outcome variables available. Cannot perform validation.")
        return None

    print(f"\nTesting predictive power for {len(available_outcomes)} outcomes:")
    for var, desc in available_outcomes.items():
        print(f"  • {var}: {desc}")

    results = []

    for method in methods:
        print(f"\n--- Testing {method} ---")

        # Get cluster labels for this method
        method_col = f'cluster_label_{method.replace(" ", "_")}'
        if method_col not in clustering_results.columns:
            print(f"  ⚠️  {method_col} not found, skipping")
            continue

        labels = clustering_results[method_col].values

        # One-hot encode clusters
        cluster_dummies = pd.get_dummies(labels, prefix='cluster')

        for outcome_var, outcome_desc in available_outcomes.items():
            # Get outcome values (remove NaN)
            outcome = outcome_data[outcome_var].values
            valid_idx = ~np.isnan(outcome)

            if valid_idx.sum() < 100:
                print(f"  ⚠️  {outcome_var}: insufficient data ({valid_idx.sum()} cities)")
                continue

            X = cluster_dummies.values[valid_idx]
            y = outcome[valid_idx]

            # Cross-validated R²
            kf = KFold(n_splits=5, shuffle=True, random_state=42)
            cv_scores = cross_val_score(
                LinearRegression(), X, y,
                cv=kf, scoring='r2'
            )

            mean_r2 = cv_scores.mean()
            std_r2 = cv_scores.std()

            results.append({
                'method': method,
                'outcome': outcome_var,
                'outcome_desc': outcome_desc,
                'r2_mean': mean_r2,
                'r2_std': std_r2,
                'n_samples': valid_idx.sum()
            })

            print(f"  {outcome_var:25s}: R² = {mean_r2:.3f} ± {std_r2:.3f}")

    results_df = pd.DataFrame(results)

    # Create comparison table
    if len(results_df) > 0:
        pivot = results_df.pivot_table(
            index='outcome',
            columns='method',
            values='r2_mean'
        )

        print("\n" + "="*80)
        print("PREDICTIVE VALIDITY SUMMARY (Cross-Validated R²)")
        print("="*80)
        print(pivot.round(3))

        # Calculate improvement over simplest method
        if 'kmeans simple' in pivot.columns:
            baseline = pivot['kmeans simple']
            for method in methods:
                if method != 'kmeans simple' and method in pivot.columns:
                    improvement = ((pivot[method] - baseline) / baseline * 100).mean()
                    print(f"\n{method} average improvement over k-means: {improvement:.1f}%")

    return results_df


# ============================================================================
# 1B. CLUSTER STABILITY COMPARISON
# ============================================================================

def compare_cluster_stability(feature_data, methods_dict, n_clusters=4, n_bootstrap=50):
    """
    Test cluster stability using bootstrap resampling.

    Args:
        feature_data: Scaled feature matrix
        methods_dict: Dictionary of clustering methods to test
        n_clusters: Number of clusters
        n_bootstrap: Number of bootstrap samples

    Returns:
        DataFrame with stability scores (ARI) for each method
    """
    print("\n" + "="*80)
    print("1B. CLUSTER STABILITY COMPARISON (Bootstrap)")
    print("="*80)

    n_samples = len(feature_data)
    sample_size = int(0.8 * n_samples)

    results = []

    for method_name, cluster_func in methods_dict.items():
        print(f"\n--- Testing {method_name} stability ---")
        ari_scores = []

        for i in range(n_bootstrap):
            if i % 10 == 0:
                print(f"  Bootstrap iteration {i}/{n_bootstrap}")

            # Two independent random samples
            idx1 = np.random.choice(n_samples, size=sample_size, replace=False)
            idx2 = np.random.choice(n_samples, size=sample_size, replace=False)

            # Find overlapping samples
            overlap = np.intersect1d(idx1, idx2)

            if len(overlap) < 100:
                continue

            # Cluster both samples
            labels1_full = cluster_func(feature_data[idx1])
            labels2_full = cluster_func(feature_data[idx2])

            # Extract labels for overlapping samples
            overlap_in_idx1 = np.where(np.isin(idx1, overlap))[0]
            overlap_in_idx2 = np.where(np.isin(idx2, overlap))[0]

            labels1 = labels1_full[overlap_in_idx1]
            labels2 = labels2_full[overlap_in_idx2]

            # Compute ARI
            ari = adjusted_rand_score(labels1, labels2)
            ari_scores.append(ari)

        mean_ari = np.mean(ari_scores)
        std_ari = np.std(ari_scores)

        results.append({
            'method': method_name,
            'mean_ari': mean_ari,
            'std_ari': std_ari,
            'min_ari': np.min(ari_scores),
            'max_ari': np.max(ari_scores)
        })

        print(f"  Stability (ARI): {mean_ari:.3f} ± {std_ari:.3f}")

    results_df = pd.DataFrame(results).sort_values('mean_ari', ascending=False)

    print("\n" + "="*80)
    print("STABILITY SUMMARY (Adjusted Rand Index)")
    print("="*80)
    print(results_df.round(3))
    print("\nInterpretation:")
    print("  ARI > 0.80: Excellent stability")
    print("  ARI 0.65-0.80: Good stability")
    print("  ARI 0.50-0.65: Moderate stability")
    print("  ARI < 0.50: Poor stability")

    return results_df


# ============================================================================
# 1C. INTERPRETABILITY COMPARISON
# ============================================================================

def compare_interpretability(feature_data, feature_names, clustering_results, methods):
    """
    Compare interpretability of different clustering methods using Random Forest
    to predict cluster membership from original features.

    Args:
        feature_data: Original scaled features
        feature_names: List of feature names
        clustering_results: DataFrame with cluster labels
        methods: List of method names to compare

    Returns:
        DataFrame with interpretability metrics
    """
    print("\n" + "="*80)
    print("1C. INTERPRETABILITY COMPARISON")
    print("="*80)

    results = []

    for method in methods:
        print(f"\n--- {method} ---")

        method_col = f'cluster_label_{method.replace(" ", "_")}'
        if method_col not in clustering_results.columns:
            print(f"  ⚠️  {method_col} not found, skipping")
            continue

        labels = clustering_results[method_col].values

        # Train RF to predict clusters from original features
        rf = RandomForestClassifier(
            n_estimators=100,
            max_depth=10,
            random_state=42,
            n_jobs=-1
        )

        rf.fit(feature_data, labels)

        # Get feature importances
        importances = rf.feature_importances_

        # Calculate metrics
        # 1. Entropy of importance distribution (lower = more interpretable)
        importance_entropy = entropy(importances + 1e-10)

        # 2. Effective number of features (inverse participation ratio)
        effective_features = 1 / np.sum(importances ** 2)

        # 3. Top-3 feature concentration
        top3_importance = np.sum(np.sort(importances)[-3:])

        # 4. Gini importance spread
        gini_spread = np.std(importances)

        results.append({
            'method': method,
            'importance_entropy': importance_entropy,
            'effective_features': effective_features,
            'top3_concentration': top3_importance,
            'importance_spread': gini_spread
        })

        # Show top features
        feature_importance_df = pd.DataFrame({
            'feature': feature_names,
            'importance': importances
        }).sort_values('importance', ascending=False)

        print("  Top 5 most important features:")
        for idx, row in feature_importance_df.head(5).iterrows():
            print(f"    {row['feature']:30s}: {row['importance']:.3f}")

        print(f"\n  Interpretability metrics:")
        print(f"    Importance entropy: {importance_entropy:.3f} (lower = clearer)")
        print(f"    Effective features: {effective_features:.1f} (lower = simpler)")
        print(f"    Top-3 concentration: {top3_importance:.3f} (higher = simpler)")

    results_df = pd.DataFrame(results)

    print("\n" + "="*80)
    print("INTERPRETABILITY SUMMARY")
    print("="*80)
    print(results_df.round(3))
    print("\nInterpretation:")
    print("  Lower entropy = clusters driven by fewer features (more interpretable)")
    print("  Lower effective features = simpler decision boundaries")
    print("  Higher top-3 concentration = dominated by few key features")

    return results_df


# ============================================================================
# 3D. INCREMENTAL PREDICTIVE VALUE
# ============================================================================

def test_incremental_value(clustering_results, outcome_data, geographic_data, methods):
    """
    Test whether clusters add predictive value beyond simple geographic groupings.

    Args:
        clustering_results: DataFrame with cluster assignments
        outcome_data: DataFrame with outcome variables
        geographic_data: DataFrame with geographic indicators (continent, region, etc.)
        methods: List of clustering methods to test

    Returns:
        DataFrame with incremental R² values
    """
    print("\n" + "="*80)
    print("3D. INCREMENTAL PREDICTIVE VALUE (Beyond Geography)")
    print("="*80)

    # Define outcomes
    outcome_vars = {
        'ODIAC': 'CO2 emissions',
        'flood_prone_land_pct': 'Flood vulnerability',
        'hospital_access': 'Health access'
    }

    # Filter to available outcomes
    available_outcomes = {k: v for k, v in outcome_vars.items() if k in outcome_data.columns}

    if len(available_outcomes) == 0:
        print("⚠️  No outcome variables available.")
        return None

    results = []

    for outcome_var, outcome_desc in available_outcomes.items():
        print(f"\n--- Outcome: {outcome_desc} ({outcome_var}) ---")

        # Get outcome values
        outcome = outcome_data[outcome_var].values
        valid_idx = ~np.isnan(outcome)

        if valid_idx.sum() < 100:
            print(f"  ⚠️  Insufficient data ({valid_idx.sum()} cities)")
            continue

        y = outcome[valid_idx]

        # Model 1: Geographic dummies only (baseline)
        if 'continent' in geographic_data.columns:
            geo_dummies = pd.get_dummies(
                geographic_data['continent'].values[valid_idx],
                prefix='continent'
            )
        elif 'CTR_MN_NM' in geographic_data.columns:
            # Use country if continent not available
            geo_dummies = pd.get_dummies(
                geographic_data['CTR_MN_NM'].values[valid_idx],
                prefix='country'
            )
        else:
            print("  ⚠️  No geographic variables available")
            continue

        model_geo = LinearRegression()
        kf = KFold(n_splits=5, shuffle=True, random_state=42)
        r2_geo = cross_val_score(model_geo, geo_dummies, y, cv=kf, scoring='r2').mean()

        print(f"  Geography only:  R² = {r2_geo:.3f}")

        # Model 2 & 3: Test each clustering method
        for method in methods:
            method_col = f'cluster_label_{method.replace(" ", "_")}'
            if method_col not in clustering_results.columns:
                continue

            labels = clustering_results[method_col].values[valid_idx]
            cluster_dummies = pd.get_dummies(labels, prefix='cluster')

            # Clusters alone
            model_cluster = LinearRegression()
            r2_cluster = cross_val_score(model_cluster, cluster_dummies, y, cv=kf, scoring='r2').mean()

            # Geography + Clusters
            combined = pd.concat([
                geo_dummies.reset_index(drop=True),
                cluster_dummies.reset_index(drop=True)
            ], axis=1)

            model_combined = LinearRegression()
            r2_combined = cross_val_score(model_combined, combined, y, cv=kf, scoring='r2').mean()

            # Calculate incremental value
            incremental_r2 = r2_combined - r2_geo
            pct_improvement = (incremental_r2 / r2_geo * 100) if r2_geo > 0 else 0

            results.append({
                'outcome': outcome_var,
                'outcome_desc': outcome_desc,
                'method': method,
                'r2_geography': r2_geo,
                'r2_clusters': r2_cluster,
                'r2_combined': r2_combined,
                'incremental_r2': incremental_r2,
                'pct_improvement': pct_improvement
            })

            print(f"  {method:20s}: R² = {r2_cluster:.3f} | Combined = {r2_combined:.3f} | Incremental = {incremental_r2:+.3f} ({pct_improvement:+.1f}%)")

    results_df = pd.DataFrame(results)

    if len(results_df) > 0:
        print("\n" + "="*80)
        print("INCREMENTAL VALUE SUMMARY")
        print("="*80)

        # Average across outcomes
        summary = results_df.groupby('method').agg({
            'incremental_r2': 'mean',
            'pct_improvement': 'mean'
        }).sort_values('incremental_r2', ascending=False)

        print("\nAverage incremental R² beyond geography:")
        print(summary.round(3))

        print("\nInterpretation:")
        print("  Incremental R² > 0.10: Substantial added value")
        print("  Incremental R² 0.05-0.10: Moderate added value")
        print("  Incremental R² < 0.05: Limited added value beyond geography")

    return results_df


# ============================================================================
# MAIN EXECUTION
# ============================================================================

if __name__ == "__main__":

    # Load data
    print("Loading data...")

    # 1. Load vulnerability variables
    vuln_data = load_vulnerability_data()

    # 2. Load clustering results
    clustering_results = pd.read_csv("data/clustering_results/dec_clusters_k4.csv")

    # 3. Load original scaled features
    cities_clean_scaled_df = pd.read_parquet(
        "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included_scaled.parquet"
    )

    # 4. Load emissions data
    emissions = pd.read_csv("data/emissions/balance_sheet.csv")
    emissions = emissions[emissions['Year'] == 2022]

    # 5. Load geographic data for incremental value test
    try:
        geo_data = pd.read_parquet(
            "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet"
        )
    except:
        geo_data = cities_clean_scaled_df.copy()

    # Merge everything
    print("\nMerging datasets...")
    full_data = (
        clustering_results
        .merge(cities_clean_scaled_df, on='GHS_urban_area_id', how='left')
        .merge(emissions[['ID_UC_G0', 'ODIAC']],
               left_on='GHS_urban_area_id', right_on='ID_UC_G0', how='left')
        .drop(columns=['ID_UC_G0'], errors='ignore')
    )

    if vuln_data is not None:
        full_data = full_data.merge(
            vuln_data,
            left_on='GHS_urban_area_id',
            right_on='ID_UC_G0',
            how='left'
        ).drop(columns=['ID_UC_G0'], errors='ignore')

    # Normalize emissions by population
    if 'ODIAC' in full_data.columns and 'GHS_population' in full_data.columns:
        full_data['ODIAC'] = full_data['ODIAC'] / full_data['GHS_population']

    print(f"Final dataset: {len(full_data)} cities, {len(full_data.columns)} variables")

    # Extract components for analysis
    feature_cols = [col for col in cities_clean_scaled_df.columns
                   if col != 'GHS_urban_area_id']
    feature_data = cities_clean_scaled_df[feature_cols].values
    feature_names = feature_cols

    outcome_cols = ['ODIAC', 'flood_prone_land_pct', 'hospital_access',
                   'land_consumption_rate', 'gender_dev_index']
    outcome_data = full_data[['GHS_urban_area_id'] +
                            [c for c in outcome_cols if c in full_data.columns]]

    # Define clustering methods for stability test
    from sklearn.cluster import KMeans, AgglomerativeClustering

    methods_dict = {
        'K-means': lambda X: KMeans(n_clusters=4, n_init=20, random_state=42).fit_predict(X),
        'Hierarchical': lambda X: AgglomerativeClustering(n_clusters=4).fit_predict(X)
    }

    # Run analyses
    methods_to_test = ['kmeans simple', 'hierarchical simple', 'kmeans embedded', 'dec']

    # 1A. Predictive Validity
    validity_results = compare_predictive_validity(
        full_data,
        outcome_data,
        feature_data,
        n_clusters=4
    )
    if validity_results is not None:
        validity_results.to_csv("data/clustering_results/validation_predictive_validity.csv", index=False)
        print("\n✓ Saved: validation_predictive_validity.csv")

    # 1B. Stability
    stability_results = compare_cluster_stability(
        feature_data,
        methods_dict,
        n_clusters=4,
        n_bootstrap=50
    )
    stability_results.to_csv("data/clustering_results/validation_stability.csv", index=False)
    print("\n✓ Saved: validation_stability.csv")

    # 1C. Interpretability
    interpretability_results = compare_interpretability(
        feature_data,
        feature_names,
        full_data,
        methods_to_test
    )
    interpretability_results.to_csv("data/clustering_results/validation_interpretability.csv", index=False)
    print("\n✓ Saved: validation_interpretability.csv")

    # 3D. Incremental Value
    incremental_results = test_incremental_value(
        full_data,
        outcome_data,
        geo_data,
        methods_to_test
    )
    if incremental_results is not None:
        incremental_results.to_csv("data/clustering_results/validation_incremental_value.csv", index=False)
        print("\n✓ Saved: validation_incremental_value.csv")

    print("\n" + "="*80)
    print("VALIDATION ANALYSIS COMPLETE")
    print("="*80)
    print("\nNext steps:")
    print("  1. Review the output CSVs in data/clustering_results/")
    print("  2. Create visualizations of the comparison results")
    print("  3. Write up interpretation for manuscript")
    print("  4. Consider running robustness tests (feature variations, etc.)")
