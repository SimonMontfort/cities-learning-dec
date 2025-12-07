library(tidyverse)
library(broom)
library(knitr)
library(effsize)

# ============================================================================
# OUTCOME ASSOCIATION VALIDATION FOR CLUSTERING
# ============================================================================
# This script tests whether clustering methods capture substantive urban
# characteristics by examining associations with theoretically-relevant outcomes.
# 
# NOTE: This is NOT predictive validation - unsupervised learning doesn't 
# optimize for prediction. We're testing construct validity.
# ============================================================================

### ─────────────────────────────────────────────────────────────
### 1. Load OUTCOMES
### ─────────────────────────────────────────────────────────────

health <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/health.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(GHS_hosp_pc = as.numeric(HL_FPC_HOS_2025)) %>% 
  select(city_id, GHS_hosp_pc)

emissions <- read_csv("data/emissions/balance_sheet.csv") %>%
  filter(Year == 2020) %>% 
  rename(city_id = ID_UC_G0) %>%
  mutate(ODIAC = as.numeric(ODIAC)) %>% 
  select(city_id, ODIAC)

low_ele_zone_pop <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_pop = as.numeric(EX_L10_SHP_2025)) %>% 
  select(city_id, low_elevation_pop)

low_ele_zone_built <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_built = as.numeric(EX_L10_SHB_2025)) %>% 
  select(city_id, low_elevation_built)

n_hazards <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(n_hazards = as.numeric(HZ_CEV_THZ_2015)) %>% 
  select(city_id, n_hazards)


floods <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(flood_prone = as.numeric(EX_010_SHP_2025)) %>%
  select(city_id, flood_prone)

wildfires <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(n_wildfires = as.numeric(HZ_CEV_WLF_2015)) %>%
  select(city_id, n_wildfires)

warm_days <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/climate.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(n_warm_days = as.numeric(CL_WDS_126_2030)) %>%
  select(city_id, n_warm_days)

pm2 <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/emissions.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(pm2 = as.numeric(EM_PM2_CON_2020)) %>%
  select(city_id, pm2)




# n_studies_per_city

### ─────────────────────────────────────────────────────────────
### 2. Load COVARIATES
### ─────────────────────────────────────────────────────────────

ghsl_clean <- arrow::read_parquet(
  "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet"
)

vars <- c(
  "GHS_population", "GHS_population_growth",
  "GHS_population_density", "GHS_population_density_growth",
  "GHS_GDP_PPP", "GHS_GDP_PPP_growth", "GHS_critical_infra",
  "hdd", "cdd", "GHS_HDI", "GHS_old_pop"
)

covars <- ghsl_clean %>%
  select(city_id = GHS_urban_area_id, all_of(vars))

# Normalize emissions by population
emissions <- emissions %>%
  left_join(covars %>% select(city_id, GHS_population), by = "city_id") %>%
  mutate(emissions_pc = ODIAC / GHS_population) %>%
  select(city_id, emissions_pc)

### ─────────────────────────────────────────────────────────────
### 3. Load CLUSTERING LABELS
### ─────────────────────────────────────────────────────────────

# Load raw clustering results
raw_clusters <- read_csv("data/clustering_results/raw_clustering_scores.csv")

# Extract labels for different methods (using run_id = 0 as example)
get_cluster_labels <- function(data, run_id, method_name) {
  selected <- data %>% 
    filter(run_id == !!run_id, method == !!method_name)
  
  if (nrow(selected) == 0) return(NULL)
  
  labels <- selected$labels %>% 
    str_split(",") %>% 
    .[[1]] %>% 
    as.integer()
  
  tibble(
    city_id = seq_along(labels),
    cluster = factor(labels)
  ) %>%
    rename_with(~paste0(method_name, "_cluster"), .cols = cluster)
}

# Get labels for all methods
kmeans_simple <- get_cluster_labels(raw_clusters, 0, "kmeans simple")
hierarchical <- get_cluster_labels(raw_clusters, 0, "hierarchical simple")
kmeans_embedded <- get_cluster_labels(raw_clusters, 0, "kmeans embedded")

# DEC consensus clusters
dec <- read_csv("data/clustering_results/dec_clusters_k4.csv") %>%
  rename(city_id = GHS_urban_area_id,
         dec_cluster = consensus_label_majority) %>%
  mutate(dec_cluster = factor(dec_cluster)) %>%
  select(city_id, dec_cluster)

### ─────────────────────────────────────────────────────────────
### 4. Merge ALL DATA
### ─────────────────────────────────────────────────────────────

df <- emissions %>% 
  left_join(health, by = "city_id") %>%
  left_join(low_ele_zone_pop, by = "city_id") %>%
  left_join(low_ele_zone_built, by = "city_id") %>%
  left_join(n_studies_per_city, by = "city_id") %>% 
  left_join(floods, by = "city_id") %>%
  left_join(n_hazards, by = "city_id") %>%
  left_join(wildfires, by = "city_id") %>%
  left_join(warm_days, by = "city_id") %>%
  left_join(pm2, by = "city_id") %>%
  
  left_join(kmeans_simple, by = "city_id") %>%
  left_join(hierarchical, by = "city_id") %>%
  left_join(kmeans_embedded, by = "city_id") %>%
  left_join(dec, by = "city_id") %>% 
  
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))

cat("Dataset loaded:\n")
cat(sprintf("  %d cities\n", nrow(df)))
cat(sprintf("  %d variables\n", ncol(df)))

### ============================================================================
### OUTCOME ASSOCIATION ANALYSIS
### ============================================================================

#' Calculate eta-squared (effect size) from ANOVA
#' 
#' @param aov_model An aov model object
#' @return Named vector with eta-squared and interpretation
calculate_eta_squared <- function(aov_model) {
  aov_summary <- summary(aov_model)[[1]]
  
  ss_between <- aov_summary$`Sum Sq`[1]  # Cluster effect
  ss_total <- sum(aov_summary$`Sum Sq`)  # Total variation
  
  eta_sq <- ss_between / ss_total
  
  # Interpretation (Cohen's guidelines adapted for eta-squared)
  interpretation <- case_when(
    eta_sq >= 0.14 ~ "Strong",
    eta_sq >= 0.06 ~ "Medium",
    TRUE ~ "Weak"
  )
  
  c(eta_squared = eta_sq, interpretation = interpretation)
}

#' Test association between clusters and outcome
#' 
#' @param data Dataframe with cluster labels and outcome
#' @param cluster_var Name of cluster variable (string)
#' @param outcome_var Name of outcome variable (string)
#' @return Tibble with test results
test_cluster_outcome_association <- function(data, cluster_var, outcome_var) {
  
  # Remove missing values
  analysis_data <- data %>%
    select(cluster = all_of(cluster_var), outcome = all_of(outcome_var)) %>%
    filter(!is.na(cluster), !is.na(outcome))
  
  n_obs <- nrow(analysis_data)
  
  if (n_obs < 100) {
    return(tibble(
      cluster_method = cluster_var,
      outcome = outcome_var,
      n = n_obs,
      f_statistic = NA,
      p_value = NA,
      se = NA,
      eta_squared = NA,
      interpretation = "Insufficient data"
    ))
  }
  
  # ANOVA: Test if cluster means differ
  aov_model <- aov(outcome ~ cluster, data = analysis_data)
  aov_summary <- summary(aov_model)[[1]]
  
  # Extract test statistics
  f_stat <- aov_summary$`F value`[1]
  p_val <- aov_summary$`Pr(>F)`[1]
  
  # Calculate effect size
  eta_results <- calculate_eta_squared(aov_model)
  
  tibble(
    cluster_method = cluster_var,
    outcome = outcome_var,
    n = n_obs,
    f_statistic = f_stat,
    p_value = p_val,
    eta_squared = eta_results["eta_squared"],
    interpretation = eta_results["interpretation"]
  )
}

#' Compare all clustering methods across all outcomes
#' 
#' @param data Dataframe with all cluster labels and outcomes
#' @param cluster_methods Vector of cluster variable names
#' @param outcomes Vector of outcome variable names
#' @return Tibble with all association test results
compare_all_associations <- function(data, cluster_methods, outcomes) {
  
  results <- map_dfr(cluster_methods, function(cluster_var) {
    map_dfr(outcomes, function(outcome_var) {
      test_cluster_outcome_association(data, cluster_var, outcome_var)
    })
  })
  
  return(results)
}

### ─────────────────────────────────────────────────────────────
### RUN ASSOCIATION ANALYSIS
### ─────────────────────────────────────────────────────────────

cat("\n")

cat("OUTCOME ASSOCIATION ANALYSIS\n")

cat("Testing if clusters differentiate cities on urban outcomes\n")
cat("(NOT predictive validation - measuring construct validity)\n")


# Define methods and outcomes to test
cluster_methods <- c(
  "kmeans simple_cluster",
  "hierarchical simple_cluster", 
  "kmeans embedded_cluster",
  "dec_cluster"
)

outcomes <- c(
  "emissions_pc",
  "GHS_hosp_pc",
  "low_elevation_pop",
  "low_elevation_built",
  "flood_prone",
  "n_hazards",
  "n_studies", 
  "n_wildfires",
  "n_warm_days",
  "pm2"
)

# Check which variables actually exist
available_methods <- cluster_methods[cluster_methods %in% names(df)]
available_outcomes <- outcomes[outcomes %in% names(df)]

cat("Testing associations:\n")
cat(sprintf("  Methods: %s\n", paste(available_methods, collapse=", ")))
cat(sprintf("  Outcomes: %s\n", paste(available_outcomes, collapse=", ")))
cat("\n")

# Run association tests
association_results <- compare_all_associations(df, available_methods, available_outcomes)

# Clean up method names for display
association_results <- association_results %>%
  mutate(
    cluster_method = str_replace(cluster_method, "_cluster$", ""),
    cluster_method = str_replace(cluster_method, "_", " ")
  )

### ─────────────────────────────────────────────────────────────
### DISPLAY RESULTS
### ─────────────────────────────────────────────────────────────

cat("\n=== DETAILED RESULTS ===\n\n")

# Print by outcome
for (outcome_name in available_outcomes) {
  cat(sprintf("--- %s ---\n", outcome_name))
  
  outcome_results <- association_results %>%
    filter(outcome == outcome_name) %>%
    arrange(desc(eta_squared))
  
  for (i in 1:nrow(outcome_results)) {
    row <- outcome_results[i,]
    cat(sprintf(
      # "  %-25s: η²=%.3f (F=%.1f, p=%.4f) [%s]\n",
      row$cluster_method,
      row$eta_squared,
      row$f_statistic,
      row$p_value,
      row$interpretation
    ))
  }
  cat("\n")
}

### ─────────────────────────────────────────────────────────────
### SUMMARY TABLES
### ─────────────────────────────────────────────────────────────


cat("OUTCOME ASSOCIATION SUMMARY (Effect Sizes: η²)\n")


# Create pivot table of eta-squared values
eta_pivot <- association_results %>%
  select(cluster_method, outcome, eta_squared) %>%
  pivot_wider(names_from = cluster_method, values_from = eta_squared)

print(kable(eta_pivot, digits = 3, format = "simple"))

cat("\nInterpretation of η² (proportion of variance explained):\n")
cat("  η² > 0.14: Strong association (clusters strongly differentiate)\n")
cat("  η² 0.06-0.14: Medium association\n")
cat("  η² < 0.06: Weak association (clusters don't differentiate well)\n\n")

### ─────────────────────────────────────────────────────────────
### AVERAGE EFFECT SIZES BY METHOD
### ─────────────────────────────────────────────────────────────


cat("AVERAGE EFFECT SIZE BY METHOD\n")


avg_effects <- association_results %>%
  mutate(eta_squared = as.numeric(eta_squared)) %>% 
  group_by(outcome, cluster_method) %>%
  summarise(
    mean_eta_squared = mean(eta_squared, na.rm = TRUE),
    median_eta_squared = median(eta_squared, na.rm = TRUE),
    n_strong = sum(interpretation == "Strong", na.rm = TRUE),
    n_medium = sum(interpretation == "Medium", na.rm = TRUE),
    n_weak = sum(interpretation == "Weak", na.rm = TRUE)
  ) %>%
  arrange(desc(mean_eta_squared))

print(kable(avg_effects, digits = 3, format = "simple"))

### ─────────────────────────────────────────────────────────────
### RELATIVE COMPARISON: DEC vs BASELINES
### ─────────────────────────────────────────────────────────────

cat("\n")

cat("RELATIVE PERFORMANCE: DEC vs BASELINE METHODS\n")


# Compare DEC to each baseline method
baseline_methods <- c("kmeans simple", "hierarchical simple", "kmeans embedded")

for (baseline in baseline_methods) {
  
  if (!baseline %in% association_results$cluster_method) next
  
  comparison <- association_results %>%
    filter(cluster_method %in% c(baseline, "dec")) %>%
    select(cluster_method, outcome, eta_squared) %>%
    pivot_wider(names_from = cluster_method, values_from = eta_squared) %>%
    mutate_at(vars(baseline, "dec"), as.numeric) %>% 
    mutate(
      improvement = dec - !!sym(baseline),
      pct_improvement = (dec - !!sym(baseline)) / !!sym(baseline) * 100
    )
  
  avg_improvement <- mean(comparison$pct_improvement, na.rm = TRUE)
  
  cat(sprintf("--- DEC vs %s ---\n", baseline))
  print(kable(
    comparison %>% select(outcome, !!sym(baseline), dec, improvement, pct_improvement),
    digits = 3,
    format = "simple"
  ))
  cat(sprintf("\nAverage improvement: %+.1f%%\n\n", avg_improvement))
  
  # Count wins/losses
  n_better <- sum(comparison$improvement > 0, na.rm = TRUE)
  n_worse <- sum(comparison$improvement < 0, na.rm = TRUE)
  n_total <- sum(!is.na(comparison$improvement))
  
  cat(sprintf("DEC performs better: %d/%d outcomes (%.0f%%)\n\n", 
              n_better, n_total, n_better/n_total*100))
}

### ─────────────────────────────────────────────────────────────
### SAVE RESULTS
### ─────────────────────────────────────────────────────────────

write_csv(association_results, "data/clustering_results/r_validation_outcome_associations.csv")
cat("\n✓ Saved: data/clustering_results/r_validation_outcome_associations.csv\n")

### ─────────────────────────────────────────────────────────────
### VISUALIZATION: EFFECT SIZES BY METHOD
### ─────────────────────────────────────────────────────────────

p1 <- association_results %>%
  mutate(eta_squared = as.numeric(eta_squared)) %>% 
  mutate(cluster_method  = str_to_title(gsub("_cluster", "", cluster_method)),
         outcome  = case_when(outcome == "emissions_pc" ~ "Emissions p.c.",
                              outcome == "n_studies" ~ "Number of case\nstudies",
                              outcome == "GHS_hosp_pc" ~ "Number of hospitals\np.c.",
                              outcome == "low_elevation_pop" ~ "% of population\nliving 5-10m elevation",
                              outcome == "low_elevation_built" ~ "% of built up\narea 5-10m elevation",
                              outcome == "flood_prone" ~ "% of people exposed to floods",
                              outcome == "n_wildfires" ~ "Number of wildfires",
                              outcome == "n_warm_days" ~ "% of days with maximum\ntemperature - projection\nssp 126",
                              outcome == "pm2" ~ "PM2.5 population weighted\naverage concentrations",
                              outcome == "n_hazards" ~ "Combined number of hazards"),) %>% 
  group_by(outcome) %>% 
  mutate(mean_eta = mean(eta_squared)) %>% 
  ggplot(aes(x = forcats::fct_reorder(outcome, mean_eta), 
             y = eta_squared, 
             col = cluster_method, 
             shape = cluster_method
             )) +
  geom_point(position = position_dodge(width = -.4), size = 2, alpha = .5) +
  # geom_hline(yintercept = c(0.06, 0.14), linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  scale_color_aaas() +
  # scale_fill_manual(
  #   values = c("Strong" = "#2ecc71", "Medium" = "#f39c12", "Weak" = "#e74c3c")
  # ) +
  # facet_wrap(~outcome) + 
  labs(
    title = "Validation of typology: cluster-outcome associations",
    # subtitle = "η² values across all outcomes",
    x = "Clustering Method",
    y = "Percentage of variation accounted for (η²)",
  ) +
  theme_SM() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

p1

ggsave("plots/r_validation_effect_sizes.pdf", 
       p1, width = 10, height = 6)
cat("✓ Saved: data/clustering_results/r_validation_effect_sizes.png\n")


### ─────────────────────────────────────────────────────────────
### FINAL SUMMARY
### ─────────────────────────────────────────────────────────────

cat("\n")
cat("VALIDATION COMPLETE\n")
cat("Key findings:\n")

# Which method performs best on average?
best_method <- avg_effects %>% slice(1) %>% pull(cluster_method)
best_eta <- avg_effects %>% slice(1) %>% pull(mean_eta_squared)

cat(sprintf("  • Best performing method: %s (mean η² = %.3f)\n", best_method, best_eta))

# How many outcomes show strong associations?
n_strong_dec <- association_results %>% 
  filter(cluster_method == "dec", interpretation == "Strong") %>% 
  nrow()

cat(sprintf("  • DEC shows strong associations for %d/%d outcomes\n", 
            n_strong_dec, length(available_outcomes)))

# Overall improvement
if ("kmeans simple" %in% association_results$cluster_method && 
    "dec" %in% association_results$cluster_method) {
  
  overall_comparison <- association_results %>%
    filter(cluster_method %in% c("kmeans simple", "dec")) %>%
    select(outcome, cluster_method, eta_squared) %>%
    pivot_wider(names_from = cluster_method, values_from = eta_squared) %>%
    mutate_at(c("kmeans simple", "dec"), as.numeric) %>% 
    mutate(improvement = (dec - `kmeans simple`) / `kmeans simple` * 100)
  
  avg_overall_improvement <- mean(overall_comparison$improvement, na.rm = TRUE)
  
  cat(sprintf("  • DEC improves over k-means by %.1f%% on average\n", avg_overall_improvement))
}

cat("\nInterpretation:\n")
cat("  These associations validate that clusters capture substantive urban\n")
cat("  characteristics relevant to sustainability outcomes. Higher effect sizes\n")
cat("  indicate clusters differentiate cities on important dimensions.\n\n")

cat("  NOTE: This is construct validation, not predictive validation.\n")
cat("  Unsupervised learning doesn't optimize for prediction.\n")
