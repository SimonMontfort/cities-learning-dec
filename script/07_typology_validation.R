# ============================================================================
# OUTCOME ASSOCIATION VALIDATION FOR CLUSTERING
# ============================================================================
# This script tests whether clustering methods capture substantive urban
# characteristics by examining associations with theoretically-relevant outcomes.
# 
# NOTE: This is NOT predictive validation - unsupervised learning doesn't 
# optimize for prediction. We're testing construct validity.
# ============================================================================

R.version
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          3.2                         
# year           2023                        
# month          10                          
# day            31                          
# svn rev        85441                       
# language       R                           
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes  
rm(list = ls())

setwd("/Users/simon/Documents/repo/cities-learning-dec")

library(tidyverse)
library(broom)
library(knitr)
library(effsize)



### ─────────────────────────────────────────────────────────────
### 1. Load COVARIATES
### ─────────────────────────────────────────────────────────────

ghsl_clean <- arrow::read_parquet(
  "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet"
)

ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") %>% 
  rename(city_id = ID_UC_G0)

vars <- c(
  "GHS_population", "GHS_population_growth",
  "GHS_population_density", "GHS_population_density_growth",
  "GHS_GDP_PPP", "GHS_GDP_PPP_growth", "GHS_critical_infra",
  "hdd", "cdd", "GHS_HDI", "GHS_old_pop"
)

covars <- ghsl_clean %>%
  select(city_id = GHS_urban_area_id, all_of(vars))


### ─────────────────────────────────────────────────────────────
### 2. Load OUTCOMES
### ─────────────────────────────────────────────────────────────


health <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/health.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(GHS_hosp_pc = as.numeric(HL_FPC_HOS_2025),
         GHS_hosp = as.numeric(HL_FCL_HOS_2024)) %>% 
  select(city_id, GHS_hosp_pc, GHS_hosp)

emissions <- read_csv("data/emissions/balance_sheet.csv") %>%
  filter(Year == 2020) %>% 
  rename(city_id = ID_UC_G0) %>%
  mutate(ODIAC = as.numeric(ODIAC)) %>% 
  left_join(ghsl %>% select(city_id, GC_POP_TOT_2025), by = "city_id") %>% 
  mutate(ODIAC_pc = as.numeric(ODIAC)/GC_POP_TOT_2025) %>% 
  select(city_id, ODIAC, ODIAC_pc)

low_ele_zone_pop <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_pop_share = as.numeric(EX_L10_SHP_2025),
         low_elevation_pop_n = as.numeric(EX_L10_POP_2025)) %>% 
  select(city_id, low_elevation_pop_share, low_elevation_pop_n)

low_ele_zone_built <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_built_share = as.numeric(EX_L10_SHB_2025),
         low_elevation_built_n = as.numeric(EX_L10_BUS_2025)) %>% 
  select(city_id, low_elevation_built_share, low_elevation_built_n)

n_hazards <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(hazards_n = as.numeric(HZ_CEV_THZ_2015)) %>% 
  mutate(hazards_pc = as.numeric(HZ_CEV_THZ_2015)/GC_POP_TOT_2025) %>% 
  select(city_id, hazards_n, hazards_pc)


floods <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(flood_prone_share = as.numeric(EX_010_SHP_2025),
         flood_prone_n = as.numeric(EX_100_POP_2025)) %>%
  select(city_id, flood_prone_share, flood_prone_n)

wildfires <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(wildfires_n = as.numeric(HZ_CEV_WLF_2015)) %>%
  mutate(wildfires_pc = as.numeric(HZ_CEV_WLF_2015)/GC_POP_TOT_2025) %>% 
  select(city_id, wildfires_n, wildfires_pc)

warm_days <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/climate.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(n_warm_days_2030 = as.numeric(CL_WDS_126_2030),
         n_warm_days_2010 = as.numeric(CL_WDS_CUR_2010)) %>%
  select(city_id, n_warm_days_2030, n_warm_days_2010)

# pm2 <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/emissions.csv") %>%
#   rename(city_id = ID_UC_G0) %>%
#   mutate(pm2 = as.numeric(EM_PM2_CON_2020),
#          pm2_pc = as.numeric(EM_PM2_PEC_2020),
#          pm2_mortality = as.numeric(EM_PM2_MOR_2020)) %>%
#   select(city_id, pm2, pm2_mortality)



#### n studies
# oa data
file_names <- list.files(
  path = "/Users/simon/Documents/repo/cities-learning/data/OpenAlex/05_deduplicated",
  pattern = "^city_works_df_NA_abstr_added_dedup_\\d+\\.csv$",
  full.names = TRUE
)
df_list <- lapply(file_names, read.csv)
oa <- do.call(rbind, df_list)

# studies per city
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv")

clean_places <- clean_places %>% 
  filter((city_word_match_yes | city_intersects_yes) %in% TRUE) %>%
  filter(id %in% oa$id) %>% # only deduplicated count
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id) %>% 
  distinct() 

n_studies_per_city <- clean_places %>% 
  group_by(city_id) %>% 
  summarise(n_studies = n()) %>% 
  left_join(ghsl, by = "city_id") %>% 
  mutate(n_studies_pc = n_studies/GC_POP_TOT_2025)


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
  # left_join(pm2, by = "city_id") %>%
  
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
  "GHS_hosp_pc", "GHS_hosp",
  "ODIAC", "ODIAC_pc",
  "low_elevation_pop_share", "low_elevation_pop_n",
  "low_elevation_built_share", "low_elevation_built_n",
  "flood_prone_share", "flood_prone_n",
  "wildfires_n", "wildfires_pc",
  "n_warm_days_2010", "n_warm_days_2030"
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

outcome_area <- c(
  # Health
  GHS_hosp_pc = "Health",
  GHS_hosp    = "Health",
  
  # Emissions
  ODIAC       = "Emissions",
  ODIAC_pc    = "Emissions",
  
  # Vulnerability
  low_elevation_pop_share  = "Vulnerability:\ncoastal",
  low_elevation_pop_n      = "Vulnerability:\ncoastal",
  low_elevation_built_share = "Vulnerability:\ncoastal",
  low_elevation_built_n    = "Vulnerability:\ncoastal",
  flood_prone_share        = "Vulnerability:\nfloods",
  flood_prone_n            = "Vulnerability:\nfloods",
  wildfires_n              = "Vulnerability:\nwildfire",
  wildfires_pc             = "Vulnerability:\nwildfire",
  n_warm_days_2010         = "Vulnerability:\nheat",
  n_warm_days_2030         = "Vulnerability:\nheat"
)

outcome_labels <- c(
  GHS_hosp_pc = "Hospitals p.c.",
  GHS_hosp    = "Hospitals",
  ODIAC       = "ODIAC emissions",
  ODIAC_pc    = "ODIAC emissions p.c.",
  low_elevation_pop_share = "% population\nliving 5–10m elev.",
  low_elevation_pop_n = "Population in\n5–10m elev.",
  low_elevation_built_share = "% built-up\narea 5–10m elev.",
  low_elevation_built_n = "Built-up area in\n5–10m elev.",
  flood_prone_share = "% exposed\nto floods",
  flood_prone_n = "Population exposed\nto floods",
  wildfires_n = "Wildfires",
  wildfires_pc = "Wildfires p.c.",
  n_warm_days_2010 = "Warm days (%)\n2010",
  n_warm_days_2030 = "Warm days (%)\n2030"
)

# Load and register a modern font (e.g., Helvetica Neue)
showtext_auto()  # Automatically use showtext for fonts

# Check available fonts
# remotes::install_github("kjhealy/myriad")
# myriad::import_myriad(font_family = "Myriad Pro", silent = F)
theme_SM <- function(){
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          text = element_text(size = 12, 
                              # family = "Myriad Pro"
          ),
          axis.text.y = element_text(colour = "grey30"),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth=.5),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.9,.05),
          legend.margin = margin(rep(2, 4)),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill="white", 
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
}

figA1 <- association_results %>%
  mutate(
    eta_squared = as.numeric(eta_squared),
    cluster_method = str_to_title(gsub("_cluster", "", cluster_method)),
    cluster_method = ifelse(cluster_method == "Dec", "Deep Embedded Clustering (DEC)", cluster_method),
    
    # assign labels & area category
    outcome_label = outcome_labels[outcome],
    outcome_type = outcome_area[outcome],
    
    # enforce plotting order using the original outcomes vector
    outcome_label = factor(outcome_label, levels = outcome_labels[outcomes]),
    
    # optional: area ordering
    outcome_type = factor(outcome_type, levels = c("Emissions", 
                                                   "Vulnerability:\ncoastal", "Vulnerability:\nfloods",
                                                   "Vulnerability:\nwildfire", "Vulnerability:\nheat",
                                                   "Health"))
  ) %>% 
  group_by(outcome_label) %>% 
  mutate(mean_eta = mean(eta_squared)) %>% 
  ggplot(aes(x = forcats::fct_reorder(outcome_label, mean_eta), 
             y = eta_squared, 
             col = cluster_method, 
             shape = cluster_method
             )) +
  geom_point(position = position_dodge(width = .7), size = 2.5, alpha = .7) +
  coord_flip() +
  scale_color_aaas() +
  facet_grid(outcome_type~., scales = "free_y", space = "free") +
  labs(
    title = "Validation of typology: cluster-outcome associations",
    x = "Outcome",
    y = "Percentage of variation accounted for (η²)",
  ) +
  theme_SM() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom", 
    strip.text.y.right = element_text(angle = 0)
  )

figA1

ggsave("plots/figA1.pdf", figA1, width = 10, height = 7)


### ─────────────────────────────────────────────────────────────
### FINAL SUMMARY
### ─────────────────────────────────────────────────────────────

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

