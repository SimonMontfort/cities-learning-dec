R.version
rm(list = ls())

setwd("/Users/simon/Documents/repo/cities-learning-dec")

library(tidyverse)
library(broom)
library(knitr)
library(effsize)
library(showtext)
library(patchwork)

### ─────────────────────────────────────────────────────────────
### 1. Load DATA
### ─────────────────────────────────────────────────────────────

ghsl_clean <- arrow::read_parquet(
  "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet"
)

ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") %>%
  rename(city_id = ID_UC_G0)

emissions <- read_csv("data/emissions/balance_sheet.csv") %>%
  filter(Year == 2020) %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(ODIAC = as.numeric(ODIAC)) %>%
  left_join(ghsl %>% select(city_id, GC_POP_TOT_2025), by = "city_id") %>%
  mutate(ODIAC_pc = as.numeric(ODIAC) / GC_POP_TOT_2025) %>%
  select(city_id, ODIAC, ODIAC_pc)

low_ele_zone_pop <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_pop_share = as.numeric(EX_L10_SHP_2025),
         low_elevation_pop_n     = as.numeric(EX_L10_POP_2025)) %>%
  select(city_id, low_elevation_pop_share, low_elevation_pop_n)

low_ele_zone_built <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(low_elevation_built_share = as.numeric(EX_L10_SHB_2025),
         low_elevation_built_n     = as.numeric(EX_L10_BUS_2025)) %>%
  select(city_id, low_elevation_built_share, low_elevation_built_n)

floods <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/exposure.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(flood_prone_share = as.numeric(EX_010_SHP_2025),
         flood_prone_n     = as.numeric(EX_100_POP_2025)) %>%
  select(city_id, flood_prone_share, flood_prone_n)

n_hazards <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(hazards_n  = as.numeric(HZ_CEV_THZ_2015),
         hazards_pc = as.numeric(HZ_CEV_THZ_2015) / GC_POP_TOT_2025) %>%
  select(city_id, hazards_n, hazards_pc)

wildfires <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/hazards_risks.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(wildfires_n  = as.numeric(HZ_CEV_WLF_2015),
         wildfires_pc = as.numeric(HZ_CEV_WLF_2015) / GC_POP_TOT_2025) %>%
  select(city_id, wildfires_n, wildfires_pc)

warm_days <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/climate.csv") %>%
  rename(city_id = ID_UC_G0) %>%
  mutate(n_warm_days_2030 = as.numeric(CL_WDS_126_2030),
         n_warm_days_2010 = as.numeric(CL_WDS_CUR_2010)) %>%
  select(city_id, n_warm_days_2030, n_warm_days_2010)

n_studies_per_city <- read.csv("data/case_study_count/n_studies_per_city.csv") %>%
  left_join(ghsl, by = "city_id") %>%
  mutate(n_studies_pc = n_studies / GC_POP_TOT_2025)

### ─────────────────────────────────────────────────────────────
### 2. Load CLUSTERING LABELS
### ─────────────────────────────────────────────────────────────

raw_clusters <- read_csv("data/clustering_results/raw_clustering_scores.csv")

get_cluster_labels <- function(data, run_id, method_name) {
  selected <- data %>% filter(run_id == !!run_id, method == !!method_name)
  if (nrow(selected) == 0) return(NULL)
  labels <- selected$labels %>% str_split(",") %>% .[[1]] %>% as.integer()
  tibble(city_id = seq_along(labels), cluster = factor(labels)) %>%
    rename_with(~paste0(method_name, "_cluster"), .cols = cluster)
}

kmeans_simple    <- get_cluster_labels(raw_clusters, 5, "kmeans simple")
hierarchical     <- get_cluster_labels(raw_clusters, 5, "hierarchical simple")
kmeans_embedded  <- get_cluster_labels(raw_clusters, 5, "kmeans embedded")

dec <- read_csv("data/clustering_results/dec_clusters_k4.csv") %>%
  rename(city_id     = GHS_urban_area_id,
         dec_cluster = consensus_label_majority) %>%
  mutate(dec_cluster = factor(dec_cluster)) %>%
  select(city_id, dec_cluster)

### ─────────────────────────────────────────────────────────────
### 3. Load INCOME GROUPS & SOFT ASSIGNMENTS
### ─────────────────────────────────────────────────────────────

ghsl_inc <- ghsl %>%
  as.data.frame() %>%
  select(city_id, GC_DEV_WIG_2025) %>%
  as_tibble() %>%
  mutate(
    city_id      = as.integer(city_id),
    income_group  = case_when(
      GC_DEV_WIG_2025 %in% c("Low income", "Lower Middle") ~ "low",
      GC_DEV_WIG_2025 == "Upper Middle"                    ~ "medium",
      GC_DEV_WIG_2025 == "High income"                     ~ "high",
      TRUE ~ NA_character_
    ),
    income_group4 = factor(GC_DEV_WIG_2025, levels = c(
      "Low income", "Lower Middle", "Upper Middle", "High income"
    ))
  ) %>%
  select(city_id, income_group, income_group4)

soft_probs <- readxl::read_xlsx("data/case_selection/data_base_clean.xlsx", sheet = 2) %>%
  filter(main_or_mixed_type == "main type") %>%
  select(city_id, cluster_name,
         p1 = `assignment_probability: Type 1`,
         p2 = `assignment_probability: Type 2`,
         p3 = `assignment_probability: Type 3`,
         p4 = `assignment_probability: Type 4`) %>%
  mutate(city_id  = as.integer(city_id),
         max_prob = pmax(p1, p2, p3, p4)) %>%
  select(city_id, cluster_name, max_prob)

### ─────────────────────────────────────────────────────────────
### 4. Merge ALL DATA
### ─────────────────────────────────────────────────────────────

df <- emissions %>%
  left_join(low_ele_zone_pop,    by = "city_id") %>%
  left_join(low_ele_zone_built,  by = "city_id") %>%
  left_join(n_studies_per_city,  by = "city_id") %>%
  left_join(floods,              by = "city_id") %>%
  left_join(n_hazards,           by = "city_id") %>%
  left_join(wildfires,           by = "city_id") %>%
  left_join(warm_days,           by = "city_id") %>%
  left_join(kmeans_simple,       by = "city_id") %>%
  left_join(hierarchical,        by = "city_id") %>%
  left_join(kmeans_embedded,     by = "city_id") %>%
  left_join(dec,                 by = "city_id") %>%
  left_join(ghsl_inc,            by = "city_id") %>%
  left_join(soft_probs,          by = "city_id") %>%
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))

cat(sprintf("Dataset: %d cities, %d variables\n", nrow(df), ncol(df)))

### ─────────────────────────────────────────────────────────────
### 5. HELPER FUNCTIONS
### ─────────────────────────────────────────────────────────────

calculate_eta_squared <- function(aov_model) {
  s         <- summary(aov_model)[[1]]
  ss_between <- s$`Sum Sq`[1]
  ss_total   <- sum(s$`Sum Sq`)
  eta_sq     <- ss_between / ss_total
  interpretation <- case_when(
    eta_sq >= 0.14 ~ "Strong",
    eta_sq >= 0.06 ~ "Medium",
    TRUE           ~ "Weak"
  )
  c(eta_squared = eta_sq, interpretation = interpretation)
}

test_cluster_outcome_association <- function(data, cluster_var, outcome_var) {
  d <- data %>%
    select(cluster = all_of(cluster_var), outcome = all_of(outcome_var)) %>%
    filter(!is.na(cluster), !is.na(outcome))
  n_obs <- nrow(d)
  if (n_obs < 100) {
    return(tibble(cluster_method = cluster_var, outcome = outcome_var,
                  n = n_obs, f_statistic = NA, p_value = NA,
                  eta_squared = NA_real_, interpretation = "Insufficient data"))
  }
  m   <- aov(outcome ~ cluster, data = d)
  s   <- summary(m)[[1]]
  eta <- calculate_eta_squared(m)
  tibble(cluster_method = cluster_var, outcome = outcome_var,
         n = n_obs, f_statistic = s$`F value`[1], p_value = s$`Pr(>F)`[1],
         eta_squared = as.numeric(eta["eta_squared"]),
         interpretation = eta["interpretation"])
}

run_anova <- function(outcome, groupvar, data) {
  d <- data %>% filter(!is.na(.data[[groupvar]]), !is.na(.data[[outcome]]))
  m <- aov(as.formula(paste(outcome, "~", groupvar)), data = d)
  calculate_eta_squared(m)
}

run_soft <- function(outcome, data) {
  d <- data %>% filter(!is.na(cluster_name), !is.na(max_prob), !is.na(.data[[outcome]]))
  m <- aov(as.formula(paste(outcome, "~ cluster_name * max_prob")), data = d)
  calculate_eta_squared(m)
}

### ─────────────────────────────────────────────────────────────
### 6. OUTCOME LABELS & GROUPINGS
### ─────────────────────────────────────────────────────────────

outcomes <- c(
  "ODIAC", "ODIAC_pc",
  "low_elevation_pop_share", "low_elevation_pop_n",
  "low_elevation_built_share", "low_elevation_built_n",
  "flood_prone_share", "flood_prone_n",
  "wildfires_n", "wildfires_pc",
  "n_warm_days_2010", "n_warm_days_2030"
)

outcome_area <- c(
  ODIAC                     = "Emissions",
  ODIAC_pc                  = "Emissions",
  low_elevation_pop_share   = "Vulnerability:\ncoastal",
  low_elevation_pop_n       = "Vulnerability:\ncoastal",
  low_elevation_built_share = "Vulnerability:\ncoastal",
  low_elevation_built_n     = "Vulnerability:\ncoastal",
  flood_prone_share         = "Vulnerability:\nfloods",
  flood_prone_n             = "Vulnerability:\nfloods",
  wildfires_n               = "Vulnerability:\nwildfire",
  wildfires_pc              = "Vulnerability:\nwildfire",
  n_warm_days_2010          = "Vulnerability:\nheat",
  n_warm_days_2030          = "Vulnerability:\nheat"
)

outcome_labels <- c(
  ODIAC                     = "ODIAC emissions",
  ODIAC_pc                  = "ODIAC emissions p.c.",
  low_elevation_pop_share   = "% population\nliving 5–10m elev.",
  low_elevation_pop_n       = "Population in\n5–10m elev.",
  low_elevation_built_share = "% built-up\narea 5–10m elev.",
  low_elevation_built_n     = "Built-up area in\n5–10m elev.",
  flood_prone_share         = "% exposed\nto floods",
  flood_prone_n             = "Population exposed\nto floods",
  wildfires_n               = "Wildfires",
  wildfires_pc              = "Wildfires p.c.",
  n_warm_days_2010          = "Warm days (%)\n2010",
  n_warm_days_2030          = "Warm days (%)\n2030"
)

### ─────────────────────────────────────────────────────────────
### 7. METHOD LEVELS, COLOURS, SHAPES  (single source of truth)
### ─────────────────────────────────────────────────────────────

method_levels <- c(
  "WB income groups (3-cat)",
  "WB income groups (4-cat)",
  "K-means",
  "Hierarchical",
  "K-means (embedded)",
  "Deep Embedded Clustering (DEC)",
  "DEC (soft assignment)"
)

method_colours <- c(
  "WB income groups (3-cat)"       = "#AAAAAA",
  "WB income groups (4-cat)"       = "#555555",
  "K-means"                        = "#8DA0CB",
  "Hierarchical"                   = "#66C2A5",
  "K-means (embedded)"             = "#FC8D62",
  "Deep Embedded Clustering (DEC)" = "#E41A1C",
  "DEC (soft assignment)"          = "#984EA3"
)

method_shapes <- c(
  "WB income groups (3-cat)"       = 15,
  "WB income groups (4-cat)"       = 15,
  "K-means"                        = 16,
  "Hierarchical"                   = 16,
  "K-means (embedded)"             = 16,
  "Deep Embedded Clustering (DEC)" = 17,
  "DEC (soft assignment)"          = 17
)

# helper to harmonise method names across all result objects
harmonise_methods <- function(x) {
  case_when(
    x %in% c("dec", "Dec")                        ~ "Deep Embedded Clustering (DEC)",
    x %in% c("dec soft assignment",
             "DEC soft assignment")               ~ "DEC (soft assignment)",
    x %in% c("income group", "Income Group")      ~ "WB income groups (3-cat)",
    x %in% c("income group (4-cat)")              ~ "WB income groups (4-cat)",
    x %in% c("kmeans simple", "Kmeans Simple")    ~ "K-means",
    x %in% c("hierarchical simple",
             "Hierarchical Simple")               ~ "Hierarchical",
    x %in% c("kmeans embedded",
             "Kmeans Embedded")                   ~ "K-means (embedded)",
    TRUE ~ x
  )
}

### ─────────────────────────────────────────────────────────────
### 8. RUN ASSOCIATION ANALYSIS
### ─────────────────────────────────────────────────────────────

cluster_methods    <- c("kmeans simple_cluster", "hierarchical simple_cluster",
                        "kmeans embedded_cluster", "dec_cluster")
available_methods  <- cluster_methods[cluster_methods %in% names(df)]
available_outcomes <- outcomes[outcomes %in% names(df)]

association_results <- map_dfr(available_methods, function(cm) {
  map_dfr(available_outcomes, ~test_cluster_outcome_association(df, cm, .x))
}) %>%
  mutate(
    cluster_method = str_replace(cluster_method, "_cluster$", ""),
    cluster_method = str_replace(cluster_method, "_", " "),
    eta_squared    = as.numeric(eta_squared),
    cluster_method = harmonise_methods(cluster_method)
  )

### ─────────────────────────────────────────────────────────────
### 9. INCOME GROUP & SOFT ASSIGNMENT RESULTS
### ─────────────────────────────────────────────────────────────

income_soft_results <- pmap_dfr(
  tibble(outcome = outcomes),
  \(outcome) {
    eta_inc3 <- run_anova(outcome, "income_group",  df)
    eta_inc4 <- run_anova(outcome, "income_group4", df)
    eta_soft <- run_soft(outcome, df)
    bind_rows(
      tibble(outcome = outcome,
             eta_squared    = as.numeric(eta_inc3[["eta_squared"]]),
             interpretation = eta_inc3[["interpretation"]],
             cluster_method = "WB income groups (3-cat)"),
      tibble(outcome = outcome,
             eta_squared    = as.numeric(eta_inc4[["eta_squared"]]),
             interpretation = eta_inc4[["interpretation"]],
             cluster_method = "WB income groups (4-cat)"),
      tibble(outcome = outcome,
             eta_squared    = as.numeric(eta_soft[["eta_squared"]]),
             interpretation = eta_soft[["interpretation"]],
             cluster_method = "DEC (soft assignment)")
    )
  }
)

all_results <- bind_rows(association_results, income_soft_results) %>%
  mutate(cluster_method = factor(cluster_method, levels = method_levels))

### ─────────────────────────────────────────────────────────────
### 10. COMPARISON SUMMARY (for reporting)
### ─────────────────────────────────────────────────────────────

comparison_results <- pmap_dfr(
  tibble(outcome = outcomes),
  \(outcome) {
    e_inc3 <- as.numeric(run_anova(outcome, "income_group",  df)[["eta_squared"]])
    e_inc4 <- as.numeric(run_anova(outcome, "income_group4", df)[["eta_squared"]])
    e_dec  <- as.numeric(run_anova(outcome, "dec_cluster",   df)[["eta_squared"]])
    e_soft <- as.numeric(run_soft(outcome, df)[["eta_squared"]])
    tibble(outcome = outcome,
           eta_inc3 = e_inc3, eta_inc4 = e_inc4,
           eta_dec  = e_dec,  eta_soft = e_soft,
           diff_dec_inc3 = e_dec - e_inc3,
           diff_dec_inc4 = e_dec - e_inc4,
           diff_soft_dec = e_soft - e_dec)
  }
)

summary_eta <- comparison_results %>%
  summarise(across(starts_with("eta_") | starts_with("diff_"), mean, na.rm = TRUE))

cat("\nMean η² summary:\n")
print(summary_eta)

dec_mean <- summary_eta$eta_dec

### ─────────────────────────────────────────────────────────────
### 11. THEME
### ─────────────────────────────────────────────────────────────

showtext_auto()

theme_SM <- function() {
  theme_light() +
    theme(
      panel.grid        = element_blank(),
      panel.border      = element_rect(colour = "grey50", fill = NA, linewidth = .5),
      strip.placement   = "outside",
      text              = element_text(size = 12),
      axis.text.y       = element_text(colour = "grey30"),
      axis.ticks.length = unit(.2, "cm"),
      axis.ticks        = element_line(colour = "grey50", linewidth = .5),
      strip.background  = element_rect(fill = "white"),
      strip.text        = element_text(colour = "black"),
      strip.clip        = "off",
      legend.text       = element_text(size = 7),
      legend.key.size   = unit(.4, "cm"),
      legend.title      = element_blank(),
      legend.background = element_rect(fill = "white", linewidth = .3,
                                       linetype = "solid", colour = "grey")
    )
}

### ─────────────────────────────────────────────────────────────
### 12. PANEL A: normalised mean η²
### ─────────────────────────────────────────────────────────────

compare_perf <- function(method) {
  overall_comparison <- all_results %>%
    filter(cluster_method %in% c(method, "Deep Embedded Clustering (DEC)")) %>%
    select(outcome, cluster_method, eta_squared) %>%
    pivot_wider(names_from = cluster_method, values_from = eta_squared) %>%
    mutate(improvement = `Deep Embedded Clustering (DEC)` / .data[[method]])
  mean(overall_comparison$improvement, na.rm = TRUE)
}

pA_data <- all_results %>%
  group_by(cluster_method) %>%
  summarise(mean_eta = mean(eta_squared, na.rm = TRUE)) %>%
  mutate(
    normalised     = mean_eta / dec_mean * 100,
    cluster_method = factor(cluster_method, levels = rev(c("DEC (soft assignment)", "Deep Embedded Clustering (DEC)",
                                                       "WB income groups (4-cat)", "WB income groups (3-cat)",
                                                       "K-means (embedded)", "K-means", "Hierarchical")))
  ) %>% 
  mutate(dummy = "")

pA <- ggplot(pA_data,
             aes(x = normalised,
                 y = cluster_method,
                 colour = cluster_method,
                 shape  = cluster_method)) +
  geom_vline(xintercept = 100, linewidth = 0.4, colour = "grey60", linetype = "dashed") +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(normalised, 1), "%")),
            hjust = -0.25, size = 3, colour = "grey20") +
  scale_colour_manual(values = method_colours, name = NULL) +
  scale_shape_manual(values = method_shapes, name = NULL) +
  scale_x_continuous(
    limits = c(0, 135),
    breaks = c(0, 25, 50, 75, 100),
    labels = \(x) paste0(x, "%")
  ) +
  facet_grid(dummy~.) + 
  # scale_y_discrete(limits = rev(method_levels)) +
  labs(x = "Mean η² relative to DEC hard assignment (= 100%)", y = NULL) +
  theme_SM() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

### ─────────────────────────────────────────────────────────────
### 13. PANEL B: η² by outcome
### ─────────────────────────────────────────────────────────────

pB <- all_results %>%
  mutate(
    outcome_label = outcome_labels[outcome],
    outcome_label = factor(outcome_label, levels = outcome_labels[outcomes]),
    outcome_type  = factor(outcome_area[outcome], levels = c(
      "Emissions", "Vulnerability:\ncoastal", "Vulnerability:\nfloods",
      "Vulnerability:\nwildfire", "Vulnerability:\nheat"
    )),
    cluster_method = factor(cluster_method, levels = rev(c("DEC (soft assignment)", "Deep Embedded Clustering (DEC)",
                                                       "WB income groups (4-cat)", "WB income groups (3-cat)",
                                                       "K-means (embedded)", "K-means", "Hierarchical")))
  ) %>%
  group_by(outcome_label) %>%
  mutate(mean_eta = mean(eta_squared, na.rm = TRUE)) %>%
  ggplot(aes(
    x      = eta_squared,
    y      = forcats::fct_reorder(outcome_label, mean_eta),
    colour = cluster_method,
    shape  = cluster_method
  )) +
  geom_point(position = position_dodge(width = 0.7), size = 2.5, alpha = 0.85) +
  scale_colour_manual(values = method_colours, name = NULL,
                      limits = method_levels) +
  scale_shape_manual(values = method_shapes, name = NULL,
                     limits = method_levels) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  facet_grid(outcome_type ~ ., scales = "free_y", space = "free") +
  labs(
    x       = "Variance explained (η²)",
    y       = NULL,
    # caption = "Four-category WB income classification ensures comparability with DEC (4 types).\nDEC soft assignment uses cluster label × max assignment probability interaction."
  ) +
  theme_SM() +
  theme(
    legend.position    = c(.7,0.07),
    legend.box         = "vertical",
    strip.text.y.right = element_text(angle = 0)
  ) +
  guides(
    colour = guide_legend(ncol = 2),
    shape  = guide_legend(ncol = 2)
  )

### ─────────────────────────────────────────────────────────────
### 14. COMBINE & SAVE
### ─────────────────────────────────────────────────────────────

get_pct <- function(method) {
  pA_data %>% filter(cluster_method == method) %>% pull(normalised) %>% round(1)
}

subtitle_text <- paste0(
  "WB income groups (3-cat) explain ", get_pct("WB income groups (3-cat)"), "%, ",
  "WB income groups (4-cat) ", get_pct("WB income groups (4-cat)"), "%, ",
  "k-means ", get_pct("K-means"), "%, ",
  "hierarchical ", get_pct("Hierarchical"), "%, and\n",
  "k-means (embedded) ", get_pct("K-means (embedded)"), "% ",
  "of the variance explained by DEC (hard assignment = 100%)"
)

p_figA1 <- (pA / pB) +
  plot_layout(heights = c(1, 5)) +
  plot_annotation(
    subtitle = subtitle_text,
    tag_levels = "a",
    theme = theme(
      plot.tag      = element_text(size = 12, face = "bold")
    )
  )

p_figA1
ggsave("plots/figA1.pdf", p_figA1, width = 10, height = 12)

