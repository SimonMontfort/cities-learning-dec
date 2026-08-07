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

setwd("/Users/simon/Documents/repo/cities-learning-dec/robustness/rural_filter")

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(sf)
library(showtext)
library(rnaturalearth)
library(cowplot)
library(purrr)
library(arrow)
library(ggsci)
library(stringi)
library(stringr)
library(ggpubr)
library(ggtext)
library(ggpp)
library(ggrepel)
library(cowplot)
library(ggpattern)
library(ggh4x)

################################################################################
# load data
################################################################################

clust <- read.csv("data/clustering_results/dec_clusters_k4.csv")
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
ghsl_clean <- read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included+.parquet")

world <- ne_countries(scale = "medium", returnclass = "sf")
bb <- ne_download(type = "wgs84_bounding_box", category = "physical", returnclass = "sf") 

oa <- readxl::read_xlsx("data/case_selection/case_selection_and_literature.xlsx", sheet = 2)

# studies per city
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv")

clean_places <- clean_places %>% 
  filter((city_word_match_yes | city_intersects_yes) %in% TRUE) %>%
  filter(id %in% oa$OpenAlex_article_id) %>% # only deduplicated count
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id) %>% 
  distinct() 

write.csv(clean_places, "data/geoparser/clean_places_augmented_dedup.csv")

n_studies_per_city <- clean_places %>% 
  group_by(city_id) %>% 
  summarise(n_studies = n())

# ipcc regions
ipcc_regions <- st_read("data/IPCC-WGI-reference-regions-v4_shapefile/IPCC-WGI-reference-regions-v4.shp")

st_layers('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg')
ipcc_regions_hexa_groupings <- st_read('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg', layer = "groupings")
ipcc_regions_hexa_regionlabels <- st_read('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg', layer = "regionlabels")
ipcc_regions_hexa_regionals <- st_read('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg', layer = "regionals")
ipcc_regions_hexa_lablelines <- st_read('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg', layer = "labellines")
ipcc_regions_hexa <- st_read('data/IPCC-WGI-reference-regions-v4_shapefile/zones.gpkg')
ipcc_regions_hexa_split <- st_read("data/IPCC-WGI-reference-regions-v4_shapefile/zones_hexagons_split_triangles.gpkg")

cities_ipcc_regions <- read.csv("data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv")


################################################################################
# load data for ex post characterisation
################################################################################

emmissions <- read.csv("data/emissions/balance_sheet.csv")

################################################################################
# covariate lists and labels
################################################################################

co_vars <- c("GHS_population", "GHS_population_growth", "GHS_population_density", "GHS_population_density_growth",
             "GHS_old_pop", 'GHS_HDI', 'GHS_female_gender_index',
             "GHS_GDP_PPP", "GHS_GDP_PPP_growth", 
             "GHS_critical_infra", 
             "hdd",
             "cdd"
)
co_vars_formatted <- c("Population", "Population growth", "Population density", "Population density growth", 
                       "Old/young population", "HDI", "Gender index", "GDP PPP", "GDP PPP growth", 
                       "Critical infrastructure", 
                       "Heating degree days", 
                       "Cooling degree days"
)
reg_vars <- c("NORTH-AMERICA", "SOUTH-AMERICA", "EUROPE", "AFRICA", "ASIA", "OCEANIA" , "SMALL ISLANDS")
reg_vars_wg2 <- c("North America", "South America", "Europe", "Africa", "Asia", "Australasia", "Small Islands")

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Type 2",
    "Type 1",
    "Type 4", 
    "Type 3"
  )) %>% 
  mutate(cluster_name = factor(cluster_name, levels = c("Type 1",
                                                        "Type 2",
                                                        "Type 3",
                                                        "Type 4"))) 

################################################################################
# project and transform 
################################################################################

proj_robin <- "+proj=robin"
ghsl <- st_transform(ghsl, proj_robin)
world <- st_transform(world, proj_robin)
bb <- st_transform(bb, proj_robin)

################################################################################
# functions needed throughout the script
################################################################################

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
          axis.text.x = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
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

rename_co_vars <- function(df, column) {
  rename_map <- c(
    "GHS_population" = "Population",
    "GHS_population_growth" = "Population growth",
    "GHS_population_density" = "Population density",
    "GHS_population_density_growth" = "Population density growth",
    "GHS_GDP_PPP" = "GDP PPP",
    "GHS_GDP_PPP_growth" = "GDP PPP growth",
    "GHS_critical_infra" = "Critical infrastructure",
    "GHS_greenness_index" = "Greenness",
    "GHS_precipitation" = "Precipitation",
    "hdd" = "Heating degree days",
    "cdd" = "Cooling degree days",
    'GHS_HDI' = "HDI", 
    'GHS_female_gender_index' = "Gender index",
    "GHS_old_pop" =  "Old/young population",
    "odiac_norm" = "CO2 emissions p.c."
  )
  
  column <- rlang::ensym(column)
  
  df %>%
    mutate(!!column := recode(!!column, !!!rename_map))
}


################################################################################
# recode variables and add ipcc continents
################################################################################

clust_probs <- clust %>%
  pivot_longer(
    cols = starts_with("mean_prob_cluster_"),
    names_to = "secondary_cluster",
    names_prefix = "mean_prob_cluster_",
    values_to = "mean_prob"
  ) %>% 
  select(-similarity) %>% 
  mutate(secondary_cluster = as.numeric(secondary_cluster)) %>% 
  left_join(cluster_names, by = c("secondary_cluster"="consensus_label_majority")) %>% 
  rename(secondary_cluster_name = cluster_name) %>% 
  left_join(cluster_names, by = c("consensus_label_majority")) 


clust <- clust %>%
  pivot_longer(
    cols = starts_with("mean_prob_cluster_"),
    names_to = "cluster_prob",
    names_prefix = "mean_prob_cluster_",
    values_to = "mean_prob"
  ) %>%
  group_by(GHS_urban_area_id) %>% 
  slice_max(mean_prob, with_ties = FALSE) %>% 
  left_join(ghsl_clean, by= "GHS_urban_area_id") %>% 
  left_join(ghsl, by= c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  mutate(GHS_population = GHS_population/1000000,
         GHS_population_density = GHS_population_density/1000000,
         GHS_GDP_PPP = GHS_GDP_PPP/1000) %>% 
  select(GHS_urban_area_id, consensus_label_majority, 
         co_vars, similarity, mean_prob, entropy
  ) %>% 
  left_join(cities_ipcc_regions, by= c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))

sort(unique(clust$consensus_label_majority))


################################################################################
# figures
################################################################################
desc_dat <- clust %>% 
  filter(!is.na(consensus_label_majority)) %>% 
  as.data.frame() %>% 
  select(consensus_label_majority, co_vars, 
         Region, mean_prob, entropy,
         GHS_urban_area_id) 

# ################################################################################
# # summary statistics: table
# ################################################################################

# Calculate stats grouped by consensus_label_majority
summ_stats <- desc_dat %>%
  select(consensus_label_majority, co_vars) %>%
  pivot_longer(-consensus_label_majority, names_to = "variable", values_to = "values") %>%
  mutate(variable = factor(variable, levels = co_vars)) %>% 
  group_by(consensus_label_majority, variable) %>%
  summarise(min = min(values, na.rm = TRUE),
            p25 = quantile(values, 0.25, na.rm = TRUE),
            median = quantile(values, 0.5, na.rm = TRUE),
            mean = mean(values, na.rm = TRUE),
            p75 = quantile(values, 0.75, na.rm = TRUE),
            max = max(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename_co_vars("variable") %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
  ungroup() %>%
  select(Type = cluster_name, Variable = variable, min, p25, median, p75, max, mean, sd) 

summ_stats

library(knitr)
library(xtable)

print(xtable(summ_stats,
             caption = "Summary Statistics by Cluster",
             # align = c("l", "l", "r", "r", "r", "r", "r", "r", "r")
),
include.rownames = FALSE,
tabular.environment = "longtable",
floating = FALSE)


################################################################################
# Cluster characterisation + outcomes data 
################################################################################


###########
# emissions
###########

# Compute median emissions per Region × cluster_name
emmissions_dat <- emmissions %>%
  as.data.frame() %>% 
  left_join(
    clust %>% select(consensus_label_majority, Region, GHS_urban_area_id, GHS_population),
    by = c("ID_UC_G0" = "GHS_urban_area_id")
  ) %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
  left_join(ghsl %>% select(GC_POP_TOT_2025, ID_UC_G0), by = c("ID_UC_G0")) %>% 
  mutate(
    odiac_norm = ODIAC / GC_POP_TOT_2025,
    Region = factor(Region, levels = reg_vars)
  )

emmissions_box_dat <- emmissions_dat %>% 
  filter(Year == 2020) 

# Calculate medians for coloring
median_data <- emmissions_box_dat %>%
  group_by(Region, cluster_name) %>%
  summarize(median_odiac = median(odiac_norm, na.rm = TRUE), .groups = "drop")

# Join medians back
emmissions_box_dat <- emmissions_box_dat %>%
  left_join(median_data, by = c("Region", "cluster_name")) %>% 
  group_by(Region, cluster_name) %>%
  mutate(
    Q1 = quantile(odiac_norm, 0.25, na.rm = TRUE),
    Q3 = quantile(odiac_norm, 0.75, na.rm = TRUE),
    IQR_val = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR_val,
    upper_bound = Q3 + 1.5 * IQR_val
  ) %>%
  filter(odiac_norm >= lower_bound & odiac_norm <= upper_bound) %>%
  ungroup()
# 
# 
# library(see)
# 
# p_emissions_box <- ggplot(emmissions_box_dat, aes(x = cluster_name, y = odiac_norm, fill = median_odiac)) +
#   geom_hline(yintercept = median(median_data$median_odiac, na.rm = TRUE), lty = 3, col = "grey50") +
#   geom_violinhalf(outliers = FALSE, trim = TRUE, 
#                   side = "l",
#                   alpha = 0.6, 
#                   lwd = 0.2, 
#                   flip = TRUE
#   ) +
#   geom_boxplot(outliers = FALSE, outlier.size = 0.5, alpha = 0.8, width = 0.3, na.rm = TRUE, lwd = 0.2, position = position_nudge(x = 0.15)) +
#   scale_fill_gradient2(
#     low = "#a0c4ff", mid = "white", high = "#FFADAD",
#     midpoint = median(median_data$median_odiac, na.rm = TRUE),
#     name = "Median Emissions"
#   ) +
#   labs(
#     x = "",
#     y = "Emissions p.c. (t CO₂ p.a.)"
#   ) +
#   # facet_wrap(~Region, nrow = 1) +
#   theme_SM() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")
# 
# ggsave(p_emissions_box, file = "plots/p_emissions_box.pdf", width = 10, height = 5)



# check
ghsl_clean %>% 
  left_join(ghsl %>% dplyr::select(ID_UC_G0, CL_B12_CUR_2010), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(clust %>% dplyr::select(GHS_urban_area_id, consensus_label_majority), by = "GHS_urban_area_id" ) %>% 
  dplyr::select(GHS_urban_area_id, consensus_label_majority, co_vars) %>% 
  pivot_longer(-c(GHS_urban_area_id, consensus_label_majority), names_to = "variable") %>% 
  mutate(clustering = ifelse(variable %in% co_vars, "Clustering", "Outcomes")) %>% 
  rename_co_vars("variable") %>% 
  left_join(cluster_names, by = c("consensus_label_majority" = "consensus_label_majority")) %>% 
  group_by(variable) %>% 
  summarise()

###########
# all data
###########

box_plot_add_covs_dat <- ghsl_clean %>% 
  left_join(ghsl %>% dplyr::select(ID_UC_G0, CL_B12_CUR_2010), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(clust %>% dplyr::select(GHS_urban_area_id, consensus_label_majority), by = "GHS_urban_area_id" ) %>% 
  left_join(emmissions_box_dat %>% dplyr::select(ID_UC_G0, odiac_norm), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>%
  dplyr::select(GHS_urban_area_id, consensus_label_majority, co_vars, GHS_HDI, odiac_norm) %>% 
  pivot_longer(-c(GHS_urban_area_id, consensus_label_majority), names_to = "variable") %>% 
  mutate(clustering = ifelse(variable %in% co_vars, "Clustering", "Outcomes")) %>% 
  rename_co_vars("variable") %>% 
  mutate(
    variable = ifelse(variable == "odiac_norm", "CO2 emissions p.c.", variable),
    variable = factor(variable, levels = c(co_vars_formatted, "CO2 emissions p.c."))) %>%
  left_join(cluster_names, by = c("consensus_label_majority" = "consensus_label_majority")) %>% 
  group_by(variable) %>%
  mutate(
    scaled_value = scale(value),
    normalized_value = value / mean(value, na.rm = TRUE)) %>%
  ungroup() 

box_plot_add_covs_dat


###########
# prepare maps
###########

desc_geo <- ghsl %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>%
  left_join(clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) 

# Compute centroids for each region
desc_geo$centroid <- st_centroid(desc_geo$geom)

to_label <- desc_geo %>% 
  filter(GHS_population >= .8) %>% 
  group_by(cluster_name, Region) %>% 
  arrange(-mean_prob) %>% 
  slice_max(mean_prob, n = 3)

write.csv(to_label %>% as.data.frame() %>% select(ID_UC_G0, cluster_name, Region, GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, n_studies), "data/case_selection/three_representative_cities_per_regions.csv")


# ################################################################################
# # mixed type assignment
# ################################################################################

desc_geo_exlc <- desc_geo %>%
  as.data.frame() %>%
  group_by(cluster_name) %>%
  mutate(
    main_mixed = ifelse(mean_prob < .65*median(mean_prob), "mixed", "main type"),
  )
table(desc_geo_exlc$cluster_name, desc_geo_exlc$main_mixed)
sum(desc_geo_exlc$main_mixed == "mixed")/nrow(desc_geo_exlc)
write.csv(as.data.frame(desc_geo_exlc) %>% 
            select(ID_UC_G0, GC_UCN_MAI_2025, cluster_name, main_mixed),
          "data/clustering_results/type_main_mixed.csv")

################################################################################
# regional cluster characterisation (fig 2)
################################################################################

scale_percentile <- function(x) {
  1 + 99 * ecdf(x)(x)
}

clust_scaled <- clust %>%
  ungroup() %>% 
  left_join(desc_geo_exlc %>% ungroup() %>% select(ID_UC_G0, main_mixed), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  mutate(cluster_name = ifelse(main_mixed == "mixed", "mixed", as.character(cluster_name)),
         cluster_name = factor(cluster_name, levels = c(levels(cluster_names$cluster_name), "mixed"))
  ) %>% 
  ungroup() %>% 
  mutate(across(all_of(co_vars), scale_percentile)) 

type_medians <- clust_scaled %>%
  group_by(cluster_name) %>%
  summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
  pivot_longer(col = all_of(co_vars), names_to = "var", values_to = "type_med")

type_region_medians <- clust_scaled %>%
  group_by(Region, cluster_name) %>%
  summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
  pivot_longer(col = all_of(co_vars), names_to = "var", values_to = "reg_type_med")

region_medians <- clust_scaled %>%
  group_by(Region) %>%
  summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
  pivot_longer(col = all_of(co_vars), names_to = "var", values_to = "reg_med")

global_medians <- clust_scaled %>%
  summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
  pivot_longer(all_of(co_vars), names_to = "var", values_to = "global_med")

df1 <- type_medians %>%
  left_join(global_medians, by="var") %>%
  mutate(
    diff = type_med - global_med,      # real percentage-point shift
    abs_diff = abs(diff)
  ) %>% 
  select(name = cluster_name, var, diff)


df3 <- type_region_medians %>%
  ungroup() %>% 
  left_join(region_medians, by=c("var", "Region")) %>%
  mutate(
    diff = reg_type_med - reg_med,      # real percentage-point shift
    abs_diff = abs(diff)
  ) %>% 
  group_by(cluster_name, Region)


# 5. Plot top 3
fig2_reg <- df3 %>% 
  rename_co_vars("var") %>%
  mutate(name_combined = paste(Region, cluster_name, var, sep = "___")) %>% 
  filter(!is.na(diff)) %>% 
  group_by(Region, cluster_name) %>% 
  arrange(-abs_diff, .by_group = T) %>% 
  slice_head(n = 3) %>% 
  mutate(name_combined = forcats::fct_reorder(name_combined, diff)) %>% 
  ggplot(aes(x = diff, y = name_combined)) +
  geom_col(aes(x = diff, fill = cluster_name), col = "black", size = .2) + 
  geom_vline(xintercept = 0, lty = 1, col = "grey30") + 
  geom_text(aes(hjust = ifelse(diff > 0, 1, 0),
                x = ifelse(diff > 0, -3, 3),
                label = sapply(name_combined, function(x) gsub("infrastructure", "infrastruct-\nure", gsub(" ", "\n", sub(".*___", "", x))))),
            lineheight= .7, size = 2.5) +
  facet_nested(
    cols = vars(Region),
    rows = vars(cluster_name),
    scales = "free_y",
    independent = "y", switch = "y"
  ) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3", 
    "mixed" = "grey")) +
  labs(
    x = "",
    y = "",
    title = "Difference to region-specific median"
  ) +
  coord_cartesian(clip = "off") + 
  theme_SM() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    panel.spacing.x=unit(0, "lines"),
    panel.spacing.y=unit(0, "lines")
  ) +
  theme(legend.position = "none")
fig2_reg

df4 <- type_region_medians %>%
  ungroup() %>% 
  left_join(type_medians, by=c("var", "cluster_name")) %>%
  mutate(
    diff = reg_type_med - type_med,
    abs_diff = abs(diff)
  ) %>% 
  group_by(cluster_name, Region)

# 5. Plot top 3
fig2_type <- df4 %>% 
  rename_co_vars("var") %>%
  mutate(name_combined = paste(Region, cluster_name, var, sep = "___")) %>% 
  filter(!is.na(diff)) %>% 
  group_by(Region, cluster_name) %>% 
  arrange(-abs_diff, .by_group = T) %>% 
  slice_head(n = 3) %>% 
  mutate(name_combined = forcats::fct_reorder(name_combined, diff)) %>% 
  ggplot(aes(x = diff, y = name_combined)) +
  geom_col(aes(x = diff, fill = cluster_name), col = "black", size = .2) + 
  geom_vline(xintercept = 0, lty = 1, col = "grey30") + 
  geom_text(aes(hjust = ifelse(diff > 0, 1, 0),
                x = ifelse(diff > 0, -3, 3),
                label = sapply(name_combined, function(x) gsub(" ", "\n", sub(".*___", "", x)))),
            lineheight= .7, size = 2.5) +
  facet_nested(
    cols = vars(Region),
    rows = vars(cluster_name),
    scales = "free_y",
    independent = "y", switch = "y"
  ) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3", 
    "mixed" = "grey")) +
  labs(
    x = "",
    y = "",
    title = "Difference to type-specific median"
  ) +
  theme_SM() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    panel.spacing.x=unit(0.05, "lines"),
    panel.spacing.y=unit(1, "lines")
  ) +
  theme(legend.position = "none")


#######----

plot_cluster_region_matrix <- function(
    data,
    value = c("share", "n"),
    title = NULL,
    add_row_totals = TRUE,
    add_col_totals = TRUE,
    fill_low = "#56B1F7",
    fill_high = "#132B43"
) {
  value <- match.arg(value)
  
  # prepare cluster naming
  data <- data %>%
    mutate(cluster_name = ifelse(main_mixed == "mixed", "mixed", as.character(cluster_name)),
           cluster_name = factor(cluster_name, levels = c(levels(cluster_names$cluster_name), "mixed")))
  
  # base aggregation
  df <- data %>%
    group_by(cluster_name, Region) %>%
    summarise(n = n(), .groups = "drop")
  
  # compute share if requested
  if (value == "share") {
    df <- df %>%
      # group_by(Region) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()
  }
  
  # ----- Add totals -----
  
  if (add_row_totals) {
    row_tot <- df %>%
      group_by(cluster_name) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(Region = "Total")
    
    if (value == "share") row_tot$share <- row_tot$n / sum(df$n)
    
    df <- bind_rows(df, row_tot)
  }
  
  if (add_col_totals) {
    col_tot <- df %>%
      group_by(Region) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(cluster_name = "Total")
    
    if (value == "share") col_tot$share <- col_tot$n / sum(df$n)
    
    df <- bind_rows(df, col_tot)
  }
  
  # determine fill column
  df$fill_val <- if (value == "share") log2(df$share) else log2(df$n)
  
  # determine label text
  df$label <- if (value == "share") {
    paste0(round(df$share * 100, 2), "%")
  } else {
    df$n
  }
  
  # reorder cluster for plotting
  df$cluster_name <- factor(df$cluster_name, levels = rev(unique(df$cluster_name)))
  
  row_line <- 1.5
  
  # ----- Plot -----
  ggplot(df, aes(Region, cluster_name, fill = fill_val)) +
    geom_tile(width = .9, height = .9) +
    geom_text(aes(label = label), color = "white") +
    scale_fill_continuous(low = fill_low, high = fill_high) +
    labs(
      x = "",
      y = "",
      title = title %||% ifelse(value == "share", "Percentage of cities", "Number of urban centres")
    ) +
    geom_hline(yintercept = row_line, color = "grey20", size = 0.6) +
    facet_wrap(~Region, scales = "free_x", nrow = 1) +
    theme_SM() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      panel.border = element_blank(),
      panel.spacing.x = unit(0.05, "lines"),
      panel.spacing.y = unit(0.1, "lines")
    )
}

p_percentage_type_region <- plot_cluster_region_matrix(
  data = desc_geo_exlc,
  value = "share",
  title = "Percentage of urban centres",
  add_row_totals = FALSE
)
p_percentage_type_region

p_n_type_region <- plot_cluster_region_matrix(
  data = desc_geo_exlc,
  value = "n",
  title = "Absolute number of urban centres",
  add_row_totals = FALSE
)
p_n_type_region


fig2 <- ggarrange(
  ggarrange(fig2_reg, # fig3_type, 
            labels = c("a",  ""), nrow = 1), 
  ggarrange(p_percentage_type_region, p_n_type_region, labels = c("b", "c"), nrow = 2), 
  labels = c("", ""), nrow = 2, heights = c(1,1))

# ggsave(fig2, file = "plots/fig2.pdf", width = 10, height = 12)

################################################################################
# global cluster characterisation (fig 1)
################################################################################

p_attr_cluster_bar <- df1 %>% 
  rename_co_vars("var") %>% 
  mutate(var = gsub(" ", "\n", var)) %>% 
  mutate(var = factor(var, gsub(" ", "\n", co_vars_formatted))) %>% 
  ggplot(aes(name, diff, fill = name, pattern = name)) + 
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3", 
    "mixed" = "white")) +
  geom_col_pattern(
    width = .8,
    linewidth=.2,
    pattern_aspect_ratio = 2,
    pattern_res = 100,
    colour = "black",
    pattern_color = "grey",
    pattern_alpha = 0.6,
    pattern_density = 0.3,
    pattern_spacing = 0.05,
    pattern_size = .09,
    pattern_key_scale_factor = 1,
    position = "stack"
  ) +
  scale_pattern_manual(values = c("none", "none", "none", "none", "stripe")) +
  geom_hline(yintercept = 0, lty = 1, col = "grey60", size = .2) + 
  facet_wrap(var~.) + 
  labs(x = "", y = "Percentile difference", title = "Percentile deviation from global median") + 
  theme_SM() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8))

test_region_thresh_prob <- desc_geo_exlc %>%
  filter(!is.na(cluster_name)) %>% 
  group_by(cluster_name, main_mixed, Region) %>%
  summarise(n = n()) %>%
  group_by(Region) %>%
  mutate(share = n/sum(n)) %>%
  mutate(pct = round(share * 100, 1),
         pct = ifelse(pct <1, NA, paste0(pct, "%"))) %>% 
  mutate(Region = gsub(" ", "\n", Region),
         Region = factor(Region, levels = c(gsub(" ", "\n", reg_vars_wg2)))) %>% 
  ggplot(aes(Region, share, fill = cluster_name, pattern = main_mixed)) +
  geom_col_pattern(
    width = .4,
    linewidth=.2,
    pattern_aspect_ratio = 1,
    pattern_res = 200,
    colour = "black",
    pattern_color = "grey",
    pattern_alpha = 0.6,
    pattern_density = 0.3,
    pattern_spacing = 0.01,
    pattern_size = .05,
    pattern_key_scale_factor = 1,
    position = "stack"
  ) +
  scale_pattern_manual(values = c("none", "stripe")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = expansion(add = c(.5, .9))) +
  geom_text(aes(label = pct),
            position = position_stacknudge(x = .22, vjust = 0.5), size = 2, hjust = 0) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3")) +
  theme_SM() +
  labs(x = "", y = "Percentage by region", title = "Type percentage by region") +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = "none")
    ),
    pattern = guide_legend(
      override.aes = list(fill = "white", colour = "black")
    )
  ) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5, size = 8),
  )
test_region_thresh_prob

to_label <- desc_geo_exlc %>% 
  filter(GHS_population >= .8 & main_mixed != "mixed") %>% 
  group_by(cluster_name, Region) %>% 
  arrange(-mean_prob) %>% 
  slice_max(mean_prob, n = 3) %>% 
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region, cluster_name, centroid) 

to_label

global_min_pop <- min(desc_geo_exlc$GHS_population, na.rm = TRUE)
global_max_pop <- max(desc_geo_exlc$GHS_population, na.rm = TRUE)

make_map <- function(df, type) {
  df %>%
    filter(cluster_name == type) %>%
    mutate(cluster_name = ifelse(main_mixed == "mixed", NA, as.character(cluster_name)),
           cluster_name = factor(cluster_name)) %>% 
    ggplot() +
    geom_sf(data = world, fill = "grey90", color = "white") +
    geom_sf(aes(geometry = centroid,
                col = cluster_name, size = GHS_population, ), shape = 16, alpha = .5,
            lwd = 0) +
    geom_sf(data = bb, col = "grey90", fill = NA, linewidth = .5) +
    ggrepel::geom_label_repel(
      data = to_label %>% filter(cluster_name %in% unique(type, "mixed")),
      aes(label = GC_UCN_MAI_2025, geometry = centroid),
      stat = "sf_coordinates", alpha = .5, size = 3.5
    ) +
    scale_color_manual(values = c(
      "Type 1" = "#E41A1C",
      "Type 2" = "#377EB8",
      "Type 3" = "#4DAF4A",
      "Type 4" = "#984EA3"),
      na.value = "grey30") +
    scale_size(range = c(.05,7), 
               limits = c(global_min_pop, global_max_pop),
    ) +
    labs(x ="", y="") +
    coord_sf(
      ylim = c(-6500000, 8650000),
      xlim = c(-18000000, 18000000),
      expand = FALSE
    ) +
    theme_map() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = NA),
      legend.position = "none",
      title = element_text(face = "plain"),
      plot.margin = margin(c(-0,-2,-1,-2), "cm")
    )
}
library(patchwork)
library(grid)

# Create your four maps
p1 <- make_map(desc_geo_exlc, "Type 1")
p2 <- make_map(desc_geo_exlc, "Type 2")
p3 <- make_map(desc_geo_exlc, "Type 3")
p4 <- make_map(desc_geo_exlc, "Type 4")

base <- plot_grid(p1, p2, p3, p4, ncol = 1, align = "v")
title <- ggdraw() + draw_label("Four main types of urban centres")
base <- plot_grid(title, base, ncol=1, rel_heights=c(0.03, 1)) 

fig1_bc <- ggarrange(p_attr_cluster_bar, 
                     test_region_thresh_prob, 
                     labels = c("b", "c"), align = "v",
                     ncol = 1)

fig1 <- ggarrange(base, fig1_bc, labels = c("a", ""), widths = c(1.4,1))
ggsave(fig1, file = "plots/fig_robust_rural_types.pdf", width = 10, height = 10)


################################################################################
# Cities per cluster with and without research
################################################################################

p_cities_per_cluster_n <- clust %>% 
  mutate(
    any_study_true = ifelse(n_studies > 1 & !is.na(n_studies), "Has research", "No research")
  ) %>% 
  group_by(consensus_label_majority, any_study_true) %>% 
  summarise(n_cities = n(), .groups = "drop") %>% 
  group_by(consensus_label_majority) %>% 
  mutate(
    total_cities = sum(n_cities),
    pct = round((n_cities / total_cities) * 100, 0),
    label = paste0(pct, "%")
  ) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  mutate(
    any_study = any_study_true,
    any_study = case_when(
      any_study_true == "Has research" & cluster_name == "Type 3" ~ "Type 3",
      any_study_true == "Has research" & cluster_name == "Type 4" ~ "Type 4",
      any_study_true == "Has research" & cluster_name == "Type 1" ~ "Type 1",
      any_study_true == "Has research" & cluster_name == "Type 2" ~ "Type 2",
      TRUE ~ any_study
    ),
    any_study = factor(
      any_study,
      levels = c(
        "Type 1", 
        "Type 2", 
        "Type 3",
        "Type 4", 
        "No research"
      )
    ),
    cluster_name = factor(cluster_name, levels = rev(levels(cluster_name)))
  ) %>% 
  ggplot(aes(x = cluster_name, y = n_cities, fill = any_study, pattern = any_study)) + 
  geom_col_pattern(
    width = .4,
    linewidth=.2,
    colour = "black",
    pattern_color = "black",
    pattern_alpha = 0.2,
    pattern_density = 0.1,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 1,
    stat = "identity", position = "stack"
  ) +
  geom_text(
    aes(label = label, y = ifelse(n_cities < 200, n_cities + 1300, n_cities)),
    position = position_stacknudge(x = .4, vjust = 0.5),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Type 1" = "#E41A1C",
      "Type 2" = "#377EB8",
      "Type 3" = "#4DAF4A",
      "Type 4" = "#984EA3",
      "No research" = "#f2f2f0"
    ),
    breaks = "No research"
  ) +
  scale_pattern_manual(
    values = c(
      "Type 1" = "none",
      "Type 2" = "none",
      "Type 3" = "none",
      "Type 4" = "none",
      "No research" = "stripe"
    ),
    breaks = "No research"
  ) + 
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  theme_SM() +
  theme(
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    legend.position = c(0.9,.05),
    axis.title = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title.position = "plot"
  ) +
  labs(x = "", y = "",  subtitle = "Number of urban centres")
p_cities_per_cluster_n

################################################################################
# Number of studies per cluster
################################################################################

p_n_studies <- clust %>% 
  group_by(cluster_name) %>% 
  summarise(n_studies = sum(n_studies)) %>% 
  mutate(cluster_name = factor(cluster_name, levels = rev(levels(cluster_name)))) %>%
  ggplot(aes(x = cluster_name, y = n_studies, fill = cluster_name)) +
  geom_bar(stat = "identity", width = .5, col = "black", size = 0.1) +
  coord_flip() +
  scale_fill_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  theme_SM() +
  theme(
    legend.title = element_text(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = .5),
    plot.title.position = "plot"
  ) +
  labs(x = "", y = "", subtitle = "Total urban centres studied")


################################################################################
# Similarity scores & Teaching/Learning Potentials
################################################################################
min_max_scale <- function(x){
  res = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(res)
}

co_mat <- clust %>%
  as.data.frame() %>%
  select(GHS_urban_area_id, all_of(co_vars), consensus_label_majority) %>%
  left_join(cluster_names, by = "consensus_label_majority")


## -----------------------------------------------------------
## 1. Prepare covariate matrix
## -----------------------------------------------------------

X <- co_mat %>%
  select(all_of(co_vars)) %>%
  scale() %>%
  as.matrix()


## -----------------------------------------------------------
## 2. Identify study cities
## -----------------------------------------------------------

n_studies_all_cities <- ghsl %>% 
  as.data.frame() %>% 
  select(ID_UC_G0) %>% 
  left_join(n_studies_per_city, by = c("ID_UC_G0" = "city_id")) %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))

study_ids <- n_studies_all_cities %>%
  filter(n_studies >= 1) %>%
  pull(ID_UC_G0)

study_rows <- match(study_ids, co_mat$GHS_urban_area_id)


## -----------------------------------------------------------
## 3. Compute cosine similarity between all cities and study cities
##    (rectangular similarity matrix)
## -----------------------------------------------------------

library(proxy)

sim_matrix_with_studies <- proxy::simil(
  X,
  X[study_rows, , drop = FALSE],
  method = "cosine",
  by_rows = TRUE
)

sim_matrix_with_studies <- as.matrix(sim_matrix_with_studies)

# optional: scale cosine [-1,1] → [0,1]
sim_with_studies <- (sim_matrix_with_studies + 1) / 2


sim_matrix_all <- proxy::simil(
  X,
  X,
  method = "cosine",
  by_rows = TRUE
)

sim_matrix_all <- as.matrix(sim_matrix_all)

# optional: scale cosine [-1,1] → [0,1]
sim_all <- (sim_matrix_all + 1) / 2


## -----------------------------------------------------------
## 4. Compute similarity score per city
## -----------------------------------------------------------

overall_similarity_with_studies <- rowMeans(sim_with_studies, na.rm = TRUE)
overall_similarity_all <- rowMeans(sim_all, na.rm = TRUE)

min_max_scale <- function(x){
  (x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

co_mat <- co_mat %>%
  mutate(
    similarity_raw_all = overall_similarity_all,
    similarity_raw_all_scaled = min_max_scale(similarity_raw_all),
    similarity_raw_with_studies = overall_similarity_with_studies,
    similarity_raw_with_studies_scaled = min_max_scale(similarity_raw_with_studies)
  )


## -----------------------------------------------------------
## 5. Prepare hex grid and city–hex intersection
## -----------------------------------------------------------

world <- st_transform(world, proj_robin)

world_hex <- st_make_grid(
  world,
  n = c(190, 190),
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE
) %>%
  st_as_sf() %>%
  mutate(hex_id = seq_len(n()))

intersections <- st_intersection(
  ghsl %>% 
    select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>% 
    st_transform(proj_robin),
  world_hex
) %>%
  mutate(inter_area = st_area(geom))

intersections_unique <- intersections %>%
  group_by(ID_UC_G0) %>%
  slice_max(inter_area, n = 1) %>%
  ungroup() %>%
  as.data.frame() %>%
  select(-geom)


## -----------------------------------------------------------
## 6. Compute mean similarity per hexagon
## -----------------------------------------------------------

hexa_data <- world_hex %>%
  left_join(intersections_unique, by = "hex_id") %>%
  left_join(
    co_mat %>% select(similarity_raw_all_scaled, similarity_raw_with_studies_scaled, GHS_urban_area_id),
    by = c("ID_UC_G0" = "GHS_urban_area_id")
  ) %>%
  group_by(hex_id) %>%
  summarise(similarity_raw_all_scaled = mean(similarity_raw_all_scaled, na.rm = TRUE),
            similarity_raw_with_studies_scaled = mean(similarity_raw_with_studies_scaled, na.rm = TRUE)) 

table(is.na(hexa_data$similarity_raw_all_scaled), is.na(hexa_data$similarity_raw_with_studies_scaled))

## -----------------------------------------------------------
## 7. Plot similarity map
## -----------------------------------------------------------

p_similarity_map_all <- hexa_data %>%
  filter(!is.na(similarity_raw_all_scaled)) %>% 
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(aes(fill = similarity_raw_all_scaled), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = "grey") +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Similarity to all urban centres",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) +
  theme_SM() +
  labs(y = "", x = "", fill = "Similarity") +
  theme(
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.position = c(.5, .1),
    legend.title = element_text(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1, 0, 0, 0), "cm")
  )

p_similarity_map_with_studies <- hexa_data %>%
  filter(!is.na(similarity_raw_with_studies_scaled)) %>% 
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(aes(fill = similarity_raw_with_studies_scaled), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = "grey") +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Similarity to urban centres with studies",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) +
  theme_SM() +
  labs(y = "", x = "", fill = "Similarity") +
  theme(
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.position = c(.5, .1),
    legend.title = element_text(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1, 0, 0, 0), "cm")
  )

## -----------------------------------------------------------
## 8. Plot similarity as bar chart
## -----------------------------------------------------------

similarity_by_type_with_studies <- co_mat %>%
  group_by(cluster_name) %>% 
  summarise(similarity_mean = mean(similarity_raw_with_studies_scaled)) %>% 
  mutate(cluster_name = factor(cluster_name, levels = rev(levels(cluster_name)))) %>%
  ggplot(aes(x = cluster_name, y = similarity_mean, fill = cluster_name)) +
  geom_bar(stat = "identity", width = .5, col = "black", size = 0.1) +
  scale_fill_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
  coord_flip() +
  theme_SM() +
  theme(
    legend.title = element_text(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = .5),
    plot.title.position = "plot"
  ) +
  labs(x = "", y = "", subtitle = "Similarity to urban centres with studies")
similarity_by_type_with_studies

similarity_by_type_all <- co_mat %>%
  group_by(cluster_name) %>% 
  summarise(similarity_mean = mean(similarity_raw_all_scaled)) %>% 
  mutate(cluster_name = factor(cluster_name, levels = rev(levels(cluster_name)))) %>%
  ggplot(aes(x = cluster_name, y = similarity_mean, fill = cluster_name)) +
  geom_bar(stat = "identity", width = .5, col = "black", size = 0.1) +
  scale_fill_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
  coord_flip() +
  theme_SM() +
  theme(
    legend.title = element_text(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = .5),
    plot.title.position = "plot"
  ) +
  labs(x = "", y = "", subtitle = "Similarity to all urban centres")
similarity_by_type_all

################################################################################
# Learning potential on map
################################################################################

learn_pot <- co_mat %>%
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0"))

map_dot_dat <- ghsl %>%
  st_transform(proj_robin) %>%
  select(ID_UC_G0, GC_UCN_MAI_2025) %>%
  mutate(geom = st_centroid(geom)) %>%
  left_join(learn_pot %>% select(- GC_UCN_MAI_2025, -GC_CNT_GAD_2025, -geom), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  left_join(n_studies_all_cities, by = c("ID_UC_G0")) %>% 
  mutate(has_research = n_studies != 0)

p_types_as_dots_with_research <- ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(
    data = map_dot_dat %>% filter(has_research),
    aes(col = cluster_name, size = n_studies),
    alpha = .5
  ) +
  scale_size_continuous(range = c(0.01, 5)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Case study coverage of urban centres by type",
    hjust = -0.2, vjust = 1.7,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) + 
  theme_SM() +
  labs(
    y = "", x = "", col = "Urban centre type", size = "Number of studies"
  ) +
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2)) +
  theme(
    legend.position = c(.045, .35),
    legend.direction = "vertical",
    legend.justification = "left",
    legend.title = element_text(),
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1, 0, 0, 0), "cm")
  )

p_types_as_dots_without_research <- ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(
    data = map_dot_dat %>% filter(!has_research),
    aes(col = cluster_name),
    size = .01
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Urban centres by type not covered by any studies",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) + 
  theme_SM() +
  labs(
    y = "", x = "", col = "City type"
  ) +
  theme(
    legend.position = c(.05, .8),
    legend.direction = "vertical",
    legend.justification = "left",
    legend.title = element_text(),
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1, 0, 0, 0), "cm")
  )

no_research <- map_dot_dat %>%
  filter(!has_research) %>%
  st_transform(proj_robin)

#---------------------------------------------------------
# 5. Assign cities (points or polygons) to grid hexes
#---------------------------------------------------------
no_res_hex <- st_join(
  no_research,
  world_hex %>% select(hex_id),
  join = st_within
)

#---------------------------------------------------------
# 6. Count number of cities without research, per hex
#---------------------------------------------------------
hex_counts <- no_res_hex %>%
  st_drop_geometry() %>%
  count(hex_id, name = "n_cities_no_research")

#---------------------------------------------------------
# 7. Add counts back onto the hex geometry
#---------------------------------------------------------
world_hex_with_counts <- world_hex %>%
  left_join(hex_counts, by = "hex_id") %>%
  filter(!is.na(n_cities_no_research))

#---------------------------------------------------------
# 8. (Optional) Plot the hex map
#---------------------------------------------------------
library(ggplot2)

breaks_raw <- c(1, 2, 4, 8, 16, 32, 64)
breaks_log <- log2(breaks_raw)

p_count_without_research <- ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(data = world_hex_with_counts, 
          aes(fill = log2(n_cities_no_research)), color = NA) +
  scale_fill_gradientn(colors = c("grey", "black"),
                       breaks = breaks_log,
                       labels = breaks_raw) +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Count of urban centres not covered by any study",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) +
  theme_SM() +
  labs(y = "", x = "", fill = "Number of urban centres without research") +
  theme(
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.position = c(.5, .1),
    legend.title = element_text(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1, 0, 0, 0), "cm")
  )

################################################################################
# Combine plots into Figure 4
################################################################################

fig4bcde <- ggarrange(
  p_similarity_map_all, p_similarity_map_with_studies,
  p_types_as_dots_without_research + theme(legend.position = "none"), 
  p_count_without_research,
  labels = c("b", "c", "d", "e")
)

fig4abcde <- ggarrange(
  p_types_as_dots_with_research,
  fig4bcde,
  labels = c("a", ""),
  ncol = 1,
  heights = c(1, 1)
)

fig4fgh <- ggarrange(
  p_n_studies + theme(text = element_text(size = 9)),
  similarity_by_type_all + theme(text = element_text(size = 9)),
  similarity_by_type_with_studies + theme(text = element_text(size = 9)),
  p_cities_per_cluster_n + theme(text = element_text(size = 9)),
  hjust = c(.5,.5,.5,.5),
  align = "h", labels = c("e", "f", "g", "h"),
  ncol = 1
)

fig4 <- ggarrange(fig4abcde, fig4fgh, labels = c("", ""), ncol = 2, widths = c(3.2, 1))

ggsave(fig4, file = "plots/fig_robust_rural_learn.pdf", height = 8, width = 10)

