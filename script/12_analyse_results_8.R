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
             # "GHS_greenness_index", 
             # "GHS_precipitation",
             "hdd",
             "cdd"
             # "GHS_land_cons",
             # , "GHS_hosp_pc"
             # , "GHS_road_len"
             # , "odiac_norm"
)
co_vars_formatted <- c("Population", "Population growth", "Population density", "Population density growth", 
                       "65+ population share", "HDI", "Gender index", "GDP PPP", "GDP PPP growth", 
                       "Critical infrastructure", 
                       # "Greenness", 
                       # "Precipitation",
                       "Heating degree days", 
                       "Cooling degree days"
                       # "Land consumption",
                       # , "Hospitals p.c."
                       # , "Road density"
                       # , "Emissions p.c."
)
reg_vars <- c("NORTH-AMERICA", "SOUTH-AMERICA", "EUROPE", "AFRICA", "ASIA", "OCEANIA" , "SMALL ISLANDS")
reg_vars_wg2 <- c("North America", "South America", "Europe", "Africa", "Asia", "Australasia", "Small Islands")

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Type 2",
    "Type 3",
    "Type 1", 
    "Type 4"
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
# ipcc_cont <- st_transform(ipcc_cont, proj_robin)

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
    "GHS_old_pop" =  "65+ population share",
    "odiac_norm" = "CO2 emissions p.c."
  )
  
  column <- rlang::ensym(column)
  
  df %>%
    mutate(!!column := recode(!!column, !!!rename_map))
}


################################################################################
# recode variables and add ipcc continents
################################################################################

ipcc_regions %>% 
  ggplot() +
  geom_sf(data = world) +
  geom_sf(aes(fill = Type == "Ocean"), alpha= .3) +     
  geom_sf(data = ghsl %>% mutate(geom = st_centroid(geom)), aes(geometry = geom), alpha = .5, size = .5) + 
  geom_label(
    aes(label = Acronym, geometry = st_centroid(geometry)),
    stat = "sf_coordinates", alpha=.5, size = 2,
  ) 

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
# quick look at the data
################################################################################
ggplot(ipcc_regions_hexa) +
  geom_sf() +
  geom_label(
    aes(label = label, geometry = geom),
    stat = "sf_coordinates", alpha=.5, size = 2, 
  ) 

ipcc_regions %>% 
  st_join(ghsl %>% mutate(geom = st_centroid(geom)) %>% st_transform(4326) %>% select("ID_UC_G0")) %>% 
  filter(!is.na(ID_UC_G0)) %>% 
  select(-ID_UC_G0) %>% 
  group_by(Acronym) %>% 
  slice(1) %>% 
  ggplot() +
  geom_sf(aes(fill = Type == "Ocean"), alpha= .3) +     
  geom_label(
    aes(label = Acronym, geometry = st_centroid(geometry)),
    stat = "sf_coordinates", alpha=.5, size = 2, 
  ) +
  geom_sf(data = ghsl %>% mutate(geom = st_centroid(geom)), aes(geometry = geom))

ggplot(ipcc_regions_hexa) + 
  geom_sf(fill = NA) +
  geom_label(
    aes(label = label, geometry = geom),
    stat = "sf_coordinates", alpha=.5, size = 2, 
  ) 

ipcc_regions <- ipcc_regions %>% 
  mutate(Acronym = ifelse(Type == "Ocean", "PAC", Acronym)) 

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

# ################################################################################
# # summary statistics: regional deviations
# ################################################################################
# 
# t <- clust %>%
#   left_join(cluster_names, by = "consensus_label_majority")
# 
# median(t$GHS_GDP_PPP[t$Region == "Africa"])
# median(t$GHS_GDP_PPP[t$cluster_name == "Type 2" & t$Region == "Africa"])
# # clust %>% 
# #   filter(cluster_name )
# 
# winsorize <- function(x, p = c(0.01, 0.99)) {
#   quant <- quantile(x, p, na.rm = TRUE)
#   x[x < quant[1]] <- quant[1]
#   x[x > quant[2]] <- quant[2]
#   x
# }
# 
# min_max_scale <- function(x) {
#   (x - min(x, na.rm = TRUE)) / 
#     (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# 
# 
# # # 1. Global z-normalization once
# clust_z <- clust %>%
#   mutate(across(all_of(co_vars), ~ min_max_scale(winsorize(.x))))
# 
# # clust_z <- clust
# 
# # 2. Regional medians (on globally standardized data)
# reg_medians <- clust_z %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(cols = all_of(co_vars), values_to = "reg_medians")
# 
# # 3. Region × type medians (on the SAME globally standardized data)
# reg_type_medians <- clust_z %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region, cluster_name) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(cols = all_of(co_vars), values_to = "reg_type_medians")
# 
# reg_type_medians %>%
#   left_join(reg_medians, by = c("Region", "name")) %>% 
#   filter(cluster_name == "Type 2", Region == "Africa") %>% 
#   mutate(
#     diff = reg_type_medians - reg_medians,
#     abs_diff = abs(diff)
#   ) %>% 
#   slice_max(abs_diff, n = 3) 
# 
# # 4. Join + difference
# df <- reg_type_medians %>%
#   left_join(reg_medians, by = c("Region", "name")) %>%
#   mutate(
#     diff = reg_type_medians - reg_medians,
#     abs_diff = abs(diff)
#   )
# 
# # 5. Plot top 3
# fig2x <- df %>% 
#   rename_co_vars("name") %>%
#   mutate(name_combined = paste(Region, cluster_name, name, sep = "___")) %>% 
#   filter(!is.na(diff)) %>% 
#   group_by(Region, cluster_name) %>% 
#   slice_max(abs_diff, n = 3) %>% 
#   mutate(name_combined = forcats::fct_reorder(name_combined, diff)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = diff, y = name_combined)) +
#   geom_point(aes(x = diff, col = cluster_name),
#              size = 2, alpha = 1, shape = 17) +
#   geom_vline(xintercept = 0, lty = 3, col = "grey") + 
#   geom_text(aes(hjust = ifelse(diff > 0, 1, 0),
#                 x = ifelse(diff > 0, -.05, .05),
#                 label = sapply(name_combined, function(x) sub(" ", "\n", sub(".*___", "", x)))),
#             lineheight= .8, size = 2.7) +
#   # scale_y_discrete(labels = function(x) sub(".*___GDP PPP growth", "💰", x)) +
#   # scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
#   facet_grid2(
#     rows = vars(Region),
#     cols = vars(cluster_name),
#     scales = "free_y",
#     independent = "y"
#   ) +
#   scale_color_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
#   # scale_y_discrete(labels = c(
#   #   # "GDP PPP" = "💰"
#   #   # "Population" = "👥",
#   #   # "Greenness" = "🌿",
#   #   # "Infra" = "🏗️"
#   # )) + 
#   labs(
#     x = "",
#     y = ""
#     # title = "Regional Median vs. Cluster-Type Median",
#   ) +
#   theme_SM() +
#   theme(
#     # axis.text.y = element_markdown(
#     #   family = "Apple Color Emoji",   # macOS emoji font
#     #   size = 12
#     # )
#     axis.ticks = element_blank(),
#     axis.text.y = element_blank()
#   ) +
#   theme(legend.position = "bottom")
# 
# fig2x
# ggsave(fig2x, file = "plots/fig2x.pdf", height = 8, width = 10)




# 
# winsorize <- function(x, p = c(0.01, 0.99)) {
#   q <- quantile(x, p, na.rm = TRUE)
#   x[x < q[1]] <- q[1]
#   x[x > q[2]] <- q[2]
#   x
# }
# 
# min_max_scale <- function(x) {
#   (x - min(x, na.rm = TRUE)) / 
#     (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# # GLOBAL winsorized + minmax-scaled dataset
# clust_scaled <- clust %>%
#   mutate(across(all_of(co_vars),
#                 ~ min_max_scale(winsorize(.x)),
#                 .names = "{.col}"))
# 
# reg_medians_scaled <- clust_scaled %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(all_of(co_vars),
#                names_to = "indicator",
#                values_to = "reg_scaled")
# 
# reg_type_medians_scaled <- clust_scaled %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region, cluster_name) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(all_of(co_vars),
#                names_to = "indicator",
#                values_to = "type_scaled")
# 
# ranking <- reg_type_medians_scaled %>%
#   left_join(reg_medians_scaled, by = c("Region", "indicator")) %>%
#   mutate(
#     diff_scaled = type_scaled - reg_scaled,
#     abs_diff_scaled = abs(diff_scaled)
#   ) %>%
#   group_by(Region, cluster_name) %>%
#   slice_max(abs_diff_scaled, n = 3, with_ties = FALSE)
# 
# # raw region medians
# reg_medians_raw <- clust %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(all_of(co_vars),
#                names_to = "indicator",
#                values_to = "reg_raw")
# 
# # raw type medians
# reg_type_medians_raw <- clust %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   group_by(Region, cluster_name) %>%
#   summarise(across(all_of(co_vars), median, na.rm = TRUE)) %>%
#   pivot_longer(all_of(co_vars),
#                names_to = "indicator",
#                values_to = "type_raw")
# 
# interp <- ranking %>%
#   left_join(reg_medians_raw,  by = c("Region", "indicator")) %>%
#   left_join(reg_type_medians_raw, by = c("Region", "cluster_name", "indicator")) %>%
#   mutate(
#     raw_ratio = type_raw / reg_raw,
#     raw_diff = type_raw - reg_raw,
#     interpretable_label = case_when(
#       raw_ratio >= 1 ~ paste0(round(raw_ratio, 1), "× higher"),
#       raw_ratio <  1 ~ paste0(round(1/raw_ratio, 1), "× lower")
#     )
#   ) %>% 
#   rename_co_vars("indicator") 
# 
# ggplot(interp, aes(x = reg_raw, xend = type_raw, y = indicator)) +
#   geom_segment(color = "grey60") +
#   geom_point(aes(x = reg_raw), color = "black", size = 2) +
#   geom_point(aes(x = type_raw, color = cluster_name), size = 2) +
#   facet_grid2(rows = vars(Region),
#               cols = vars(cluster_name),
#               scales = "free_y",
#               independent = "y") +
#   labs(
#     x = "Raw Value (Median)",
#     y = "Indicator",
#     title = "Interpretable Differences: Raw Medians",
#     subtitle = "Indicators selected based on scaled differences"
#   ) +
#   theme_SM()
# 
# ggplot(interp, aes(x = raw_ratio, y = indicator, fill = cluster_name)) +
#   geom_col() +
#   geom_vline(xintercept = 1, lty = 2) +
#   facet_grid2(rows = vars(Region), cols = vars(cluster_name),
#               scales = "free_y",
#               independent = "y") +
#   geom_text(aes(hjust = ifelse(raw_ratio > 1, 1, 0),
#                 x = ifelse(raw_ratio > 1, .95, 1.05),
#                 label = sapply(indicator, function(x) sub(" ", "\n", sub(".*___", "", x)))),
#             lineheight= .8, size = 2.7) +
#   scale_x_continuous(transform = "log2") +
#   labs(
#     x = "Type Median / Regional Median",
#     y = "Indicator",
#     title = "Multiplicative Differences (Interpretation-Friendly)"
#   ) +
#   theme_SM() +
#   theme(
#     # axis.text.y = element_markdown(
#     #   family = "Apple Color Emoji",   # macOS emoji font
#     #   size = 12
#     # )
#     axis.ticks = element_blank(),
#     axis.text.y = element_blank()
#   ) 
# 
# 
# 
# ################################################################################
# # summary statistics: correlations
# ################################################################################
# 
# # Compute correlation matrix for numeric columns only
# cor_mat <- cor(desc_dat %>% 
#                  # left_join(coastal_cities, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  # left_join(infra, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  left_join(gender, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  left_join(hdi, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  # left_join(green, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  left_join(lecz, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  left_join(emmissions_box_dat %>% select(odiac_norm, ID_UC_G0), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#                  dplyr::select(co_vars, EX_LEC_SHP_2025, IN_CIS_ALL_2020, SC_SEC_GDF_2020, SC_SEC_HDI_2020, GR_AVG_GRN_2025, odiac_norm), use = "complete.obs")
# 
# diag(cor_mat) <- NA
# cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
# 
# # Convert to long format
# cor_df <- as.data.frame(cor_mat) %>%
#   rownames_to_column(var = "Var1") %>%
#   pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>% 
#   filter(!is.na(Correlation)) %>% 
#   mutate(Var1 = factor(Var1, levels = c(co_vars, "dist_to_coast_km", "EX_LEC_SHP_2025", "IN_CIS_ALL_2020", "SC_SEC_GDF_2020", "SC_SEC_HDI_2020", "GR_AVG_GRN_2025", "odiac_norm")),
#          Var2 = factor(Var2, levels = c(co_vars, "dist_to_coast_km", "EX_LEC_SHP_2025", "IN_CIS_ALL_2020", "SC_SEC_GDF_2020", "SC_SEC_HDI_2020", "GR_AVG_GRN_2025", "odiac_norm"))) %>%
#   rename_co_vars("Var1") %>%
#   rename_co_vars("Var2")
# 
# # Plot heatmap with labels
# ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
#   geom_tile() +
#   geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0, limit = c(-1, 1), name = "Correlation") +
#   theme_SM() +
#   theme(axis.title = element_blank(),
#         legend.direction = "horizontal",
#         legend.title = element_text(),
#         ) +
#   coord_fixed()
# 



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


library(see)

p_emissions_box <- ggplot(emmissions_box_dat, aes(x = cluster_name, y = odiac_norm, fill = median_odiac)) +
  geom_hline(yintercept = median(median_data$median_odiac, na.rm = TRUE), lty = 3, col = "grey50") +
  geom_violinhalf(outliers = FALSE, trim = TRUE, 
                  side = "l",
                  alpha = 0.6, 
                  lwd = 0.2, 
                  flip = TRUE
  ) +
  geom_boxplot(outliers = FALSE, outlier.size = 0.5, alpha = 0.8, width = 0.3, na.rm = TRUE, lwd = 0.2, position = position_nudge(x = 0.15)) +
  scale_fill_gradient2(
    low = "#a0c4ff", mid = "white", high = "#FFADAD",
    midpoint = median(median_data$median_odiac, na.rm = TRUE),
    name = "Median Emissions"
  ) +
  labs(
    x = "",
    y = "Emissions p.c. (t CO₂ p.a.)"
  ) +
  # facet_wrap(~Region, nrow = 1) +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(p_emissions_box, file = "plots/p_emissions_box.pdf", width = 10, height = 5)



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

to_label %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region, cluster_name)

box_plot_list <- list()
box_plot_add_covs_just_point <- list()
p_types_map <- list()
# min_cl <- min(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
# max_cl <- max(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
for (cluster in cluster_names$cluster_name) {
  
  # Plot the maps with centroids
  p_types_map[[cluster]] <- desc_geo %>%
    filter(cluster_name == cluster) %>%
    ggplot() +
    geom_sf(data = world, fill = "grey90", color = "white") +  # World map with light gray color
    geom_sf(aes(geometry = centroid,
                col = mean_prob, fill = mean_prob, alpha = mean_prob, size = mean_prob), lwd = 0) +
    ggrepel::geom_label_repel(
      data = to_label %>%
        filter(cluster_name == cluster),
      aes(label = GC_UCN_MAI_2025,
          geometry = centroid),
      stat = "sf_coordinates", alpha=.5, size = 3.5,
    ) +
    scale_color_gradient2(low="white", mid="#ffe0a3", high="#963d03",
                          limits = c(0, 1), oob = scales::squish) +
    scale_alpha_continuous(range = c(0.05, 1)) +
    scale_size_continuous(range = c(.05, 1)) +
    geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
    coord_sf(crs = proj_robin) +
    # annotate(
    #   "label",
    #   x = -Inf, y = Inf,
    #   label = cluster_names$cluster_name[cluster_names$cluster_name == cluster],
    #   hjust = -0.1, vjust = 1.1,
    #   size = 4,
    #   fill = "white",
    #   label.size = 0.3
    # ) +
    theme_SM() +
    theme(
      panel.border = element_rect(color = NA),
      legend.position = "none",
      plot.margin = margin(c(-1,-2,-1,-2), "cm")
    ) +
    labs(col = "Cities", x = "", y = "")
  
  # Compute means by cluster and variable
  means_df <- box_plot_add_covs_dat %>%
    group_by(cluster_name, variable) %>%
    summarise(mean_val = mean(normalized_value, na.rm = TRUE), .groups = "drop") %>%
    filter(cluster_name == cluster) %>%
    select(-cluster_name)
  
  show_legend <- FALSE
  show_legend <- cluster == "Type 1"
  
  # Plot
  box_plot_add_covs_just_point[[cluster]] <- box_plot_add_covs_dat %>%
    filter(cluster_name == cluster) %>%
    select(-cluster_name) %>%
    ggplot(aes(x = variable, y = normalized_value)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_violin(alpha = 0.9, color = NA, scale = "width", aes(fill = clustering)) +
    scale_fill_manual(values = c("#ffe0a3", "cornflowerblue")) +
    geom_boxplot(width = 0.15, outlier.size = 0.5, color = "grey", outliers = F) +
    geom_point(
      data = means_df,
      aes(x = variable, y = mean_val, shape = "Mean"),
      size = 2, fill = "darkred", color = "black", inherit.aes = FALSE
    ) +
    geom_label(
      data = means_df,
      aes(x = variable, y = mean_val, label = round(mean_val, 2), vjust = ifelse(mean_val >=8,1.2,-.8)),
      size = 3, fill = "white", color = "black",
      label.size = 0, alpha = 0.7, inherit.aes = FALSE
    ) +
    scale_shape_manual(values = c("Mean" = 21), name = "") +
    scale_y_continuous(
      trans = "log2",
      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
      labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
      limits = c(0.05, 19)
    ) +
    theme_SM() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = if (show_legend) c(.89, .8) else "none",
      legend.justification = "center",
      legend.box.just = "center",
      legend.box.background = element_rect(color = "grey", size = 0.2),
      legend.box.margin = margin(rep(2, 4)),
      legend.background = element_blank(),
      legend.spacing.y = unit(0.01, "lines"),
      legend.box = "vertical",
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9),
      plot.margin = margin(c(-6,3,-2,3), "cm")
    ) +
    labs(
      x = "",
      y = "Normalized value",
      title = ""
    )
  
  plot.with.inset <-
    ggdraw() +
    draw_plot(p_types_map[[cluster]], y = .25) +
    draw_plot(box_plot_add_covs_just_point[[cluster]], x = 0, y = 0, width = .98, height = .6)
  
  plot.with.inset
  
  box_plot_list[[cluster]] <- plot.with.inset
}

# order
box_plot_list <- box_plot_list[levels(cluster_names$cluster_name)]
fig1_old <- plot_grid(plotlist = box_plot_list, ncol = 2, labels = "auto", align = "v")
ggsave(fig1_old, filename = "plots/fig1_old.pdf", width = 10, height = 10)

# ggsave(box_plot_list[[1]], filename = "plots/type_1.pdf", width = 5, height = 3.8)
# ggsave(box_plot_list[[2]], filename = "plots/type_2.pdf", width = 5, height = 3.8)
# ggsave(box_plot_list[[3]], filename = "plots/type_3.pdf", width = 5, height = 3.8)
# ggsave(box_plot_list[[4]], filename = "plots/type_4.pdf", width = 5, height = 3.8)


################################################################################
# mixed type assignment
################################################################################

desc_geo_exlc <- desc_geo %>%
  as.data.frame() %>%
  group_by(cluster_name) %>%
  mutate(
    main_mixed = ifelse(mean_prob < .65*median(mean_prob), "mixed", "main type"),
  )
table(desc_geo_exlc$cluster_name, desc_geo_exlc$main_mixed)


write.csv(as.data.frame(desc_geo_exlc) %>% 
            select(ID_UC_G0, GC_UCN_MAI_2025, cluster_name, main_mixed),
          "data/clustering_results/type_main_mixed.csv")


## where should the cutoff lie ?
# 1) test differences
res_wilcox <- wilcox.test(mean_prob ~ main_mixed, data = desc_geo_exlc) 

# 2) overlap of CIs and median by group?
cluster_stats <- desc_geo_exlc %>% 
  group_by(cluster_name) %>%
  summarise(
    se = sd(mean_prob, na.rm =T) / sqrt(n()),
    median_prob = median(mean_prob),
    mean_prob = mean(mean_prob),
    ci_low = mean_prob - 1.96*se,
    ci_high = mean_prob + 1.96*se,
  ) %>% 
  mutate(cutoff = .65 * median_prob,)

p_assign_probs <- desc_geo %>%
  ggplot(aes(cluster_name, mean_prob, fill = cluster_name)) +
  geom_violin(position = position_nudge(-.2)) +
  geom_boxplot(width = .2, position = position_nudge(-.2)) +
  geom_errorbar(
    data = cluster_stats,
    aes(ymin = ci_low, ymax = ci_high, color = "95% CI of mean"),
    width = .2,
    position = position_nudge(.2)
  ) +
  geom_point(
    data = cluster_stats,
    aes(y = mean_prob),
    position = position_nudge(.2)
  ) +
  geom_point(
    data = desc_geo %>%
      group_by(cluster_name) %>%
      mutate(mean_prob = .65 * median(mean_prob)),
    aes(y = mean_prob,
        color = "Mixed type\ncutoff = 0.65 × median\nWilcox test: p < 2.2e-16"),
    shape = 5, size = 3
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "95% CI of mean" = "black",
      "Mixed type\ncutoff = 0.65 × median\nWilcox test: p < 2.2e-16" = colors()[571]
    )
  ) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3"
  ),
  guide="none") +
  guides(
    color = guide_legend(
      ncol = 1,  
      override.aes = list(
        shape = c(NA, 5),
        linetype = c(1, 0),
        size = c(0.6, 3)
      )
    ),
  ) +
  labs(title = "Main type assignment probability", x = "", y = "Assignment probability") +
  theme_SM() +
  theme(
    legend.position = c(.98, .82),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "grey", size = 0.2),
    legend.box.margin = margin(2, 0, 2, 2),
    legend.spacing.y = unit(0.01, "lines"),
    legend.background = element_blank()
  )
p_assign_probs

p_share_mixed <- desc_geo_exlc %>%
  group_by(cluster_name, main_mixed) %>% 
  summarise(n = n()) %>% 
  group_by(cluster_name) %>% 
  mutate(share = n/sum(n)) %>%
  mutate(pct = round(share * 100, 1),
         pct = paste0(pct, "%")) %>% 
  ggplot(aes(cluster_name, share, fill = cluster_name, pattern = main_mixed)) +
  geom_col_pattern(
    width = .4,
    linewidth=.2,
    colour = "black",
    pattern_color = "grey",
    pattern_alpha = 0.6,
    pattern_density = 0.3,
    pattern_spacing = 0.01,
    pattern_size = .05,
    pattern_key_scale_factor = 1,
    # position = "fill"
  ) +
  scale_pattern_manual(values = c("none", "stripe")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = expansion(add = c(.3, 1.2))) +
  geom_text(aes(label = pct),
            position = position_stacknudge(x = .22, vjust = 0.5), size = 3, hjust = 0) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3",
    "mixed" = "grey"
  )) + 
  labs(x = "", y ="Percentage", title = "Mixed type percentage") +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = "none")
    ),
    pattern = guide_legend(
      override.aes = list(fill = "white", colour = "black")
    )
  ) +
  theme_SM()  + 
  theme(
    legend.position = c(.8,.81),
    legend.justification = "left",
    legend.box.just = "left", 
    legend.box.background = element_rect(color = "grey", size = 0.2),
    legend.box.margin = margin(rep(2, 4)),
    legend.background = element_blank(),
    legend.spacing.y = unit(0.01, "lines"),
    legend.box = "vertical",
  )

p_share_mixed


# Function to round a numeric vector to fixed decimals and preserve sum = 1
round_preserve_sum <- function(x, digits = 2) {
  scaled <- x * 10^digits
  floored <- floor(scaled)
  remainder <- scaled - floored
  shortfall <- round(sum(scaled)) - sum(floored)
  
  # Order the remainders decreasingly, add 1 to top 'shortfall' entries
  indices <- order(remainder, decreasing = TRUE)[seq_len(shortfall)]
  floored[indices] <- floored[indices] + 1
  
  result <- floored / 10^digits
  return(result)
}

p_co_assignment_prob <- clust_probs %>%
  group_by(cluster_name, secondary_cluster_name) %>%
  summarise(mean_prob = mean(mean_prob, na.rm = TRUE), .groups = "drop") %>%
  group_by(cluster_name) %>%
  mutate(rounded_prob = round_preserve_sum(mean_prob)) %>%
  ungroup() %>% 
  ggplot(aes(x = cluster_name, y = secondary_cluster_name, fill = mean_prob)) +
  geom_tile(color = "white", height = .98, width = 0.86) +
  geom_text(aes(label = sprintf("%.2f", rounded_prob))) +
  scale_fill_gradient2(
    low = "white", mid = "#fff1cc", high = "#963d03",
    oob = scales::squish
  ) +
  labs(
    x = "Main city type (hard assignment)",
    y = "Secondary city type (soft assignment)",
    title = "Overlapping cluster membership"
  ) +
  theme_SM() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.margin = margin(c(0,0,0,3), "cm")
  )
p_co_assignment_prob


# 3) stability
boot_fun <- function(data, indices) {
  d <- data[indices, ] %>% as.data.frame()
  
  d <- d %>%
    group_by(cluster_name) %>%
    mutate(
      cutoff = 0.65 * median(mean_prob),
      mixed  = mean_prob < cutoff
    )
  
  mean(d$mixed)
}
library(boot)

boot_results <- boot(desc_geo_exlc, boot_fun, R = 5000)

boot_df <- data.frame(boot_vals = boot_results$t)

p_stability <- ggplot(boot_df, aes(boot_vals)) +
  geom_histogram(bins = 40, color = "black", fill = "grey", size = .1) +
  geom_vline(aes(xintercept = boot_results$t0), color = colors()[571], size = 1.2) +
  annotate(
    geom = "label",
    x = boot_results$t0 + .0006,
    y = 300,
    hjust = 0,
    label = paste0(
      "Bootstrapped mixed\ntype cities: ", round(boot_results$t0, 3), "\n",
      "Observed mixed\ntype cities: ", round(mean(desc_geo_exlc$main_mixed == "mixed"), 3)
    ),
    color = colors()[571],
    fill = alpha("white", 0.6),    
    label.size = 0
  ) + 
  labs(
    title = "Bootstrap distribution of mixed type share",
    x = "Share of mixed type cities",
    y = "Frequency"
  ) +
  theme_SM()
p_stability

figA3 <- ggarrange(p_assign_probs, p_share_mixed, p_stability, p_co_assignment_prob, labels = "auto", align = "hv")
figA3
ggsave(figA3, file = "plots/figA3.pdf", height = 10, width = 10)

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

df2 <- region_medians %>%
  left_join(global_medians, by="var") %>%
  mutate(
    diff = reg_med - global_med,      # real percentage-point shift
    abs_diff = abs(diff)
  ) %>% 
  select(name = Region, var, diff)

p_type_region_median_diffs <- df2 %>%
  rename_co_vars("var") %>%
  mutate(stat_type = ifelse(grepl("Type|mixed", name), "City type", "Region")) %>% 
  mutate(var = factor(var, levels = co_vars_formatted),
         name = factor(name, levels = rev(c(reg_vars_wg2,  levels(cluster_names$cluster_name), "mixed")))) %>%
  ggplot(aes(var, name, fill = diff)) +
  geom_tile(width=0.9, height=0.9) +
  geom_text(aes(label = round(diff,0)), size = 3) +
  scale_fill_gradient2() +
  facet_grid(stat_type~., scales = "free_y", space = "free") +
  theme_SM() +
  theme(legend.position = "none", 
        axis.title = element_blank()) + 
  labs(x = "", y = "") 
p_type_region_median_diffs
ggsave(p_type_region_median_diffs, file = "plots/p_type_region_median_diffs.pdf", height = 6, width = 7)



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
      title = title %||% ifelse(value == "share", "Percentage of cities", "Number of cities")
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
  title = "Percentage of cities",
  add_row_totals = FALSE
)
p_percentage_type_region

p_n_type_region <- plot_cluster_region_matrix(
  data = desc_geo_exlc,
  value = "n",
  title = "Absolute number of cities",
  add_row_totals = FALSE
)
p_n_type_region


fig2 <- ggarrange(
  ggarrange(fig2_reg, # fig2_type, 
            labels = c("a",  ""), nrow = 1), 
  ggarrange(p_percentage_type_region, p_n_type_region, labels = c("c", "d"), nrow = 2), 
  labels = c("", ""), nrow = 2, heights = c(1,1))

ggsave(fig2, file = "plots/fig2.pdf", width = 10, height = 12)

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
title <- ggdraw() + draw_label("Four main city types")
base <- plot_grid(title, base, ncol=1, rel_heights=c(0.03, 1)) 

fig1_bc <- ggarrange(p_attr_cluster_bar, 
                     test_region_thresh_prob, 
                     labels = c("b", "c"), align = "v",
                     ncol = 1)

fig1 <- ggarrange(base, fig1_bc, labels = c("a", ""), widths = c(1.4,1))
ggsave(fig1, file = "plots/fig1.pdf", width = 10, height = 10)


################################################################################
# assignment probabilities (Extended Data Fig X)
################################################################################


test_assign_probs_secondary_type <- clust_probs %>% 
  left_join(cities_ipcc_regions, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  ggplot(aes(cluster_name, mean_prob, col = cluster_name)) + 
  geom_violin() +
  geom_boxplot(size = .1, width = .1, outliers = F) + 
  facet_grid(secondary_cluster_name~Region) +
  scale_color_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
  labs(title = "primary and secondary assignment probabilities by region and type") +
  theme_SM() + 
  labs(x = "", y = "Probability") +
  theme(legend.position = "bottom")
test_assign_probs_secondary_type
ggsave(test_assign_probs_secondary_type, file = "plots/test_assign_probs_secondary_type.pdf", width = 10, height = 10)


test_region_asia <- desc_geo %>% 
  filter(Region == "Asia") %>% 
  as.data.frame() %>% 
  group_by(cluster_name, GC_CNT_GAD_2025) %>% 
  summarise(n = n()) %>% 
  group_by(GC_CNT_GAD_2025) %>% 
  mutate(share = n/sum(n)) %>% 
  ggplot(aes(GC_CNT_GAD_2025, share, fill = cluster_name)) + 
  geom_col(position = "stack", width = .4, col = "black", size = .2) +
  geom_text(aes(label = paste0(round(share * 100, 0), "%")),
            position = position_stacknudge(x = .22, vjust = 0.5), size = 3, hjust = 0) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3")) +
  theme_SM() + 
  theme(legend.position = "bottom")
ggsave(test_region_asia, file = "plots/test_region_asia.pdf", width = 20, height = 5)


clust_probs %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  filter(GC_UCN_MAI_2025 %in% c("Paris", "London", "Berlin", "Basel", "Mombasa", "Victoria", "Pretoria", "Durban")) %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, cluster_name, secondary_cluster_name) %>% 
  arrange(GC_UCN_MAI_2025, -mean_prob) %>% as.data.frame()

clust_probs %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cities_ipcc_regions, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  filter(Region == "Asia") %>% 
  filter(GC_UCN_MAI_2025 %in% c("Shanghai", "Hechi", "Fuzhou", "Guyiyang", "Tokyo", "Chiang Mai", "Ayutthaya", "Cebu City", "Manila")) %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, cluster_name, secondary_cluster_name) %>% 
  arrange(GC_UCN_MAI_2025, -mean_prob) %>% as.data.frame()

# Cebu City --> now Type 2
# New Delhi --> now Type 4 (and secondary type 3!), before Type 1
# ghsl$GC_UCN_MAI_2025[grepl("Ulaanbaata", ghsl$GC_UCN_MAI_2025)]

clust_probs %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cities_ipcc_regions, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  filter(Region == "Asia") %>% 
  filter(GC_UCN_MAI_2025 %in% c("New Delhi", "Vadodara", "Isfahan", "Ahwaz", "Mota", "Gaziantep", "Dubai", "Ulaanbaatar", "Mota")) %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, cluster_name, secondary_cluster_name) %>% 
  arrange(GC_UCN_MAI_2025, secondary_cluster_name) %>% as.data.frame()


clust_probs %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  filter(GC_UCN_MAI_2025 %in% c("Berlin", "Basel", "Mombasa", "Victoria", "Pretoria", "Phuthaditjhaba")) %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, cluster_name, secondary_cluster_name) %>% 
  arrange(GC_UCN_MAI_2025, -mean_prob) %>% as.data.frame()

clust_probs %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  filter(GC_UCN_MAI_2025 %in% c("Santiago de Cuba", "Cartagena", "Mombasa", "Cancún", "Basra", "Makassar", "Berlin", "Melbourne", "Louisville", "Chongqing")) %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, mean_prob, cluster_name, secondary_cluster_name) %>% 
  arrange(GC_UCN_MAI_2025, -mean_prob) %>% as.data.frame()

################################################################################
# distributions by types
################################################################################

# pick your percentiles
lower_p <- 0.0025   
upper_p <- 0.9975  

facet_limits <- box_plot_add_covs_dat %>%
  group_by(variable) %>%
  summarize(
    ymin = quantile(normalized_value, lower_p, na.rm = TRUE),
    ymax = quantile(normalized_value, upper_p, na.rm = TRUE)
  )
facet_limits

y_scales <- lapply(seq_len(nrow(facet_limits)), function(i) {
  scale_y_continuous(limits = c(facet_limits$ymin[i], facet_limits$ymax[i]))
})



# Compute means by cluster and variable
means_df <- box_plot_add_covs_dat %>%
  group_by(clustering, cluster_name, variable) %>%
  summarise(mean_val = mean(normalized_value, na.rm = TRUE), .groups = "drop") 

# scales <- list(
#   # Here you have to specify all the scales, one for each facet row in your case
#   scale_y_continuous(limits = c(0, 10)),
#   scale_y_continuous(limits = c(0, 10)),
#   scale_y_continuous(limits = c(0, 10)),
#   scale_y_continuous(limits = c(-2, 17))
# )

# show_legend <- cluster == "Mega all in"

p_box_characteristics <- box_plot_add_covs_dat %>% 
  ggplot(aes(x = cluster_name, y = normalized_value)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_violin(alpha = 0.5, color = NA, scale = "width", aes(fill = cluster_name), trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, color = "grey", outliers = F, aes(fill = cluster_name)) +
  scale_fill_manual(values = c(
    "Type 1" = "#E41A1C",
    "Type 2" = "#377EB8",
    "Type 3" = "#4DAF4A",
    "Type 4" = "#984EA3")) +
  geom_point(
    data = means_df,
    aes(x = cluster_name, y = mean_val, shape = "Mean value by type"),
    size = 2, fill = "darkred", color = "black", inherit.aes = FALSE
  ) +
  geom_label(
    data = means_df,
    aes(x = cluster_name, y = mean_val, label = round(mean_val, 2)),
    vjust = -0.3, size = 2.2, fill = "white", color = "black",      # color of the label text and border
    label.size = 0, alpha = 0.7, inherit.aes = FALSE
  ) + 
  scale_shape_manual(values = c("Mean value by type" = 21), name = "") +
  facet_wrap(.~variable, scales = "free") +
  ggh4x::facetted_pos_scales(
    y = y_scales
  ) +
  coord_flip() +
  theme_SM() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = c(.85,.1),
    legend.justification = "left",
    legend.box.just = "left", 
    legend.box.background = element_rect(color = "grey", size = 0.2),
    legend.box.margin = margin(rep(2, 4)),
    legend.background = element_blank(),
    legend.spacing.y = unit(0.01, "lines"),
    legend.box = "vertical",
    legend.direction = "vertical",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = margin(c(-6,3,-2,3), "cm")
  ) +
  labs(
    x = "",
    y = "Normalized value",
    title = ""
  ) + 
  guides(fill=guide_legend(ncol=2))
p_box_characteristics
ggsave(p_box_characteristics, file = "plots/p_box_characteristics.pdf", height = 7, width = 10)

##################################################################
# examples
##################################################################

plot_covariate_boxplot <- function(box_plot_add_covs_dat, clust_probs, selected_cluster, highlight_id = NULL, city_name, limits = NULL) {
  
  # Filter data for the selected cluster
  cluster_data <- box_plot_add_covs_dat %>%
    filter(cluster_name == selected_cluster) 
  
  # If highlight_id is provided, get that observation
  if (!is.null(highlight_id)) {
    highlight_df <- cluster_data %>%
      filter(GHS_urban_area_id == highlight_id) %>% 
      mutate(highlight_val = normalized_value)
    
    highlight_types <- clust_probs %>% 
      filter(GHS_urban_area_id == highlight_id) 
    
  } else {
    stop("must provide city id")
  }
  
  rad <- highlight_types %>%
    select(secondary_cluster_name, mean_prob) %>%
    ggplot(aes(x = secondary_cluster_name, y = mean_prob)) +
    geom_col(color = "black", fill = "lightgrey", width = 0.7) +
    geom_text(aes(label = scales::percent(mean_prob, accuracy = 1)),
              vjust = -0.5, size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    labs(x = NULL, y = "", title = "Type") +
    theme_SM() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.ticks.length = unit(.5, "mm"),
          axis.text = element_text(size = 8),
          axis.title = element_blank(),
          plot.title = element_text(size = 10))
  
  # compute lower and upper whiskers
  iqr_limits <- cluster_data %>%
    filter(cluster_name == selected_cluster) %>%
    summarise(
      Q1 = quantile(normalized_value, 0.25, na.rm = TRUE),
      Q3 = quantile(normalized_value, 0.75, na.rm = TRUE)
    ) %>%
    mutate(
      IQR = Q3 - Q1,
      lower = Q1 - 10 * IQR,
      upper = Q3 + 15 * IQR
    )
  
  filtered_data <- box_plot_add_covs_dat %>%
    filter(value >= iqr_limits$lower & value <= iqr_limits$upper)
  
  
  
  # Create plot
  p <- ggplot(cluster_data, aes(x = variable, y = normalized_value)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_violin(alpha = 0.9, color = NA, scale = "width", aes(fill = clustering)) +
    scale_fill_manual(values = c("#ffe0a3", "cornflowerblue")) +
    geom_boxplot(width = 0.15, outliers = F, outlier.colour = "#ffe0a3") +
    geom_point(
      data = highlight_df,
      aes(x = variable, y = highlight_val, 
          # shape = "Example city"
      ),
      size = 2, fill = "darkred", color = "black", inherit.aes = FALSE
    ) +
    geom_label(
      data = highlight_df,
      aes(x = variable, y = highlight_val, label = round(highlight_val, 2)),
      vjust = -0.8, size = 2.2, fill = "white", color = "black",
      label.size = 0.25, alpha = 0.7, inherit.aes = FALSE
    ) +
    # scale_shape_manual(values = c("Example city" = 21), name = "") +
    scale_y_continuous(
      trans = "log2",
      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
      labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
      limits = c(-5, 30)
    ) +
    theme_SM() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = c(.95, .85),
      legend.justification = "right",
      legend.box.just = "right",
      legend.direction = "horizontal", 
      legend.byrow = T,
      legend.box = "horizontal",
      axis.ticks.length = unit(.5, "mm"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8), 
      plot.title = element_text(size = 10)
    ) +
    labs(
      x = "",
      y = "Normalized value",
      title = city_name
    )
  
  if(is.null(limits)){
    # scale y limits based on ylim from the cluster
    p = p + coord_cartesian(ylim = c(min(filtered_data$value), max(filtered_data$value)))
  } else {
    # scale y limits based on ylim provided 
    p = p + coord_cartesian(ylim = limits)
  }
  
  p_ins = ggarrange(p, rad, align = "h", widths = c(3, 1.2))
  
  return(p_ins)
}


plot_multiple_cities <- function(city_names = NULL, city_ids = NULL, ghsl, clust_probs, covariate_data, output_dir = "plots", height = 12, limits = NULL) {
  
  highlight_ids <- c()
  if (!is.null(city_names)){
    for (city_name in city_names) {
      # Get the corresponding ID
      highlight_id <- unique(ghsl$ID_UC_G0[ghsl$GC_UCN_MAI_2025 %in% city_name])
      highlight_id <- highlight_id[!is.na(highlight_id)]
      if(length(highlight_id)>1){
        stop("City name does not uniquely identify.")
      }
      if (length(highlight_id) == 0) {
        message(paste0("Skipping '", city_name, "' — no valid highlight_id found."))
        next
      }
      highlight_ids <- c(highlight_ids, highlight_id)
    }
  }
  
  highlight_ids <- c(highlight_ids, city_ids)
  
  
  plot_list <- list()
  for (highlight_id in highlight_ids) {
    city_name <- ghsl$GC_UCN_MAI_2025[ghsl$ID_UC_G0 %in% highlight_id]
    
    # Get cluster name
    selected_cluster <- covariate_data %>%
      filter(GHS_urban_area_id %in% highlight_id) %>%
      pull(cluster_name) %>%
      unique() %>%
      as.character()
    
    if (length(selected_cluster) != 1) {
      message(paste0("Skipping '", city_name, "' — could not uniquely identify cluster."))
      next
    }
    
    # Generate plot
    p <- plot_covariate_boxplot(
      box_plot_add_covs_dat,
      selected_cluster = selected_cluster,
      clust_probs = clust_probs, 
      highlight_id = highlight_id,
      city_name = paste0(city_name, " - ", selected_cluster), 
      limits = limits
    )
    
    plot_list[[city_name]] <- p
    
  }
  
  # Save plot as PDF
  output_file <- plot_grid(plotlist = plot_list, ncol = 2, labels = "auto", align = "v")
  ggsave(output_file, file = output_dir, width = 10, height = height)
  
}

# # Vector of city names to plot"
city_names <- c("Cartagena", "Mombasa", "Santiago de Cuba", "Cancún", "Basra", "Berlin", "Melbourne", "Louisville", "Chongqing", "Makassar")
city_ids <- ghsl %>% filter(GC_UCN_MAI_2025 %in% city_names)
city_ids <- city_ids %>% filter(!(GC_UCN_MAI_2025 == "Cartagena" & GC_CNT_GAD_2025 == "Spain")) %>% as.data.frame()
city_ids <- city_ids[match(city_names, city_ids$GC_UCN_MAI_2025), ] %>% pull(ID_UC_G0)


# Run batch plotting
plot_multiple_cities(
  # city_names = city_names,
  ghsl = ghsl,
  city_ids = city_ids[1:5],
  clust_probs = clust_probs,
  covariate_data = box_plot_add_covs_dat,
  output_dir = "plots/figA4.pdf",
  limits = c(.0625,32)
  # height = 12
)

# Run batch plotting
plot_multiple_cities(
  # city_names = city_names,
  ghsl = ghsl,
  city_ids = city_ids[6:10],
  clust_probs = clust_probs,
  covariate_data = box_plot_add_covs_dat,
  output_dir = "plots/figA5.pdf",
  limits = c(.0625,32)
  # height = 12
)

################################################################################
# learning examples 
################################################################################


# import solutions
case_ex <- read.csv("data/case_study_solutions/export-5.csv") %>% as_tibble()
clim_sol <- readxl::read_xlsx("data/climate_solutions_typology/climate_solution_typology_long_2.xlsx") %>% as_tibble()

solution_cols <- colnames(case_ex)[grepl("sol", colnames(case_ex))]

case_ex <- case_ex %>% 
  filter(any(!is.na(solution_cols))) %>% # only those with solutions
  left_join(clean_places %>% 
              tibble() %>% 
              mutate(id = gsub("https://openalex.org/", "", id)), by = c("openalex_id" = "id")) %>% 
  pivot_longer(solution_cols, names_to = "solution_id") %>% 
  select(openalex_id, solution_id, value, city_id) %>% 
  filter(value !=0) %>% 
  mutate(solution_id = as.numeric(gsub("sol.", "", solution_id, fixed = T))) %>% 
  left_join(clim_sol %>% select(`Services/Provisions`, solution_type, solution_id, solution_name, wg_ipcc), by = "solution_id") %>% 
  left_join(ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = c("city_id" = "ID_UC_G0")) %>% 
  rename(city_name = GC_UCN_MAI_2025, country = GC_CNT_GAD_2025) %>% 
  filter(city_name %in% city_names) %>% 
  left_join(clust_probs %>% 
              group_by(GHS_urban_area_id) %>% 
              arrange(-mean_prob) %>% 
              slice(1), by = c("city_id" = "GHS_urban_area_id")) %>% 
  filter(!is.na(solution_type)) 


examples_dat <- ghsl %>%
  mutate(geom = st_centroid(geom)) %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_DEV_USR_2025, geom) %>%
  left_join(clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  left_join(clust_probs %>%
              select(GHS_urban_area_id, mean_prob, secondary_cluster_name) %>%
              pivot_wider(names_from = secondary_cluster_name, values_from = mean_prob), by = c("ID_UC_G0" = "GHS_urban_area_id"))


# --- 1. Extract probability matrices ---

examples_dat_teaching <- examples_dat %>% 
  filter(ID_UC_G0 %in% city_ids) 

examples_dat_learning <- examples_dat %>% 
  filter(!ID_UC_G0 %in% city_ids) 

probs_teaching <- as.matrix(
  examples_dat_teaching %>% 
    as.data.frame() %>% 
    select(`Type 1`, `Type 2`, `Type 3`, `Type 4`)
)

probs_learning <- as.matrix(
  examples_dat_learning %>% 
    as.data.frame() %>% 
    select(`Type 1`, `Type 2`, `Type 3`, `Type 4`)
)


cosine_dist_fast <- function(p, Q) {
  # p: length 4 vector (1 teaching city)
  # Q: matrix N x 4 (learning cities)
  
  p_norm <- sqrt(sum(p * p))
  Q_norm <- sqrt(rowSums(Q * Q))
  
  sim <- (Q %*% p) / (Q_norm * p_norm)
  
  dist <- 1 - sim  # convert to distance
  return(as.numeric(dist))
}

dist_list <- apply(probs_teaching, 1, cosine_dist_fast, Q = probs_learning)

# dist_list is a matrix: rows = learning cities, columns = teaching cities
dist_list <- t(dist_list)

# For each teaching city, retrieve indices of 3 closest learning cities
nearest_indices <- apply(dist_list, 1, function(d) order(d)[1:3])

# nearest_indices is 3 x N_teaching
# transpose it so each row corresponds to one teaching city
nearest_indices_t <- t(nearest_indices)

results_df <- tibble()

for (i in seq_len(nrow(nearest_indices_t))) {
  
  results_df <- bind_rows(
    results_df,
    tibble(
      teaching_ids = examples_dat_teaching$ID_UC_G0[i],
      learning_ids = examples_dat_learning$ID_UC_G0[ nearest_indices_t[i, ] ],
      cosine_distance  = dist_list[i, nearest_indices_t[i, ] ]
    )
  )
}

results_df

examples_dat_learning <- examples_dat_learning %>% 
  # filter(!ID_UC_G0 %in% c(1624, 9932)) %>% # Shahadi, Baragashi are GHSL data errors
  filter(ID_UC_G0 %in% results_df$learning_ids)

examples_dat_teaching <- examples_dat_teaching %>% 
  filter(ID_UC_G0 %in% unique(results_df$teaching_ids))


examples_dat_teaching_coords <- examples_dat_teaching %>%
  mutate(
    lon = st_coordinates(geom)[,1],
    lat = st_coordinates(geom)[,2] 
  ) %>%
  as.data.frame() %>%
  select(-geom) %>% 
  left_join(case_ex %>% 
              group_by(city_id) %>% 
              summarise(n_solution_studies = n()), by = c("ID_UC_G0" = "city_id")) %>% 
  mutate(n_solution_studies = ifelse(is.na(n_solution_studies), 0, n_solution_studies))


examples_dat_learning_teaching <- bind_rows(
  examples_dat_teaching %>% mutate(learning = "teaching"), 
  examples_dat_learning %>% mutate(learning = "learning")) %>%
  mutate(
    fill_state = paste(cluster_name, learning, sep = "_")
  )

fig4 <- ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA) +
  
  geom_label_repel(
    data = examples_dat_learning_teaching,
    aes(
      label = GC_UCN_MAI_2025,
      geometry = geom,
      fill = fill_state,  
      col  = learning
    ),
    show.legend = FALSE,
    stat = "sf_coordinates",
    segment.colour = "grey60",
    size = 3.5,
    max.overlaps = Inf,
    force = .2,
    seed = 10
  ) +
  
  scale_fill_manual(values = c(
    "Type 1_learning"  = alpha("#E41A1C", 0.15), 
    "Type 1_teaching"  = "#E41A1C",
    
    "Type 2_learning"  = alpha("#377EB8", 0.15),
    "Type 2_teaching"  = "#377EB8",
    
    "Type 3_learning"  = alpha("#4DAF4A", 0.15),
    "Type 3_teaching"  = "#4DAF4A",
    
    "Type 4_learning"  = alpha("#984EA3", 0.15),
    "Type 4_teaching"  = "#984EA3"
  )) +
  
  scale_color_manual(values = c(
    "learning" = "grey30",
    "teaching" = "white"
  )) +
  
  ggnewscale::new_scale_colour() + 
  
  geom_sf(data = examples_dat_teaching,
          aes(col = cluster_name),
          size = 6, alpha = 0.9) + 
  scale_color_manual("Representative case study\nexamples; numbers indicate\nclimate solutions",
                     values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
                     guide = guide_legend(order = 2)) +
  
  # --- Total number of studies in center ---
  geom_text(
    data = examples_dat_teaching_coords,
    aes(x = lon, y = lat, label = n_solution_studies), col= "white",
    size = 2, fontface = "bold"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = c(rep(3,4))
      ),
      title.position = "top",
      ncol = 2
    )
  ) +
  
  ggnewscale::new_scale_colour() + 
  
  geom_sf(data = examples_dat_learning,
          aes(col = cluster_name),
          size = 2, alpha = 0.2, inherit.aes = F) +
  scale_color_manual("Similar cities with\ntransfer-learning potential",
                     values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
                     guide = guide_legend(order = 1)) +
  
  # Separate legends for teaching vs learning
  guides(
    color = guide_legend(
      override.aes = list(
        size = c(rep(3,4))),
      title.position = "top",
      ncol = 2
    )
  ) +
  
  scale_x_continuous(limits = c(st_bbox(examples_dat_learning_teaching)[1], st_bbox(examples_dat_learning_teaching)[3]))+
  scale_y_continuous(limits = c(st_bbox(examples_dat_learning_teaching)[2], st_bbox(examples_dat_learning_teaching)[4]))+
  
  theme_void() +
  theme(
    legend.position = c(0.635, 0.35),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.title.position = "top",
    legend.direction = "vertical",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.box = "horizonzal",
    legend.spacing.y = unit(.05, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
    legend.box.margin = margin(rep(2, 4)),
    text = element_text(size = 8), 
    plot.margin = margin(rep(-1, 4)),
  )

fig4
ggsave(fig4, file = "plots/fig4.pdf", height = 4.5, width = 10)


figA6 <- case_ex %>%
  mutate(wg_ipcc = factor(wg_ipcc, levels = c("adaptation", "mitigation", "cross-cutting"))) %>%
  group_by(wg_ipcc, cluster_name) %>%
  summarise(n_solution_studies = n(), .groups = "drop") %>%
  group_by(cluster_name) %>%
  mutate(share = n_solution_studies / sum(n_solution_studies)) %>% 
  ggplot(aes(x = cluster_name, y = share, fill = wg_ipcc)) +
  geom_col(color = "black", linewidth = 0.2, width = .4) +
  scale_alpha_manual(values = c(1,.7,.4)) +
  scale_pattern_manual(values = c("none", "none", "stripe")) +
  geom_text(aes(label = paste0(round(share * 100), "%")),
            position = position_stacknudge(x = .22, vjust = 0.5), size = 3, hjust = 0) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_npg() +
  theme_SM() +
  labs(y = "", x = "") +
  theme(legend.position = "bottom")
figA6

ggsave(figA6, file = "plots/figA6.pdf", width = 5, height = 5)


case_ex <- case_ex %>%
  mutate(solution_short = sapply(solution_name, function(x) {
    if (nchar(x) <= 37) return(x)
    
    words <- str_split(x, " ")[[1]]
    lines <- c()
    current_line <- words[1]
    
    for (i in 2:length(words)) {
      # Check if adding the next word exceeds 37 chars
      if (nchar(current_line) + 1 + nchar(words[i]) > 37) {
        lines <- c(lines, current_line)  # finish current line
        current_line <- words[i]         # start new line
      } else {
        current_line <- paste(current_line, words[i])
      }
    }
    lines <- c(lines, current_line)     # add last line
    paste(lines, collapse = "\n")
  }))



res_long <- case_ex %>% 
  group_by(city_name, `Services/Provisions`, solution_type, solution_short, cluster_name) %>% 
  dplyr::count(name = "n_studies_solutions")

# Heatmap 
res_long <- res_long %>% 
  mutate(solution_short = ifelse(grepl("AI-based", solution_short), "Highly accessible compact urban\nform and transit networks", solution_short)) %>% 
  mutate(`Services/Provisions` = gsub(" \\([0-9]\\)", "", `Services/Provisions`),
         `Services/Provisions` = str_trim(`Services/Provisions`),
         `Services/Provisions` = ifelse(`Services/Provisions` == "Thermal comfort and Heat stress management", "Heat &\nthermal\nmanagement", `Services/Provisions`),
         `Services/Provisions` = ifelse(`Services/Provisions` == "Food provisioning systems", "Food", `Services/Provisions`),
         `Services/Provisions` = ifelse(`Services/Provisions` == "Disaster and risk management", "Disaster\n& risk\nmanagement", `Services/Provisions`),
         `Services/Provisions` = ifelse(`Services/Provisions` == "Carbon dioxide removal", "Carbon\ndioxide\nremoval", `Services/Provisions`),
         `Services/Provisions` = ifelse(`Services/Provisions` == "Waste management", "Waste", `Services/Provisions`),
         `Services/Provisions` = factor(`Services/Provisions`, levels = c("Mobility", "Buildings", "Energy", "Heat &\nthermal\nmanagement",
                                                                          "Food", "Water", "Waste", "Disaster\n& risk\nmanagement", 
                                                                          "Carbon\ndioxide\nremoval"))) %>% 
  mutate(solution_short = stri_trim(solution_short)) %>% 
  ungroup() 

res_long <- res_long %>%
  group_by(`Services/Provisions`, solution_short) %>% 
  tidyr::complete(
    city_name = unique(.$city_name),
    cluster_name = unique(.$cluster_name),
    fill = list(n_studies_solutions = NA)
  ) %>% 
  filter(paste(cluster_name, city_name) %in% paste(case_ex$cluster_name, case_ex$city_name)) %>% 
  mutate(col = ifelse(`Services/Provisions` %in% c("Mobility", "Buildings", "Energy", "Waste","Carbon dioxide\nremoval"), 1, 2)) %>% 
  mutate(cluster_name = gsub(" ", "\n", cluster_name)) 


# Function to create heatmap for a given column
plot_solution_heatmap <- function(res_long, col_value) {
  
  # 2. After completing missing city rows, left join the metadata back in
  filled_data <- res_long %>%
    filter(col == col_value) %>%
    ggplot(aes(x = solution_short, y = city_name, fill = n_studies_solutions)) +
    geom_tile(color = "white", width = .9, height = .9, na.rm = T) +
    geom_text(aes(label = n_studies_solutions), color = "white", size = 3.5) +
    scale_fill_continuous(na.value=NA) + 
    facet_nested(
      `Services/Provisions` ~ cluster_name,
      scales = "free",
      space = "free",
      switch = "y"
    ) +
    coord_flip() +
    labs(
      x = "Services/Provision", y = "", 
      fill = "Number of climate solutions in the\ncase study examples"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid = element_blank(),
      strip.placement = "outside",
      legend.position = "bottom",
      legend.justification = "right",
      legend.title.position = "top",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.box = "vertical",
      legend.spacing.y = unit(0.1, "cm"),
      legend.background = element_blank(),
      legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
      axis.text.y.left = element_text(lineheight = 0.7, size = 8),
      strip.text.y.left = element_text(angle = 90, size = 10, hjust = 0.5),
      strip.background.y = element_rect(fill = "grey35"),
      strip.text.y = element_text(colour = "white"),
      strip.text.x = element_text(colour = "black"),
      strip.background.x = element_rect(fill = "white"),
      panel.spacing = unit(.05, "lines"),
      panel.border = element_rect(color = "grey90", fill = NA, size = 1), 
      panel.background = element_rect(fill = "white"),
      axis.ticks = element_line(color = "grey90"),
      axis.ticks.length = unit(0.15, "cm"),
      plot.margin = unit(c(0,0,0,.5), "cm")
    )
}


# Generate plots
p1 <- plot_solution_heatmap(res_long, 1)
p2 <- plot_solution_heatmap(res_long, 2)

# Combine plots
fig5 <- ggarrange(p1 + theme(legend.position = "none"), p2)
ggsave(fig5, file = "plots/fig5.pdf", height = 9, width = 10)

################################################################################
# additional descriptives
################################################################################

library(scales)

# Population share by Region per cluster
p_pop_share_cont <- clust %>%
  group_by(cluster_name, Region) %>%
  summarise(GHS_population = sum(GHS_population), .groups = "drop") %>%
  mutate(pop_share = GHS_population / sum(GHS_population)) %>%
  ggplot(aes(x = Region, y = pop_share)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~cluster_name) +
  labs(x = "Region", y = "Population Share") +
  theme_SM() +
  theme(plot.title = element_blank())

# City share by Region per cluster
p_city_share_cont <- clust %>%
  group_by(cluster_name, Region) %>%
  summarise(n_cities = n(), .groups = "drop") %>%
  mutate(share_cities = n_cities / sum(n_cities)) %>%
  ggplot(aes(x = Region, y = share_cities)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~cluster_name) +
  labs(x = "Region", y = "City Share") +
  theme_SM() +
  theme(plot.title = element_blank())

# Combine plots vertically
p_pop_and_city_share_cont <- ggarrange(
  p_pop_share_cont, 
  p_city_share_cont, 
  labels = c("c", "d"),
  ncol = 1,
  align = "v"
)

# Save to PDF
ggsave("plots/p_pop_and_city_share_cont.pdf", p_pop_and_city_share_cont, height = 10, width = 8)


################################################################################
# evidence growth by group
################################################################################

clust_with_topics <- clust %>%
  left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>%
  left_join(oa %>% select(id, abstract, publication_year), by = "id") %>%
  as_tibble()

# Assign IPCC phases
data_phase <- clust_with_topics %>%
  mutate(
    publication_year = as.numeric(publication_year),
    phase = case_when(
      publication_year < 1990 ~ "AR1",
      publication_year > 1990 & publication_year <= 1995 ~ "AR2",
      publication_year > 1995 & publication_year <= 2001 ~ "AR3",
      publication_year > 2001 & publication_year <= 2007 ~ "AR4",
      publication_year > 2007 & publication_year <= 2014 ~ "AR5",
      publication_year > 2014 & publication_year <= 2022 ~ "AR6"
    )
  ) 

# Aggregate: n_studies per phase per cluster and Region
growth_by_phase <- data_phase %>%
  filter(!is.na(phase)) %>%
  group_by(phase, cluster_name, Region) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  mutate(phase = factor(phase, levels = c("AR1", "AR2", "AR3", "AR4", "AR5", "AR6"))) %>%
  arrange(cluster_name, Region, phase) %>%
  group_by(cluster_name, Region) %>%
  mutate(
    baseline = first(n_studies),  # for normalized growth
    norm_growth = n_studies / baseline
  ) 

growth_by_phase_anno <- growth_by_phase %>% 
  group_by(cluster_name, phase) %>% 
  summarise(n_studies = sum(n_studies)) %>% 
  mutate(n_studies_lag = lag(n_studies),
         pct_growth = round(100 * (n_studies - n_studies_lag) / n_studies_lag, 1))


# Add midpoint year of each phase for plotting
phase_years <- tibble(
  phase = c("AR1", "AR2", "AR3", "AR4", "AR5", "AR6"),
  year = c(1990, 1993, 1998, 2004, 2011, 2018)
)

growth_by_phase_anno <- left_join(growth_by_phase_anno, phase_years, by = "phase")
growth_by_phase <- left_join(growth_by_phase, phase_years, by = "phase")

# Create annotation labels only for phases with lag
annotations <- growth_by_phase_anno %>%
  filter(!is.na(pct_growth)) %>%
  mutate(
    label = paste0(pct_growth, "%"),
    y = 130  
  )

# Final plot: normalized growth + annotations with phase-to-phase growth
p_g_rel <- ggplot(growth_by_phase, aes(x = year, y = norm_growth, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  facet_wrap(~cluster_name) +
  geom_text(
    data = annotations,
    aes(x = year, y = y, label = label),
    inherit.aes = FALSE,
    vjust = -0.5, size = 2.5
  ) +
  geom_vline(
    xintercept = c(1990.5, 1995.5, 2001.5, 2007.5, 2014.5, 2022.5),
    color = "grey60", linetype = "dashed", size = 0.3
  ) +
  ylim(c(0, 150)) + xlim(c(1990,2025)) +
  scale_color_npg() +
  labs(
    x = "Publication year",
    y = "Relative growth (normalized to AR1)"
  ) +
  theme_SM() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0),
    legend.position = c(.35, .75)
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

# Print
p_g_rel

# Step 1: Assign IPCC phase labels
data_phase <- clust_with_topics %>%
  mutate(
    publication_year = as.numeric(publication_year),
    phase = case_when(
      publication_year < 1990 ~ "AR1",
      publication_year > 1990 & publication_year <= 1995 ~ "AR2",
      publication_year > 1995 & publication_year <= 2001 ~ "AR3",
      publication_year > 2001 & publication_year <= 2007 ~ "AR4",
      publication_year > 2007 & publication_year <= 2014 ~ "AR5",
      publication_year > 2014 & publication_year <= 2022 ~ "AR6",
      publication_year > 2022 ~ "AR7"
    )
  ) 

# Step 2: Compute annotation totals per cluster and phase
annotations <- data_phase %>%
  filter(!is.na(phase)) %>%
  group_by(phase, cluster_name) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(
    x = case_when(
      phase == "AR1" ~ 1990,
      phase == "AR2" ~ 1993,
      phase == "AR3" ~ 1998,
      phase == "AR4" ~ 2004,
      phase == "AR5" ~ 2011,
      phase == "AR6" ~ 2018,
      phase == "AR7" ~ 2025
    ),
    y = 14000  
  )

# Step 3: Main plot with bars, vlines, and annotations
p_g_abs <- data_phase %>%
  filter(publication_year >= 1990) %>%
  group_by(publication_year, cluster_name, Region) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  ggplot(aes(x = publication_year, y = n_studies, fill = Region)) +
  geom_bar(stat = "identity", position = "stack", width = .8) +
  facet_wrap(~cluster_name) +
  # Add vertical IPCC lines (optional)
  geom_vline(xintercept = c(1990.5, 1995.5, 2001.5, 2007.5, 2014.5, 2022.5), color = "grey60", linetype = "dashed", size = 0.3) +
  # Add annotation counts
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = total),
    inherit.aes = FALSE,
    vjust = -0.5, size = 2.5
  ) +
  ylim(c(0, 15000)) +
  scale_fill_npg() +
  labs(x = "Publication year", y = "Studies") +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        legend.position = c(.35,.7)) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

figA7 <- ggarrange(p_g_abs, p_g_rel, ncol = 1, labels = c("a", "b"), align = "v")
ggsave(figA7, file = "plots/figA7.pdf", height = 9, width = 10) 


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
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  theme_SM() +
  theme(
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    legend.position = c(0.9,.05),
    axis.title = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(x = "", y = "",  subtitle = "Number of cities")
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
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  theme_SM() +
  theme(
    legend.title = element_text(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = .5)
  ) +
  labs(x = "", y = "", subtitle = "Total number of cases studied")


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
    label = "Similarity to all cities",
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
    label = "Similarity to cities with studies",
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
    axis.text.x = element_text(angle = 0, hjust = .5)
  ) +
  labs(x = "", y = "", subtitle = "Similarity to cities with studies")
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
    axis.text.x = element_text(angle = 0, hjust = .5)
  ) +
  labs(x = "", y = "", subtitle = "Similarity to all cities")
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
    label = "Research coverage of global cities by type",
    hjust = -0.2, vjust = 1.7,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) + 
  theme_SM() +
  labs(
    y = "", x = "", col = "City type", size = "Number of studies"
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
    label = "Cities by type not covered by any studies",
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
    label = "Count of cities not covered by any studies",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) +
  theme_SM() +
  labs(y = "", x = "", fill = "Number of cities without research") +
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
# Combine plots into Figure 2
################################################################################

fig3bcde <- ggarrange(
  p_similarity_map_all, p_similarity_map_with_studies,
  p_types_as_dots_without_research + theme(legend.position = "none"), 
  p_count_without_research,
  labels = c("b", "c", "d", "e")
)

fig3abcde <- ggarrange(
  p_types_as_dots_with_research,
  fig3bcde,
  labels = c("a", ""),
  ncol = 1,
  heights = c(1, 1)
)

fig3fgh <- ggarrange(
  p_n_studies + theme(text = element_text(size = 9)),
  similarity_by_type_all + theme(text = element_text(size = 9)),
  similarity_by_type_with_studies + theme(text = element_text(size = 9)),
  p_cities_per_cluster_n + theme(text = element_text(size = 9)),
  align = "h", labels = c("e", "f", "g", "h"),
  ncol = 1
)

fig3 <- ggarrange(fig3abcde, fig3fgh, labels = c("", ""), ncol = 2, widths = c(3.2, 1))

ggsave(fig3, file = "plots/fig3.pdf", height = 8, width = 10)
