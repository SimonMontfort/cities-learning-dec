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

################################################################################
# load data
################################################################################

clust <- read.csv("data/clustering_results/dec_clusters_k4.csv")
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
ghsl_clean <- read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet")

labelled_topics <- readxl::read_xlsx("data/topic_model/labelled_topics_2.xlsx")
main_topic <- read.csv("data/topic_model/main_topic_220.csv")

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

cites_ipcc_regions <- read.csv("data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv")


################################################################################
# load data for ex post characterisation
################################################################################

emmissions <- read.csv("data/emissions/balance_sheet.csv")

load_data <- function(file_name, value_col, new_name) {
  read.csv(file.path("data/GHS_UCDB_GLOBE_R2024A_V1_0", file_name)) %>%
    select(ID_UC_G0, all_of(value_col)) %>%
    mutate(across(all_of(value_col), as.numeric)) %>% 
    rename(!!new_name := value_col)
}

# === Load datasets with custom names ===
gender <- load_data("socioeconomic.csv",  "SC_SEC_GDF_2020", "GHS_female_gender_index")
hdi    <- load_data("socioeconomic.csv",  "SC_SEC_HDI_2020", "GHS_HDI")
lecz   <- load_data("exposure.csv",       "EX_L10_B23_2020", "GHS_builtup_below_10m")
# hazards   <- load_data("exposure.csv",       "HZ_CEV_DRO_2015", "GHS_builtup_below_10m")

# haz <- readxl:: read_xlsx("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "HAZARD_RISK")
# table(haz$HZ_CEV_DRO_2015)
# 
# t <- haz %>% 
#   left_join(ghsl, by = c("ID_UC_G0"))
# 
# cor(as.numeric(t$HZ_CEV_DRO_2015), t$hdd, use = "complete")
# cor(as.numeric(t$HZ_CEV_DRO_2015), t$CL_B12_CUR_2010, use = "complete")

################################################################################
# covariate lists and labels
################################################################################

co_vars <- c("GHS_population", "GHS_population_growth", "GHS_population_density", "GHS_population_density_growth",
             "GHS_GDP_PPP", "GHS_GDP_PPP_growth", 
             "GHS_critical_infra", "GHS_greenness_index", "GHS_precipitation",
             "hdd", "cdd")
co_vars_formatted <- c("Population", "Population growth", "Population density", "Population density growth", 
                       "GDP PPP", "GDP PPP growth", 
                       "Critical infrastructure", "Greenness", "Precipitation",
                       "Heating degree days", "Cooling degree days")
reg_vars <- c("NORTH-AMERICA", "SOUTH-AMERICA", "EUROPE", "AFRICA", "ASIA", "OCEANIA" , "SMALL ISLANDS")
reg_vars_wg2 <- c("North America", "South America", "Europe", "Africa", "Asia", "Australasia", "Small Islands")
  
cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Type 3",
    "Type 4",
    "Type 1", 
    "Type 2"
  )) %>% 
  mutate(cluster_name = factor(cluster_name, levels = c("Type 1",
                                                        "Type 2",
                                                        "Type 3",
                                                        "Type 4"))) %>% 
  mutate(cluster_name_break = c(
    "Mitigation\nfirst",
    "Mega\nall in",
    "Development\nfirst", 
    "Urban\nplanning\nfirst"
  ),
  cluster_name_break = factor(cluster_name_break, levels =  c(
    "Development\nfirst", 
    "Mitigation\nfirst",
    "Urban\nplanning\nfirst",
    "Mega\nall in"
  )))

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
    "cdd" = "Cooling degree days"
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
  select(-similarity, -entropy) %>% 
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
  slice_max(mean_prob) %>% 
  left_join(ghsl_clean, by= "GHS_urban_area_id") %>% 
  left_join(ghsl, by= c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  mutate(GHS_population = GHS_population/1000000,
         GHS_population_density = GHS_population_density/1000000,
         GHS_GDP_PPP = GHS_GDP_PPP/1000,
         hdd = hdd,
         cdd = cdd) %>% 
  select(GHS_urban_area_id, consensus_label_majority, 
         co_vars, similarity, mean_prob
         ) %>% 
  left_join(cites_ipcc_regions, by= c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)) %>% 
  # mutate(similarity = (similarity - min(similarity, na.rm = TRUE)) /
  #          (max(similarity, na.rm = TRUE) - min(similarity, na.rm = TRUE)),
  #        similarity_n_studies_per_city=similarity*n_studies
  #        ) %>% 
  group_by(consensus_label_majority, Region) %>% 
  arrange(consensus_label_majority, Region, -mean_prob) %>% 
  mutate(representative_city = row_number()<=2)


sort(unique(clust$consensus_label_majority))
# table(clust$consensus_label_majority, clust$representative_city)

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
         Region, mean_prob,
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
  filter(Year == 2022) 

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
  facet_wrap(~Region, nrow = 1) +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(p_emissions_box, file = "plots/p_emissions_box.pdf", width = 10, height = 5)



# calc_avg_growth <- function(x) {
#   # Remove leading NAs only
#   if(all(is.na(x))) return(NA)  # Entire series is NA
#   x <- x[seq(from = which(!is.na(x))[1], to = length(x))]
#   
#   # If less than 2 valid values after trimming, return NA
#   if(sum(!is.na(x)) < 2) return(NA)
#   
#   # Compute growth using available consecutive values
#   diffs <- diff(log(x), lag = 1)
#   mean(diffs, na.rm = TRUE)
# }
# 
# # Compute growth rates per group for ODIAC and EDGAR
# growth_rates <- emmissions_dat %>%
#   group_by(ID_UC_G0, GC_CNT_GAD_2025, GC_UCN_MAI_2025) %>%
#   filter(Year >=2017) %>% 
#   arrange(Year, .by_group = T) %>% 
#   summarise(
#     ODIAC_avg_growth = calc_avg_growth(ODIAC),
#     EDGAR_avg_growth = calc_avg_growth(EDGAR),
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     ODIAC_avg_growth_pct = (exp(ODIAC_avg_growth) - 1) * 100,
#     EDGAR_avg_growth_pct = (exp(EDGAR_avg_growth) - 1) * 100
#   )
# 
# growth_rates %>% 
#   left_join(clust %>% select(consensus_label_majority, GHS_urban_area_id), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
#   group_by(consensus_label_majority) %>% 
#   arrange(ODIAC_avg_growth_pct, .by_group = TRUE) %>% 
#   slice(1:10) %>% 
#   as.data.frame()

###########
# all data
###########

box_plot_add_covs_dat <- ghsl_clean %>% 
  left_join(ghsl %>% dplyr::select(ID_UC_G0, CL_B12_CUR_2010), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(clust %>% dplyr::select(GHS_urban_area_id, consensus_label_majority), by = "GHS_urban_area_id" ) %>% 
  left_join(gender, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(hdi, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(emmissions_box_dat %>% dplyr::select(ID_UC_G0, odiac_norm), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  dplyr::select(GHS_urban_area_id, consensus_label_majority, co_vars, 
                GHS_female_gender_index, GHS_HDI, odiac_norm) %>% 
  pivot_longer(-c(GHS_urban_area_id, consensus_label_majority), names_to = "variable") %>% 
  mutate(clustering = ifelse(variable %in% co_vars, "Clustering", "Outcomes")) %>% 
  rename_co_vars("variable") %>% 
  mutate(variable = ifelse(variable == "GHS_female_gender_index", "Female gender index", variable),
         variable = ifelse(variable == "GHS_HDI", "Human Development index", variable),
         variable = ifelse(variable == "odiac_norm", "CO2 emissions p.c.", variable),
         variable = factor(variable, levels = c(co_vars_formatted, "Female gender index", 
                                                "Human Development index", "CO2 emissions p.c."))) %>% 
  left_join(cluster_names, by = c("consensus_label_majority" = "consensus_label_majority")) %>% 
  group_by(variable) %>%
  mutate(
    # adjusted_value = ifelse(value == 0, 1e-4 *  mean(abs(value), na.rm = TRUE), value),
    # normalized_value = sign(adjusted_value) * log2(abs(adjusted_value) / mean(abs(value), na.rm = TRUE))
    normalized_value = value / mean(value, na.rm = TRUE)) %>%
  ungroup() 




###########
# prepare maps
###########

desc_geo <- ghsl %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>%
  left_join(clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
  left_join(cluster_names, by = "consensus_label_majority")

# Compute centroids for each region
desc_geo$centroid <- st_centroid(desc_geo$geom)

to_label <- desc_geo %>% 
  filter(GHS_population >= 1) %>% 
  group_by(cluster_name, Region) %>% 
  arrange(-mean_prob) %>% 
  slice_max(mean_prob, n = 3)

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
    arrange(representative_city) %>% 
    ggplot() +
    geom_sf(data = world, fill = "grey90", color = "white") +  # World map with light gray color
    geom_sf(aes(geometry = centroid, 
                col = mean_prob, fill = mean_prob, alpha = mean_prob, size = mean_prob), lwd = 0
            # , size = .5
            ) + 
    ggrepel::geom_label_repel(
      data = to_label %>% 
        filter(cluster_name == cluster),
      aes(label = GC_UCN_MAI_2025, 
          geometry = centroid),
      stat = "sf_coordinates", alpha=.5, size = 3.5, 
    ) +
    # scale_color_manual(values = c("#ffb84d", "black")) +
    # scale_color_manual(values = c("#ffb84d", "black")) +
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
fig1 <- plot_grid(plotlist = box_plot_list, ncol = 2, labels = "auto", align = "v")
ggsave(fig1, filename = "plots/fig1.pdf", width = 10, height = 10)

ggsave(box_plot_list[[1]], filename = "plots/type_1.pdf", width = 5, height = 3.8)
ggsave(box_plot_list[[2]], filename = "plots/type_2.pdf", width = 5, height = 3.8)
ggsave(box_plot_list[[3]], filename = "plots/type_3.pdf", width = 5, height = 3.8)
ggsave(box_plot_list[[4]], filename = "plots/type_4.pdf", width = 5, height = 3.8)


# Compute means by cluster and variable
means_df <- box_plot_add_covs_dat %>%
  group_by(clustering, cluster_name, variable) %>%
  summarise(mean_val = mean(normalized_value, na.rm = TRUE), .groups = "drop") 

scales <- list(
  # Here you have to specify all the scales, one for each facet row in your case
  scale_y_continuous(limits = c(0, 10)),
  scale_y_continuous(limits = c(0, 10)),
  scale_y_continuous(limits = c(0, 10)),
  scale_y_continuous(limits = c(-2, 17))
)

# show_legend <- cluster == "Mega all in"

p_box_characteristics <- box_plot_add_covs_dat %>% 
  ggplot(aes(x = variable, y = normalized_value)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_violin(alpha = 0.5, color = NA, scale = "width", aes(fill = cluster_name)) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, color = "grey", outliers = F, aes(fill = cluster_name)) +
  scale_fill_manual(values = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))) +
  geom_point(
    data = means_df,
    aes(x = variable, y = mean_val, shape = "Mean value by type"),
    size = 2, fill = "darkred", color = "black", inherit.aes = FALSE
  ) +
  geom_label(
    data = means_df,
    aes(x = variable, y = mean_val, label = round(mean_val, 2)),
    vjust = -0.8, size = 2.2, fill = "white", color = "black",      # color of the label text and border
    label.size = 0, alpha = 0.7, inherit.aes = FALSE
  ) + 
  scale_shape_manual(values = c("Mean value by type" = 21), name = "") +
  # Y-scale from 1/8 to 8
  # scale_y_continuous(
  #   trans = "log2",
  #   breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
  #   labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
  #   limits = c(0.01, 15)
  # ) +
  # facet_grid(cluster_name~clustering, scales = "free", space = "free_x") +
  ggh4x::facet_grid2(cluster_name~clustering, scales = "free", space = "free_x") +
  ggh4x::facetted_pos_scales(y = scales) +
  # ylim(-2,17) +
  theme_SM() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = c(.51,.16),
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

###############################################################################
# mean probs across Regions: TODO - move to ucertainty script
################################################################################

# Summarize count per Region and cluster_name
count_df <- desc_geo %>%
  as.data.frame() %>%
  group_by(Region, cluster_name) %>%
  summarise(n = n(), .groups = "drop")

# Plot with annotation
p_mean_prob_cont <- desc_geo %>%
  as.data.frame() %>%
  ggplot(aes(x = Region, y = mean_prob, group = Region)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~cluster_name) +
  geom_text(
    data = count_df,
    aes(x = Region, y = 0.95, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    title = "Mean probability by Region and Cluster",
    x = "Region",
    y = "Probability distribution"
  ) +
  ylim(c(0.25, 1)) + 
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p_mean_prob_cont, file = "plots/p_mean_prob_cont.pdf", width = 10, height = 6)

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
          axis.title = element_text(size = 8),
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
      # trans = "log2",
      # breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
      # labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
      # limits = c(-5, 30)
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
city_names <- c("Santiago de Cuba", "Cartagena", "Mombasa", "Cancún", "Basra", "Makassar", "Berlin", "Melbourne", "Louisville", "Chongqing")
city_ids <- ghsl %>% filter(GC_UCN_MAI_2025 %in% city_names)
city_ids <- city_ids %>% filter(!(GC_UCN_MAI_2025 == "Cartagena" & GC_CNT_GAD_2025 == "Spain")) %>% as.data.frame()
city_ids <- city_ids[match(city_names, city_ids$GC_UCN_MAI_2025), ] %>% pull(ID_UC_G0)
# city_ids <- c(2902, 3275, 2869, 5483, 8035, 8557, 11185, 11508)

# Run batch plotting
plot_multiple_cities(
  # city_names = city_names,
  ghsl = ghsl,
  city_ids = city_ids,
  clust_probs = clust_probs,
  covariate_data = box_plot_add_covs_dat,
  output_dir = "plots/figA3.pdf",
  # height = 12
)


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


# analyse and plot learning potential
library(dplyr)
library(matrixStats)
library(purrr)

examples_dat <- ghsl %>% ## TODO: right continent definition
  mutate(geom = st_centroid(geom)) %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_DEV_USR_2025, geom) %>%
  left_join(clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
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
  filter(!ID_UC_G0 %in% c(1624, 9932)) %>% # Shahadi, Baragashi are GHSL data errors
  filter(ID_UC_G0 %in% results_df$learning_ids)

examples_dat_teaching <- examples_dat_teaching %>% 
  filter(ID_UC_G0 %in% unique(results_df$teaching_ids))


examples_dat_teaching_coords <- examples_dat_teaching %>%
  mutate(
    lon = st_coordinates(geom)[,1],  # X = longitude
    lat = st_coordinates(geom)[,2]   # Y = latitude
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

fig3 <- ggplot() +
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
  
  # scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  
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
    legend.position = c(0.01, 0.4),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.title.position = "top",
    legend.direction = "vertical",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.box = "vertical",
    legend.spacing.y = unit(.05, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
    legend.box.margin = margin(rep(2, 4)),
    text = element_text(size = 8), 
    plot.margin = margin(rep(-1, 4)),
  )

fig3
ggsave(fig3, file = "plots/fig3.pdf", height = 4.5, width = 10)


fig4 <- case_ex %>%
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
fig4

ggsave(fig4, file = "plots/fig4.pdf", width = 5, height = 5)


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

##################################################################
# in-depth case studies
##################################################################

# # for each type, check which are the 3 continents with most cities?
# cities_per_type_and_region <- clust_probs %>% 
#   left_join(cites_ipcc_regions, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#   filter(secondary_cluster_name == cluster_name) %>% 
#   group_by(cluster_name, Region) %>% 
#   count() %>% 
#   arrange(cluster_name, -n) %>% 
#   group_by(cluster_name) %>% 
#   filter(n > 10)
#   # slice(1:5)
# 
# # select a few cities for each type with the highest assignment probability
# selected_city_ids <- c()
# # define sigmoid function
# sigmoid <- function(x) 1 / (1 + exp(-x))
# for (row in 1:nrow(cities_per_type_and_region)) {
#   
#   selected_city_id <- clust %>%
#     ungroup() %>%
#     left_join(cluster_names, by = "consensus_label_majority") %>%
#     filter(
#       cluster_name == cities_per_type_and_region$cluster_name[row],
#       Region == cities_per_type_and_region$Region[row]
#     ) %>%
#     mutate(
#       prob_rescaled = sigmoid(mean_prob),
#       n_studies_rescaled = sigmoid(n_studies),
#       studies_prob = (prob_rescaled ^ 0.1) * (n_studies_rescaled ^ 0.9)
#     ) %>% 
#     arrange(desc(n_studies_rescaled)) %>%  # now use the combined score for ranking
#     slice(1:5) %>%
#     pull(GHS_urban_area_id)
#   
#   selected_city_ids <- c(selected_city_ids, selected_city_id)
# }
# selected_city_ids
# 
# t <- clust %>% 
#   filter(GHS_urban_area_id %in% selected_city_ids) %>%
#   as.data.frame() %>% 
#   left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), 
#             by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#   select(GHS_urban_area_id, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region, consensus_label_majority, n_studies, mean_prob) %>%
#   left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>% 
#   left_join(main_topic %>% select(document, group1, group2), by = c("id" = "document")) %>% 
#   left_join(oa %>% select(id, title, abstract), by = "id") %>% 
#   left_join(cluster_names, by = "consensus_label_majority") %>% 
#   as_tibble() 
# 
# # summarise data
# heatmap_data <- t %>%
#   group_by(GHS_urban_area_id, GC_UCN_MAI_2025, Region, cluster_name, group1, group2) %>%
#   count(name = "n") %>%
#   ungroup()
# 
# # normalise fills within each facet
# heatmap_data_norm <- heatmap_data %>%
#   group_by(cluster_name) %>%
#   mutate(n_norm = log(n / max(n, na.rm = TRUE))) %>%
#   ungroup()
# 
# totals <- heatmap_data %>% 
#   group_by(GHS_urban_area_id, GC_UCN_MAI_2025, Region, cluster_name) %>% 
#   summarise(n = sum(n)) %>%
#   ungroup() %>% 
#   mutate(n_norm = log(n / max(n, na.rm = TRUE))) %>%
#   mutate(group1 = "Total", group2 = "Total") %>% 
#   ungroup()
# 
# heatmap_data_norm <- heatmap_data_norm %>% 
#   bind_rows(heatmap_data_norm, totals)
# 
# probs <- t %>% 
#   filter(GHS_urban_area_id %in% heatmap_data$GHS_urban_area_id) 
# 
# # plot
# library(ggh4x)
# 
# fig4 <- heatmap_data_norm %>%
#   filter(!is.na(group1)) %>%
#   ggplot(aes(x = GC_UCN_MAI_2025, y = group2, fill = n_norm)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = n), color = "white", size = 2.5) +
#   facet_nested(
#     rows = vars(group1),
#     cols = vars(cluster_name, Region),   
#     scales = "free",
#     space = "free"
#   ) +
#   scale_fill_viridis_c(option = "A") +
#   labs(
#     x = "",
#     y = "",
#     # fill = "Normalised intensity"
#   ) +
#   theme_SM() +
#   theme(strip.placement = "inside",
#         strip.background = element_rect(color = "grey"),
#         axis.title = element_blank()
#         # strip.background.x=element_rect(color = NA)
#         ) + 
#   theme(legend.position = "none")
# 
# fig4
# 
# ggsave(fig4, file = "plots/fig4.pdf", width = 13, height = 5)

# as.data.frame() %>% 
  # left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), 
  #           by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  # # select(GHS_urban_area_id, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region, consensus_label_majority, n_studies, mean_prob) %>% 
  # left_join(cluster_names, by = "consensus_label_majority") %>% 
  # as_tibble()

# case_selection_bar <- t %>%
#   group_by(GHS_urban_area_id, cluster_name, GC_UCN_MAI_2025,group1, group2) %>% 
#   count() %>% 
#   group_by(GHS_urban_area_id) %>% 
#   mutate(total = sum(n)) %>% 
#   # Use reorder with ID for ordering, but use GC_UCN_MAI_2025 for labels
#   ggplot(aes(x = reorder(GC_UCN_MAI_2025, total), y = n)) +
# 
#   geom_bar(stat = "identity", fill = "#963d03", col = "black") +
#   coord_flip() +
#   facet_wrap(group1~cluster_name, scales = "free") +
#   labs(
#     x = "",
#     y = "Number of Studies"
#   ) +
#   theme_SM()
# case_selection_bar
# 
# t <- clust %>% 
#   # filter(GHS_urban_area_id %in% selected_city_ids) %>% 
#   # left_join(cites_ipcc_regions, by = c("ID_UC_G0")) %>% 
#   as.data.frame() %>% 
#   left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), 
#             by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
#   select(GHS_urban_area_id, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region, consensus_label_majority) %>%
#   left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>% 
#   left_join(main_topic %>% select(document, group1, group2), by = c("id" = "document")) %>% 
#   left_join(oa %>% select(id, title, abstract), by = "id") %>% 
#   left_join(cluster_names, by = "consensus_label_majority") %>% 
#   as_tibble() 
# 
# gammas <- readRDS("/Users/simon/Documents/repo/cities-learning/data/topic_model/td_gammas_220.rds")
# 
# gammas_220 <- gammas %>% 
#   filter(K == 220) %>% 
#   unnest(model) %>% 
#   left_join(labelled_topics %>% select(topic, name, group1, group2)) %>% 
#   filter(group1 != "Other") %>% 
#   
# 
# t %>% 
#   ggplot(aes())
# 
# t %>% group_by(cluster_name, GHS_urban_area_id, GC_UCN_MAI_2025, group1, group2) %>% count()
# 
# t %>% 
#   group_by()
# 
# t$title[9]
# t$abstract[9]
# t$title[10]
# t$abstract[10]
# t$title[3]
# t$abstract[3]
# 
# t %>% 
#   group_bootstraps()
# 
# ##################################################################
# # type by continent
# ##################################################################
# 
# pop_by_type <- ghsl %>%
#   as.data.frame() %>%
#   left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority, GHS_population), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
#   group_by(Region, consensus_label_majority) %>%
#   reframe(pop = sum(GHS_population)) %>%
#   mutate(pop_share = pop/sum(pop)) %>%
#   group_by(consensus_label_majority) %>%
#   mutate(pop_share_norm = pop_share/mean(pop_share))
# 
# p_type_by_cont <- clust %>%
#   mutate(Region = factor(Region, levels = reg_vars_wg2)) %>% 
#   group_by(consensus_label_majority, Region) %>%
#   reframe(number_of_studies = sum(n_studies)) %>%
#   group_by(Region) %>%
#   mutate(research_share = number_of_studies/sum(number_of_studies),
#          research_share_norm = research_share/mean(research_share)) %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>%
#   left_join(pop_by_type, by = c("consensus_label_majority", "Region")) %>%
#   mutate(research_share_to_pop_share = research_share_norm/pop_share_norm) %>%
#   ggplot(aes(Region, cluster_name, fill = log2(research_share_to_pop_share))) +
#   geom_tile(height = .95, width = .95) +
#   scale_fill_gradient2(
#     low = "darkred", mid = "grey", high = "darkslateblue", midpoint = 1,
#   ) +
#   geom_text(aes(label = paste0(number_of_studies, "\n", "(", round(research_share_to_pop_share, 2), ")")),
#             size = 2.3, lineheight = .8, col = "white") +
#   scale_color_manual(values = c("black", "white")) +
#   theme_SM() +
#   labs(y = "", x = "", fill = "Studies") +
#   theme(legend.position = "none",
#         legend.direction = "horizontal",
#         legend.title = element_text(),
#         axis.text = element_text(size = 7),
#         legend.text = element_text(angle = 45, hjust = 1))
# 
# 
# ggsave(p_type_by_cont, file = "plots/p_type_by_cont.pdf", width = 5, height = 4)

##################################################################
# exp post characterisation
##################################################################
# 
# 
# 
# box_plot_add_covs_by_clust <- box_plot_add_covs_dat %>% 
#   ggplot(aes(x = cluster_name, y = normalized_value)) +
#   geom_violin(alpha = 0.9, color = NA, scale = "width", aes(fill = clustering)) +
#   scale_fill_manual(values = c("#ffe0a3", "cornflowerblue")) + 
#   geom_boxplot(width = 0.15, outlier.size = 0.5, color = "grey", outliers = F) +
#   geom_point(
#     data = means_df,
#     aes(x = cluster_name, y = mean_val, shape = "Mean"),
#     size = 2, fill = "darkred", color = "black"
#   ) +
#   scale_shape_manual(values = c("Mean" = 21), name = "") +
#   facet_wrap(~variable, scales = "free_y") +
#   # Y-scale from 1/8 to 8
#   scale_y_continuous(
#     trans = "log2",
#     breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
#     labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
#     limits = c(0.1, 12)
#   ) +
#   geom_hline(yintercept = 1, lty = 2) +
#   coord_flip() +
#   theme_SM() +
#   theme(
#     # axis.text.x = element_text(angle = 45, hjust = 1),
#     # panel.spacing.y = unit(-2, "lines"),
#     legend.position = c(.96,.1),
#     legend.justification = "right",
#     legend.box.just = "right"
#     # plot.margin = unit(c(-.5,0,-.8,0), "cm")
#   ) +
#   labs(
#     x = "",
#     y = "Normalized Value (log scale)",
#     title = ""
#   )
# 
# ggsave(box_plot_add_covs_by_clust, file = "plots/box_plot_add_covs_by_clust.pdf", height = 7, width = 10)



################################################################################
# case selection
################################################################################
# min_max_scale <- function(x){
#   res = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#   return(res)
# }
# 
# 
# 
# best_30_per_group <- n_studies_per_city %>% 
#   arrange(-n_studies) %>% 
#   left_join(ghsl %>% dplyr::select(GC_UCN_MAI_2025, ID_UC_G0), by = c("city_id" = "ID_UC_G0")) %>% 
#   left_join(clust %>% dplyr::select(GHS_urban_area_id, consensus_label_majority, similarity), 
#             by = c("city_id" = "GHS_urban_area_id")) %>% 
#   # filter(city_id)
#   filter(city_id != 2255) %>% 
#   as.data.frame() %>% 
#   group_by(consensus_label_majority) %>% 
#   mutate(
#     similarity_norm = min_max_scale(similarity),
#     n_studies_norm = min_max_scale(n_studies),
#     best_cases = similarity_norm * n_studies_norm
#   ) %>% 
#   mutate(best_cases = row_number() < 30) %>% 
#   ungroup() %>% 
#   filter(best_cases, !is.na(GC_UCN_MAI_2025)) %>% 
#   left_join(cluster_names, by = "consensus_label_majority") 
# 
######### needs to be checked
# desc_geo_case_selection <- desc_geo %>% 
#   as.data.frame() %>% 
#   group_by(consensus_label_majority) %>% 
#   mutate(similarity_norm = min_max_scale(similarity),
#          n_studies_norm = min_max_scale(n_studies),
#          best_cases = similarity_norm*n_studies_norm
#   ) %>% 
#   mutate(best_cases = row_number()<30) %>% 
#   ungroup()
# 
# case_selection <- list()
# min_cl <- min(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
# max_cl <- max(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
# for (cluster in min_cl:max_cl) {
#   
#   case_selection[[cluster+1]] <- desc_geo %>% 
#     filter(consensus_label_majority == cluster) %>% 
#     left_join(best_30_per_group %>% dplyr::select(city_id, best_cases), by = c("ID_UC_G0" = "city_id")) %>% 
#     mutate(best_cases = ifelse(is.na(best_cases), FALSE, best_cases)) %>% 
#     ggplot() + 
#     geom_sf(data = world, fill = "grey90", color = "white") +  # World map with light gray color
#     geom_sf(aes(geometry = centroid, col = best_cases), size = .5, alpha = .3) +  
#     ggrepel::geom_label_repel(
#       aes(label = ifelse(best_cases == T, GC_UCN_MAI_2025, NA), geometry = centroid),
#       stat = "sf_coordinates", alpha=.5, size = 1.5,  max.overlaps = 50,
#     ) +
#     scale_color_manual(values = c("#ffb84d", "black")) +
#     geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
#     coord_sf(crs = proj_robin) + 
#     annotate(
#       "label",
#       x = -Inf, y = Inf,
#       label = cluster_names$cluster_name[cluster_names$consensus_label_majority == cluster],
#       hjust = 0, vjust = .9,
#       size = 3,
#       fill = "white",
#       label.size = 0.3
#     ) + 
#     theme_SM() +
#     theme(
#       panel.border = element_rect(color = NA),
#       legend.position = "none",
#     ) +
#     labs(col = "Cities", x = "", y = "")
# }
#   
# case_selection_maps <- plot_grid(plotlist = case_selection, ncol = 2, labels = "auto", align = "v")
# ggsave(case_selection_maps, filename = paste0("plots/case_selection_maps.pdf"), width = 10, height = 6)
# 
# 
# case_selection_bar <- best_30_per_group %>% 
#   # Use reorder with ID for ordering, but use GC_UCN_MAI_2025 for labels
#   ggplot(aes(x = reorder(GC_UCN_MAI_2025, n_studies), y = n_studies)) +
#   
#   geom_bar(stat = "identity", fill = "#963d03", col = "black") +
#   coord_flip() +
#   facet_wrap(~cluster_name, scales = "free") +
#   labs(
#     x = "",
#     y = "Number of Studies"
#   ) +
#   theme_SM()
# 
# ggsave(case_selection_bar, file = "plots/case_selection_bar.pdf", width = 10, height = 8)

# library(ggplot2)
# library(ggpubr)
# library(scales)
# 
# # Population share plot
# p_pop_share <- clust %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>% 
#   group_by(cluster_name) %>%
#   summarise(GHS_population = sum(GHS_population), .groups = "drop") %>%
#   mutate(pop = GHS_population / sum(GHS_population)) %>%
#   ggplot(aes(x = cluster_name, y = pop)) +
#   geom_bar(stat = "identity", color = "black", fill = "lightblue") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(x = "Cluster", y = "Population Share") +
#   theme_SM() +
#   theme(plot.title = element_blank())
# 
# # City share plot
# p_city_share <- clust %>%
#   left_join(cluster_names, by = "consensus_label_majority") %>% 
#   group_by(cluster_name) %>%
#   summarise(n_cities = n(), .groups = "drop") %>%
#   mutate(share_cities = n_cities / sum(n_cities)) %>%
#   ggplot(aes(x = cluster_name, y = share_cities)) +
#   geom_bar(stat = "identity", color = "black", fill = "lightblue") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(x = "Cluster", y = "City Share") +
#   theme_SM() +
#   theme(plot.title = element_blank())
# 
# # Combine plots
# p_pop_and_city_share <- ggarrange(
#   p_pop_share,
#   p_city_share,
#   labels = c("a", "b"),
#   ncol = 2,
#   align = "hv"
# )
# 
# ggsave("plots/p_pop_and_city_share.pdf", p_pop_and_city_share, height = 7, width = 10)


library(scales)

# Population share by Region per cluster
p_pop_share_cont <- clust %>%
  left_join(cluster_names, by = "consensus_label_majority") %>% 
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
  left_join(cluster_names, by = "consensus_label_majority") %>% 
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
# types by IPCC region
################################################################################
# 
# ipcc_regions <- st_transform(ipcc_regions, crs = 4326)
# desc_geo <- st_transform(desc_geo, crs = 4326)
# 
# # Assuming desc_geo is a dataframe with lat/long and consensus_label_majority
# desc_geo_sf <- st_as_sf(desc_geo, coords = c("longitude", "latitude"), crs = 4326)
# 
# # Assuming ipcc_regions is an sf object with the polygons of the regions
# ipcc_regions <- st_make_valid(ipcc_regions)
# desc_geo_sf <- st_make_valid(desc_geo_sf)
# desc_geo_sf <- st_join(desc_geo_sf, ipcc_regions) 
# 
# desc_geo_sf <- desc_geo_sf %>% 
#   mutate(Acronym = ifelse(Acronym %in% c("EPO", "NPO"), "PAC", Acronym)) 
# 
# cluster_share_most_prevalent <- desc_geo_sf %>%
#   as.data.frame() %>%
#   group_by(Acronym, consensus_label_majority) %>%
#   summarise(n = n()) %>%
#   group_by(Acronym) %>%
#   mutate(share = n/sum(n)) %>% 
#   arrange(-share, .by_group = T) %>% 
#   slice(1:3) %>% 
#   mutate(triangle_3_id = row_number())
# 
# ipcc_regions_hexa_split <- ipcc_regions_hexa_split %>% 
#   # check this, probably needs to be broader and also include the other island cities
#   mutate(label = ifelse(label %in% c("EPO", "NPO"), "PAC", label)) %>% 
#   mutate(triangle_3_id  = case_when(triangle_id <=2 ~ 1, 
#                                     triangle_id >2 & triangle_id <5 ~ 2,
#                                     triangle_id >=5 ~ 3,)) %>% 
#   left_join(cluster_share_most_prevalent, by = c("label" = "Acronym", "triangle_3_id")) 
# 
# # Function to scale triangle inward from centroid using share
# scale_triangle <- function(triangle, share) {
#   coords <- st_coordinates(triangle)[, 1:2]
#   
#   # Get points
#   p1 <- coords[1, ]
#   p2 <- coords[2, ]
#   centroid <- coords[3, ]  # assumed order is [p1, p2, centroid, p1]
#   
#   # Compute scaled points
#   s <- share
#   new_p1 <- centroid + s * (p1 - centroid)
#   new_p2 <- centroid + s * (p2 - centroid)
#   
#   # Make polygon
#   new_coords <- rbind(new_p1, new_p2, centroid, new_p1)
#   st_polygon(list(new_coords)) %>% st_sfc(crs = st_crs(triangle))
# }
# 
# # Apply to each triangle
# scaled_triangles <- purrr::pmap_dfr(ipcc_regions_hexa_split, function(geom, share, hex_id, triangle_id, label, triangle_3_id, consensus_label_majority,...) {
#   if (is.na(share)) return(NULL)
#   new_geom <- scale_triangle(geom, share)
#   st_sf(geometry = new_geom, share = share, hex_id = hex_id, triangle_id = triangle_id, label = label, triangle_3_id = triangle_3_id, consensus_label_majority = consensus_label_majority)
# })
# 
# # reassign crs
# scaled_triangles <- st_set_crs(scaled_triangles, st_crs(ipcc_regions_hexa_split))
# 
# # group by the same types
# scaled_triangles <- scaled_triangles %>% 
#   group_by(hex_id, triangle_3_id) %>% 
#   summarise(geometry = st_union(geometry), 
#             share = first(share), 
#             label = first(label), 
#             consensus_label_majority = first(consensus_label_majority)) %>% 
#   left_join(cluster_names, by = "consensus_label_majority")
# 
# 
# 
# library(ggpattern)
# library(ggsci)
# p_regional_types <- ggplot() + 
#   geom_sf(data = ipcc_regions_hexa, fill = NA) + 
#   # geom_sf(data = scaled_triangles, aes(fill = as.factor(consensus_label_majority))) +
#   geom_sf_pattern(
#     data = scaled_triangles,
#     aes(pattern_colour = as.factor(cluster_name),
#         pattern_angle = as.factor(cluster_name)
#       ),
#     pattern_density = .2,
#     pattern_spacing = 0.01,
#     # pattern_alpha = .5,
#     pattern_size = .05,
#     pattern_fill = "grey", 
#     pattern_key_scale_factor = 0.6, 
#     lwd = .5
#   ) +
#   scale_fill_npg() +
#   geom_sf(data = ipcc_regions_hexa_groupings) +
#   geom_label(
#     data = ipcc_regions_hexa_groupings,
#     aes(label = Name, x = auxiliary_storage_labeling_positionx, y = auxiliary_storage_labeling_positiony),
#     alpha=.5, size = 2, label.size = NA
#   ) + 
#   theme_SM() + 
#   labs(x= "", y = "") + 
#   theme(panel.border = element_blank(), 
#         axis.ticks = element_blank(), 
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(), 
#         legend.position = "bottom")
# ggsave(p_regional_types, filename = "plots/p_regional_types.pdf", width = 10, height = 5)
# 
# ################################################################################
# # learning potential by cluster overall
# ################################################################################
# 
# 
# 
# # p_cities_per_cluster_share <- clust %>% 
# #   left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>% 
# #   mutate(any_study_true = ifelse(n_studies > 1 & !is.na(n_studies), T, F)) %>% 
# #   group_by(consensus_label_majority, any_study_true) %>% 
# #   summarise(n_cities = n()) %>% 
# #   ggplot(aes(x = consensus_label_majority, y = n_cities, fill = any_study_true)) + 
# #   geom_bar(stat = "identity", position = "fill") +  
# #   scale_fill_manual(values = c("#963d03", "#825d69")) +
# #   coord_flip() +
# #   scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
# #   theme_SM() + 
# #   labs(x = "Cluster", y = "Numer of Cities")
# # p_cities_per_cluster_share
# # ggsave(p_cities_per_cluster_share, file = "plots/p_cities_per_cluster_share.pdf", width = 10, height = 6)
# 
# 
# clust_with_topics <- clust %>%
#   left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>%
#   left_join(main_topic, by = c("id" = "document")) %>%
#   # left_join(labelled_topics %>% select(topic, name, group1, group2), by = c("topic")) %>% 
#   left_join(oa %>% select(id, abstract, publication_year), by = "id") %>% 
#   as_tibble()
# 
# n_case_studies_per_cluster <- clust_with_topics %>% 
#   group_by(id) %>% 
#   slice(1) %>% 
#   group_by(consensus_label_majority) %>% 
#   summarise(n_studies = n())
# 
# n_cities_per_cluster <- clust %>% 
#   group_by(consensus_label_majority) %>% 
#   summarise(n_cities = n())
# 
# n_pop_per_clust <- ghsl %>% 
#   left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
#   group_by(consensus_label_majority) %>% 
#   summarise(n_pop = sum(GC_POP_TOT_2025))
# 
# p_over_under_researched_by_clust <- left_join(n_case_studies_per_cluster, n_cities_per_cluster, by = "consensus_label_majority") %>% 
#   left_join(n_pop_per_clust, by = "consensus_label_majority") %>% 
#   mutate(share_studies = n_studies/sum(n_studies),
#          share_cities = n_cities/sum(n_cities),
#          share_pop = n_pop/sum(n_pop),
#          ratio_with_cities = share_studies/share_cities,
#          ratio_with_pop = share_studies/share_pop,
#   ) %>% 
#   pivot_longer(c(
#     ratio_with_cities,
#                  ratio_with_pop), names_to = "ratio_type", values_to = "ratio") %>% 
#   mutate(ratio_type = ifelse(ratio_type == "ratio_with_cities", "City share", "Population share")) %>% 
#   left_join(cluster_names, by = "consensus_label_majority") %>% 
#   ggplot(aes(x = cluster_name, y = ratio, shape = ratio_type, col = ratio_type)) +  
#   geom_point(size = 3) + 
#   scale_color_manual(values = c("#a892c7", "#a892c7")) +
#   scale_shape_manual(values = c(1, 17)) +
#   labs(x = "", y = expression(paste('log2', "(research share)")), col = "Normalization", shape = "Normalization", subtitle = "Normalised research share") +
#   scale_y_continuous(transform = "log2", 
#                      breaks = c(.125, .25, 0.5, 1, 2, 4, 8, 16),
#                      labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16")) +
#   coord_flip() +
#   # scale_x_discrete(limits=rev) +
#   geom_hline(aes(yintercept = 1), lty = 2, alpha = 0.7, color = "gray50") +
#   theme_SM() + 
#   theme(legend.title = element_text(), 
#         axis.title = element_text(size = 10),
#         legend.position = c(.95,.1))
# 
# ggsave(p_over_under_researched_by_clust, file = "plots/p_over_under_researched_by_clust.pdf", width = 5, height = 5)
# 
# # p_cities_and_over_under_per_clus <- ggarrange(p_cities_per_cluster_n, p_over_under_researched_by_clust, labels = c("a", "b"))
# # ggsave(p_cities_and_over_under_per_clus, file = "plots/p_cities_and_over_under_per_clus.pdf", width = 10, height = 5)
# 
# ################################################################################
# # learning potential by cluster by g1 topic
# ################################################################################
# # 
# # cities_g2_topics <- ghsl %>% 
# #   as.data.frame() %>% 
# #   select(ID_UC_G0, GC_UCN_MAI_2025) %>% 
# #   left_join(clean_places, by = c("ID_UC_G0" = "city_id")) %>% 
# #   left_join(main_topic %>% select(topic, gamma, document), by = c("id" = "document")) %>% 
# #   left_join(labelled_topics %>% select(topic, name, group1, group2), by = c("topic")) %>% 
# #   group_by(ID_UC_G0, group2, group1) %>% 
# #   summarise(n_studies = sum(!is.na(group2)), .groups = "drop") %>% 
# #   left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id")) 
# # 
# # # Step 1: Get all unique group2 values (excluding NA)
# # group2_vals <- cities_g2_topics %>%
# #   filter(!is.na(group2)) %>%
# #   distinct(group2)
# # 
# # # Step 3: Create a balanced panel of all combinations
# # balanced_panel <- expand.grid(
# #   ID_UC_G0 = unique(cities_g2_topics$ID_UC_G0),
# #   group2 = group2_vals$group2,
# #   stringsAsFactors = FALSE) %>%
# #   left_join(cities_g2_topics, by = c("ID_UC_G0", "group2")) %>%
# #   left_join(clust %>% distinct(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id"), suffix = c("", "_from_cluster")) %>%
# #   mutate(
# #     n_studies = ifelse(is.na(n_studies), 0, n_studies),
# #     consensus_label_majority = coalesce(consensus_label_majority, consensus_label_majority_from_cluster)
# #   ) %>%
# #   select(ID_UC_G0, group2, n_studies, consensus_label_majority)
# # 
# # summary_df <- balanced_panel %>%
# #   mutate(research_presence = ifelse(n_studies > 0, "Has research", "No research")) %>%
# #   left_join(labelled_topics %>% distinct(group1, group2), by = "group2") %>%
# #   group_by(consensus_label_majority, group1, group2, research_presence) %>%
# #   summarise(n_cities = n_distinct(ID_UC_G0), .groups = "drop") %>% 
# #   filter(!group1 %in% c("Other", "Impacts")) %>% 
# #   group_by(consensus_label_majority, group1, group2) %>%
# #   mutate(
# #     total_cities = sum(n_cities),
# #     pct = round((n_cities / total_cities) * 100, 0),
# #     label = paste0(pct, "%")
# #   ) %>%
# #   ungroup() %>% 
# #   left_join(cluster_names, by = "consensus_label_majority")
# # 
# # # labels_vec <- setNames(str_replace_all(
# # #   cluster_names$cluster_name,
# # #   paste0("((?:\\S+\\s+){3})"),
# # #   "\\1\n"
# # # ), as.character(cluster_names$consensus_label_majority))
# # 
# # figA5 <- ggplot(summary_df, aes(x = group2, y = pct/100, fill = research_presence)) +
# #   geom_bar(stat = "identity", position = "fill", width = .5, col = "black", size=0.1) +
# #   facet_grid(group1 ~ cluster_name, scales = "free_y", space = "free") +
# #   geom_text(aes(label = ifelse(label == "0%", "", label)),
# #             position = position_stacknudge(x = .4, vjust = 0.5),
# #             size = 2.5,
# #             color = "black") +
# #   labs(
# #     title = "Cities with and without Research by Topic and Cluster",
# #     x = "",
# #     y = "",
# #     fill = "Research Status"
# #   ) +
# #   scale_fill_manual(values = c("#a892c7", "#f2f2f0")) +  
# #   scale_y_continuous(labels = scales::percent) +
# #   coord_flip() + 
# #   theme_SM() +
# #   theme(legend.position = "bottom", 
# #         axis.title = element_text(size = 10),
# #         strip.text = element_text(size = 7),
# #         axis.text = element_text(size = 7))
# # ggsave(figA5, file = "plots/figA5.pdf", width = 10, height = 7)
# # 
# # # figA5 <- ggarrange(p_cities_and_over_under_per_clus, p_learning_by_topic, labels = c("", "c"), nrow = 2, heights = c(1,3))
# # # ggsave(figA5, file = "plots/figA5.pdf", width = 10, height = 10)

################################################################################
# evidence growth by group
################################################################################

clust_with_topics <- clust %>%
  left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>%
  # left_join(main_topic, by = c("id" = "document")) %>%
  # left_join(labelled_topics %>% select(topic, name, group1, group2), by = c("topic")) %>%
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
  ) %>% 
  left_join(cluster_names, by = "consensus_label_majority") 

# Aggregate: n_studies per phase per cluster and Region
growth_by_phase <- data_phase %>%
  filter(!is.na(phase)) %>%
  group_by(phase, cluster_name, Region) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  mutate(phase = factor(phase, levels = c("AR1", "AR2", "AR3", "AR4", "AR5", "AR6"))) %>%
  arrange(cluster_name, Region, phase) %>%
  # summarise(n_studies = sum(n_studies)) %>% 
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
    y = 130  # place slightly above the typical range of norm_growth
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
    # Region = ifelse(Region == "Australia", "Oceania", Region),
    phase = case_when(
      publication_year < 1990 ~ "AR1",
      publication_year > 1990 & publication_year <= 1995 ~ "AR2",
      publication_year > 1995 & publication_year <= 2001 ~ "AR3",
      publication_year > 2001 & publication_year <= 2007 ~ "AR4",
      publication_year > 2007 & publication_year <= 2014 ~ "AR5",
      publication_year > 2014 & publication_year <= 2022 ~ "AR6",
      publication_year > 2022 ~ "AR7"
    )
  ) %>% 
  left_join(cluster_names, by = "consensus_label_majority")
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
    y = 14000  # start from 0; we’ll place it just above x-axis
  )

# Step 3: Main plot with bars, vlines, and annotations
p_g_abs <- data_phase %>%
  filter(publication_year >= 1990
         # & publication_year <= 2022
         ) %>%
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

figA4 <- ggarrange(p_g_abs, p_g_rel, ncol = 1, labels = c("a", "b"), align = "v")
ggsave(figA4, file = "plots/figA4.pdf", height = 9, width = 10) 


################################################################################
# Cities per cluster with and without research
################################################################################

library(ggpattern)

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
ggsave(p_cities_per_cluster_n, file = "plots/p_cities_per_cluster_n.pdf", width = 5, height = 5)

################################################################################
# Number of studies per cluster
################################################################################

p_n_studies <- clust %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
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
  labs(x = "", y = "", subtitle = "Total number of studies")


# ################################################################################
# # Cluster cities & research metrics
# ################################################################################
# 
# cluster_cities <- ghsl %>%
#   mutate(geom = st_centroid(geom)) %>%
#   select(ID_UC_G0, geom) %>%
#   left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id"))
# 
# pub_counts <- cluster_cities %>%
#   as.data.frame() %>% 
#   group_by(ID_UC_G0) %>% 
#   summarise(research_count = n(), .groups = "drop")
# 
# city_topic_matrix <- pub_counts %>%
#   # pivot_wider(names_from = "group2", values_from = "research_count", values_fill = 0) %>%
#   right_join(ghsl %>% select(ID_UC_G0), by = "ID_UC_G0") %>%
#   mutate(across(where(is.numeric), ~ replace_na(., 0)),
#          ID_UC_G0 = as.character(ID_UC_G0))
# 
# research_metrics <- city_topic_matrix %>%
#   as.data.frame() %>% 
#   rowwise() %>%
#   mutate(
#     research_volume = sum(c_across(where(is.numeric))),
#     research_evenness = {
#       counts <- c_across(where(is.numeric))
#       if (sum(counts) == 0) 0 else (1 - ineq::Gini(counts))
#     }
#   ) %>%
#   ungroup() %>%
#   select(ID_UC_G0, research_volume, research_evenness)

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

sim_matrix <- proxy::simil(
  as.matrix(co_mat %>% select(all_of(co_vars)) %>% scale()),
  method = "cosine",
  by_rows = TRUE
)

similarity_sums <- rowSums(as.matrix(sim_matrix), na.rm = TRUE)

co_mat <- co_mat %>%
  mutate(similarity_scaled = min_max_scale(similarity_sums))

similarity_by_type <- co_mat %>%
  group_by(cluster_name) %>%
  summarise(similarity_scaled = mean(similarity_scaled), .groups = "drop") %>%
  mutate(cluster_name = factor(cluster_name, levels = rev(levels(cluster_name)))) %>%
  ggplot(aes(x = cluster_name, y = similarity_scaled, fill = cluster_name)) +
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
  left_join(n_studies_per_city, by = c("ID_UC_G0" = "city_id")) %>% 
  mutate(has_research = !is.na(n_studies))

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
    label = "Cities not covered by any studies",
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


################################################################################
# Hex grid and maps
################################################################################

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

ghsl_points <- ghsl %>%
  st_transform(proj_robin) %>%
  st_centroid() %>%
  select(ID_UC_G0, GC_UCN_MAI_2025) %>%
  # left_join(learn_pot, by = "ID_UC_G0") %>%
  st_centroid()

intersections <- st_intersection(
  ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>% st_transform(proj_robin),
  world_hex
) %>%
  mutate(inter_area = st_area(geom))

intersections_unique <- intersections %>%
  group_by(ID_UC_G0) %>%
  slice_max(inter_area, n = 1) %>%
  ungroup() %>%
  as.data.frame() %>%
  select(-geom)

hexa_data <- world_hex %>%
  left_join(intersections_unique, by = "hex_id") %>%
  left_join(co_mat, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  group_by(hex_id) %>%
  summarise(similarity_scaled = mean(similarity_scaled, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(similarity_scaled))

p_similarity_map <- hexa_data %>%
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_sf(aes(fill = similarity_scaled), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = "grey") +
  geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = "Similarity to all other cities",
    hjust = -0.1, vjust = 1.2,
    size = 3.5,
    fill = "white",
    label.size = 0.3
  ) + 
  theme_SM() +
  labs(y = "", x = "", fill = "Similarity to all other cities") +
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

fig2bc <- ggarrange(
  p_similarity_map,
  p_types_as_dots_without_research + theme(legend.position = "none"),
  labels = c("b", "c")
)

fig2abc <- ggarrange(
  p_types_as_dots_with_research,
  fig2bc,
  labels = c("a", ""),
  ncol = 1,
  heights = c(2.2, 1)
)

fig2def <- ggarrange(
  p_n_studies + theme(text = element_text(size = 9)),
  similarity_by_type + theme(text = element_text(size = 9)),
  p_cities_per_cluster_n + theme(text = element_text(size = 9)),
  align = "h", labels = c("d", "e", "f"),
  ncol = 1
)

fig2 <- ggarrange(fig2abc, fig2def, labels = c("", ""), ncol = 2, widths = c(3.2, 1))

ggsave(fig2, file = "plots/fig2.pdf", height = 6, width = 10)


# ################################################################################
# # example cities
# ################################################################################
# 
# library(scales) # for pretty breaks
# library(colorspace)
# library(dplyr)
# library(scales)   # for rescale
# library(ggplot2)
# library(ggrepel)
# 
# sampled_cities_clean <- read.csv("data/case_selection/selected_cites_paper.csv")
# 
# p_map_selected_cities <- ggplot() +
#   geom_sf(data = world %>% st_union(), fill = "grey90", color = NA, size = .3) +
#   geom_sf(
#     data = map_dot_dat %>% filter(ID_UC_G0 %in% sampled_cities_clean$city_id),
#     aes(col = cluster_name),
#     size = 1
#   ) +
#   # Add repelled labels
#   geom_text_repel(
#     data = map_dot_dat %>% filter(ID_UC_G0 %in% sampled_cities_clean$city_id),
#     aes(
#       geometry = geom,                 # sf geometry
#       label = GC_UCN_MAI_2025                     # column with city names
#     ),
#     stat = "sf_coordinates",              # convert sf points to x/y
#     size = 2.5,
#     max.overlaps = Inf,
#     min.segment.length = 0,
#     segment.color = "grey50"
#   ) +
#   scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
#   geom_sf(data = bb, col = "grey70", fill = "transparent", linewidth = .5) +
#   theme_SM() +
#   labs(
#     y = "", x = "", col = "City type",
#     title = ""
#   ) +
#   theme(
#     legend.position = c(.05, .3),
#     legend.direction = "vertical",
#     legend.justification = "left",
#     legend.title = element_text(),
#     legend.box = "vertical",
#     legend.spacing.y = unit(0.1, "cm"),
#     legend.background = element_blank(),
#     legend.box.background = element_rect(fill = "white", color = "grey", size = 0.5),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.length = unit(0, "cm"),
#     text = element_text(size = 8),
#     panel.spacing = unit(-0.15, "lines"),
#     panel.border = element_blank(),
#     plot.margin = margin(c(-1, 0, 0, 0), "cm")
#   )
# p_map_selected_cities

# 
# library(dplyr)
# library(ggplot2)
# library(ggforce)   # for facet_nested
# 
# # Function to create heatmap for selected regions
# plot_city_heatmap <- function(regions = c("North America", "Europe", "Asia"), reg_vars_wg2) {
#   
#   # Ensure region factor ordering
#   region_levels <- reg_vars_wg2
#   regions <- intersect(regions, region_levels)
#   
#   box_plot_add_covs_dat %>%
#     filter(GHS_urban_area_id %in% sampled_cities_clean$city_id) %>%
#     left_join(
#       ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025),
#       by = c("GHS_urban_area_id" = "ID_UC_G0")
#     ) %>%
#     left_join(
#       sampled_cities_clean %>% select(-X),
#       by = c("GHS_urban_area_id" = "city_id")
#     ) %>%
#     filter(region %in% regions) %>%
#     mutate(region = factor(region, levels = region_levels)) %>%
#     group_by(variable) %>%
#     mutate(
#       vmin = min(normalized_value, na.rm = TRUE),
#       vmax = max(normalized_value, na.rm = TRUE),
#       fill_scaled = case_when(
#         normalized_value < 1 ~ 0.5 * (normalized_value - vmin) / (1 - vmin),
#         normalized_value > 1 ~ 0.5 + 0.5 * (normalized_value - 1) / (vmax - 1),
#         TRUE ~ 0.5
#       )
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(GC_UCN_MAI_2025, variable, fill = fill_scaled)) +
#     geom_tile() +
#     geom_text(aes(label = round(normalized_value, 1)), size = 1.5) +
#     facet_nested(~ region + cluster_name, scales = "free", space = "free") +
#     scale_fill_gradientn(
#       colors = c("#2166ac", "#f7f7f7", "#97181e"),
#       values = c(0, 0.5, 1),
#       limits = c(0, 1),
#       breaks = c(0, 0.5, 1),
#       labels = c("Min", "1", "Max")
#     ) +
#     labs(fill = "Deviation\nfrom the mean") +
#     theme(
#       axis.title = element_blank(),
#       axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#       panel.grid = element_blank(),
#       strip.placement = "outside",
#       legend.position = c(-.1, -1),
#       legend.direction = "horizontal",
#       legend.title.position = "top",
#       axis.text.y.left = element_text(lineheight = 0.7, size = 9),
#       strip.text.y.left = element_text(angle = 90, size = 10, hjust = 0.5),
#       strip.text = element_text(colour = "white"),
#       strip.background = element_rect(fill = "grey35"),
#       panel.background = element_blank(),
#       axis.ticks = element_line(color = "grey90"),
#       axis.ticks.length = unit(0.15, "cm")
#     )
# }
# 
# 
# # Example usage:
# p_heatmap_1 <- plot_city_heatmap(regions = c( "North America", "Europe", "Asia"), reg_vars_wg2)
# p_heatmap_2 <- plot_city_heatmap(regions = c("South America", "Africa", "Australasia", "Small Islands"), reg_vars_wg2)
# 
# 
# combined_map <- ggdraw() +
#   draw_plot(p_map_selected_cities) +
#   draw_plot(p_heatmap_1, x = 0, y = 0.68, width = 1, height = 0.32) +
#   draw_plot(p_heatmap_2, x = 0, y = 0.32, width = 1, height = -0.32)  # x/y in 0–1 npc coords
# 
# ggsave(combined_map, file = "plots/p_examples_map_and_cov.pdf", height = 10, width = 10)
# 
# 
# ################################################################################
# # scalable climtate solutions based on keywords
# ################################################################################
# 
# clim_sol <- read_parquet("data/climate_solutions_typology/oa_sentence_solutions.parquet")
# 
# keywords_compact <- c("compact city",
#                       "walkability",
#                       "walkable neighborhood",
#                       "high density",
#                       "mixed use",
#                       "transit-oriented development",
#                       "TOD",
#                       "urban sprawl",
#                       "active transport",
#                       "15-minute city")
# 
# keywords_electrific <- c(
#   "electric vehicle",
#   "electric bus",
#   "EV adoption",
#   "e-mobility",
#   "low-carbon transport",
#   "mode shift",
#   "public transport electrification",
#   "shared mobility",
#   "zero-emission vehicle",
#   "transport decarbonization"
# )
# 
# keywords_build <- c(
#   "energy-efficient building",
#   "building retrofit",
#   "passive house",
#   "green building",
#   "low-carbon material",
#   "embodied carbon",
#   "net-zero building",
#   "building envelope",
#   "HVAC efficiency",
#   "zero-energy building",
#   "thermal insulation",
#   "LEED certified",
#   "deep retrofit",
#   "building renovation",
#   "energy retrofit",
#   "insulation upgrade"
# )
# 
# keywords_blue_green <- c(
#   "green infrastructure",
#   "green roof",
#   "green wall",
#   "urban forest",
#   "tree canopy",
#   "blue infrastructure",
#   "stormwater management",
#   "permeable surface",
#   "heat island mitigation",
#   "nature-based solution",
#   "NBS"
# )
# 
# 
# solution_topics <- ghsl %>%
#   mutate(geometry = st_centroid(geom)) %>%
#   
#   dplyr::select(ID_UC_G0, geom) %>%
#   left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
#   mutate(
#     contains_compact = str_detect(abstract, str_c(fixed(keywords_compact), collapse = "|")),
#     contains_electrific = str_detect(abstract, str_c(fixed(keywords_electrific), collapse = "|")),
#     contains_build = str_detect(abstract, str_c(fixed(keywords_build), collapse = "|")),
#     contains_blue_green = str_detect(abstract, str_c(fixed(keywords_blue_green), collapse = "|"))
#   )
# 
# solution_topics
# 
# # A) which types on which hexa
# clust_with_city_geom <- left_join(ghsl %>% dplyr::select(ID_UC_G0) %>% mutate(geom = st_centroid(geom)), clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
#   st_transform(4326) %>% 
#   st_make_valid() 
# 
# ipcc_cluster_presence <- ipcc_regions %>%
#   st_join(clust_with_city_geom) %>%
#   st_drop_geometry() %>%
#   group_by(Acronym, consensus_label_majority) %>%
#   summarise(cluster_present = any(!is.na(ID_UC_G0)), .groups = "drop") %>%
#   mutate(cluster_col = paste0("cluster_", consensus_label_majority)) %>%
#   dplyr::select(-consensus_label_majority) %>% 
#   pivot_wider(names_from = cluster_col, values_from = cluster_present, values_fill = FALSE) %>% 
#   pivot_longer(starts_with("cluster_"), names_to = "consensus_label_majority", values_to = "cluster_present") %>%
#   mutate(consensus_label_majority = as.numeric(gsub("cluster_", "", consensus_label_majority))) %>% 
#   filter(!is.na(consensus_label_majority))
# 
# # ipcc_regions_clusters <- left_join(ipcc_regions, ipcc_cluster_presence, by = c("Acronym"))
# 
# ipcc_regions_hexa_clusters <- ipcc_regions_hexa %>% 
#   left_join(ipcc_cluster_presence, by = c("label" = "Acronym"))
# 
# # B) which solutions on which hexa
# solution_topics <- st_transform(solution_topics, 4326)
# ipcc_regions_hexa <- st_transform(ipcc_regions_hexa, 4326)
# 
# # Step 1: Spatial join - assign each city point to an IPCC region
# solution_with_region <- solution_topics %>%
#   st_as_sf() %>%
#   st_make_valid() %>% 
#   st_join(ipcc_regions) 
# 
# # solution_with_region <- solution_with_region %>% 
# #   mutate(Acronym = ifelse(Acronym %in% c("EPO", "NPO"), "PAC", Acronym)) 
# 
# # Step 2: Reshape from wide to long for the solution types
# solution_summary <- solution_with_region %>%
#   dplyr::select(ID_UC_G0, Name, consensus_label_majority, Acronym,
#          contains_compact, contains_electrific, contains_build, contains_blue_green) %>%
#   pivot_longer(
#     cols = starts_with("contains_"),
#     names_to = "solution_type",
#     values_to = "has_solution"
#   ) %>%
#   as.data.frame() %>% 
#   group_by(consensus_label_majority, Acronym, solution_type) %>% 
#   summarise(n_studies = sum(has_solution, na.rm = T), .groups = "drop")
# 
# # Step 4: Join counts back to IPCC region polygons
# ipcc_map_data <- ipcc_regions_hexa %>%
#   left_join(solution_summary, by = c("label" = "Acronym"))  # 'Name' is region name
# 
# # Optional: clean up solution_type labels
# ipcc_map_data <- ipcc_map_data %>%
#   mutate(
#     solution_type = recode(solution_type,
#                            contains_compact = "Compact\nCity",
#                            contains_electrific = "Transport\nElectrification",
#                            contains_build = "Green\nBuildings",
#                            contains_blue_green = "Blue-Green\nInfrastructure"
#     )
#   ) %>% 
#   filter(!is.na(solution_type) & !is.na(consensus_label_majority))
# 
# labels_vec <- setNames(str_replace_all(
#   cluster_names$cluster_name,
#   paste0("((?:\\S+\\s+){2})"),
#   "\\1\n"
# ), as.character(cluster_names$consensus_label_majority))
# 
# # Step 5: Plot
# library(ggnewscale)
# library(ggplot2)
# library(ggnewscale)
# library(viridis)
# library(ggstar)
# 
# ########### legend 
# # Legend items
# library(stringr)
# 
# # Legend items
# legend_items <- data.frame(
#   category = rev(c("No Cities\nof that type",
#                "No Studies\nof that type",
#                "2","8","32","128","512")),
#   fill = rev(c("white", "grey90", viridis(5, option = "C")))
# )
# 
# # Base vertical positions
# legend_items$y <- rev(seq_len(nrow(legend_items)))
# legend_items$x <- 2
# 
# # Manual vertical legend
# legend <- ggplot(legend_items, aes(x = x, y = y, fill = fill)) +
#   ggstar::geom_star(
#     aes(fill = fill),
#     starshape = "hexagon",
#     size = 3,
#     color = "grey"
#   ) +
#   geom_text(aes(x = x + 1.5, y = y, label = category),
#             hjust = 0, size = 2.5, lineheight = .8) +
#   scale_fill_identity() +
#   coord_fixed() +
#   ggtitle("Number\nof studies") +
#   xlim(1, 6) +
#   theme_void() +
#   coord_cartesian(clip = "off") +
#   theme(
#     plot.title = element_text(size = 9, margin = margin(l = 5, t = 10)),
#     plot.margin = margin(0,2,10,0),
#     plot.background = element_rect(color = "black", fill = "white", size = 0.4) 
#   )
# 
# legend
# 
# 
# 
# # --- 1. MAP (streamlined, no dummy legend hacks) ---
# p_solutions_by_ipcc_region_and_cluster_map <- ggplot() +
#   # First layer: IPCC regions with cluster presence as binary fill (white vs grey)
#   geom_sf(data = ipcc_regions_hexa_clusters, aes(geometry = geom), fill = "white", color = "grey") +
#   geom_sf(data = ipcc_regions_hexa_clusters %>% filter(cluster_present),
#           aes(geometry = geom), fill = "grey90", color = NA) +
#   geom_sf(data = ipcc_map_data,
#           aes(geometry = geom, fill = log2(n_studies)), color = "grey") +
#   scale_fill_viridis_c(
#     option = "C",
#     na.value = "grey90",
#     name = "Cities with\nResearch",
#     breaks = log2(2^(0:8)),
#     labels = c(2^(0:8))
#   ) +
#   facet_grid(solution_type~consensus_label_majority,  
#              labeller = labeller(consensus_label_majority = as_labeller(labels_vec))
#   ) +
#   labs(
#     # title = "Number of Studies Documenting Each Urban Solution by IPCC Region and Cluster",
#     # subtitle = "Regions with no studies -> grey; regions with no cities in that cluster -> white",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_SM() +
#   theme(plot.margin = margin(0,0,0,0, "cm"),
#         strip.position="left") +
#   theme(
#     panel.border = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     legend.position = "none", 
#     strip.text.y = element_text(angle = 0),
#     strip.text = element_text(size = 9)
#   )
# 
# p_solutions_by_ipcc_region_and_cluster_map
# ggsave(p_solutions_by_ipcc_region_and_cluster_map, file = "plots/p_solutions_by_ipcc_region_and_cluster_map.pdf", width = 10, height = 7)
# 
# p_solutions_by_ipcc_region_and_cluster_bar <- solution_summary %>% 
#   group_by(consensus_label_majority, solution_type) %>% 
#   summarise(Studies = sum(n_studies), .groups = "drop") %>% 
#   mutate(
#     solution_type = recode(solution_type,
#                            contains_compact = "Compact City",
#                            contains_electrific = "Transport Electrification",
#                            contains_build = "Green Buildings",
#                            contains_blue_green = "Blue-Green"
#     )
#   ) %>% 
#   group_by(consensus_label_majority) %>% 
#   mutate(
#     Percentage = Studies / sum(Studies) * 100
#   ) %>% 
#   ungroup() %>% 
#   pivot_longer(
#     cols = c("Studies", "Percentage"), 
#     names_to = "value_type", 
#     values_to = "value"
#   ) %>% 
#   mutate(value = round(value, 0)) %>% 
#   ggplot(aes(x = solution_type, y = value)) +
#   geom_bar(stat = "identity", position = "stack", col ="black", fill = "lightblue", alpha = .1, width = .5, size = .1) +
#   geom_text(aes(x = solution_type, y = value, 
#                 label = ifelse(value_type ==  "Percentage", paste0(value, "%"), value)),
#             vjust = -1, size = 2) +
#   scale_y_continuous(expand = expansion(mult = c(0, .3))) +
#   facet_grid(value_type ~ consensus_label_majority, scales = "free_y", switch = "x") +
#   labs(x = "", y = NULL) +
#   theme_SM() +
#   theme(legend.position = "none", 
#         strip.text.x = element_blank(),
#         element_text(size = 10),
#         plot.margin = margin(.6, 3, -1, .2, unit = "cm"),
#   ) 
# p_solutions_by_ipcc_region_and_cluster_bar
# ggsave(p_solutions_by_ipcc_region_and_cluster_bar, file = "plots/p_solutions_bar.pdf", width = 10, height = 8)
# 
# p_solutions_by_ipcc_region_and_cluster <- ggarrange(p_solutions_by_ipcc_region_and_cluster_map, p_solutions_by_ipcc_region_and_cluster_bar, 
#                                                     labels = c("a", "b"), ncol = 1, 
#                                                     heights = c(6,3.5),
#                                                     align = "h"
# )
# p_solutions_by_ipcc_region_and_cluster_leg <- ggdraw() +
#   draw_plot(p_solutions_by_ipcc_region_and_cluster) +
#   draw_plot(legend, x = 0.89, y = 0.11, width = 0.1, height = 0.25) 
# 
# ggsave(p_solutions_by_ipcc_region_and_cluster_leg, file = "plots/p_solutions_by_ipcc_region_and_cluster_leg.pdf", width = 10, height = 7.5)
