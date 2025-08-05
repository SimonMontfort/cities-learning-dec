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

################################################################################
# load data
################################################################################

clust <- read.csv("data/clustering_results/dec_clusters_k4.csv")
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
ghsl_clean <- read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation.parquet")

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
  filter(!is.na(city_intersection_id) & !is.na(city_word_match_id)) %>%
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id)

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

proj_robin <- "+proj=robin"
co_vars <- c("GHS_population", "GHS_population_growth", "GHS_population_density", "GHS_population_density_growth",
             "GHS_GDP_PPP", "GHS_GDP_PPP_growth", 
             "CL_B12_CUR_2010",
             "hdd", "cdd")
co_vars_formatted <- c("Population", "Population growth", "Population density", "Population density growth", 
                       "GDP PPP", "GDP PPP growth",  
                       "Precipitation",
                       "Heating degree days", "Cooling degree days")
reg_vars <- c("Africa", "Asia", "Europe", "North.America", "Oceania", "South.America")

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Small, hot, dense and expanding",
    "Affluent, low-density cities",
    "Fast-developing cities",
    "Mega and large cities"
  )) %>% 
  mutate(cluster_name = factor(cluster_name, levels = cluster_name)) 


ghsl <- st_transform(ghsl, proj_robin)
world <- st_transform(world, proj_robin)
bb <- st_transform(bb, proj_robin)

emmissions <- read.csv("data/emissions/balance_sheet.csv")



################################################################################
# functions needed throughout the script
################################################################################

# Load and register a modern font (e.g., Helvetica Neue)
showtext_auto()  # Automatically use showtext for fonts

# Check available fonts
remotes::install_github("kjhealy/myriad")
myriad::import_myriad(font_family = "Myriad Pro", silent = F)
theme_SM <- function(){
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          text = element_text(size = 12, family = "Myriad Pro"),
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
    "CL_B12_CUR_2010" = "Precipitation",
    "hdd" = "Heating degree days",
    "cdd" = "Cooling degree days"
  )
  
  column <- rlang::ensym(column)
  
  df %>%
    mutate(!!column := recode(!!column, !!!rename_map))
}


################################################################################
# recode variables
################################################################################

clust <- clust %>% 
  left_join(ghsl_clean, by= "GHS_urban_area_id") %>% 
  left_join(ghsl, by= c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  mutate(GHS_population = GHS_population/1000000,
         GHS_population_density = GHS_population_density/1000000,
         GHS_GDP_PPP = GHS_GDP_PPP/1000,
         hdd = hdd,
         cdd = cdd) %>% 
  select(GHS_urban_area_id, consensus_label_majority, 
         co_vars, continent, similarity
         ) %>% 
  mutate(
    North.America = as.integer(continent == "North America"),
    South.America = as.integer(continent == "South America"),
    Europe = as.integer(continent == "Europe"),
    Africa = as.integer(continent == "Africa"),
    Asia = as.integer(continent == "Asia"),
    Oceania = as.integer(continent %in% c("Oceania", "Australia"))
  ) %>% 
  left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)) %>% 
  mutate(similarity = (similarity - min(similarity, na.rm = TRUE)) /
           (max(similarity, na.rm = TRUE) - min(similarity, na.rm = TRUE)),
         similarity_n_studies_per_city=similarity*n_studies
         ) %>% 
  group_by(consensus_label_majority, continent) %>% 
  arrange(consensus_label_majority, continent, -similarity_n_studies_per_city) %>% 
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
  # mutate(Acronym = ifelse(Type == "Ocean", "PAC", Acronym))
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
  select(consensus_label_majority, co_vars, reg_vars, GHS_urban_area_id) 

p_cov_desc <- desc_dat %>% 
  pivot_longer(co_vars, names_to = "co_var", values_to = "co_var_value") %>% 
  pivot_longer(reg_vars, names_to = "reg_vars", values_to = "reg_vars_value") %>% 
  filter(reg_vars_value == 1) %>% 
  ggplot(aes(x = co_var_value, fill = reg_vars)) + 
  geom_histogram(alpha = .5) + 
  facet_wrap(~co_var, scales = "free") +
  scale_fill_npg() +
  theme_SM() +
  theme(legend.position = "bottom")
ggsave(p_cov_desc, file = "plots/p_cov_desc.pdf", width = 10, height = 6)
  
any(rowSums(desc_dat[, c("Africa", "Asia", "Europe", "North.America", "Oceania", "South.America")])) >1

################################################################################
# summary stats
################################################################################

# Calculate stats grouped by consensus_label_majority
result <- desc_dat %>%
  select(consensus_label_majority, co_vars) %>%
  pivot_longer(-consensus_label_majority, names_to = "variable", values_to = "values") %>%
  group_by(consensus_label_majority, variable) %>%
  summarise(mean = mean(values),
            p25 = quantile(values, 0.25, na.rm = TRUE),
            p75 = quantile(values, 0.75, na.rm = TRUE)) %>% 
  rename_co_vars("variable") %>% 
  mutate(unit = "",
         unit = ifelse(variable == "Precipitation", "yearly average in mm", unit),
         unit = ifelse(variable == "GDP PPP", "GPD p.c. purchased power parity in 1,000 $", unit),
         unit = ifelse(variable == "GDP PPP growth", "yearly % GDP p.c. growth (CAGR: 2000-2025)", unit),
         unit = ifelse(variable == "Population", "million inhabitants", unit),
         unit = ifelse(variable == "Population growth", "yearly % population growth (CAGR: 2000-2025)", unit),
         unit = ifelse(variable == "Population density", "million inhabitants per km2 built-up area", unit),
         unit = ifelse(variable == "Population density growth", "yearly % population density growth (CAGR: 2000-2025)", unit),
         unit = ifelse(variable == "Cooling degree days", "Cumulative daily average temperature deviation above 18 °C", unit),
         unit = ifelse(variable == "Heating degree days", "Cumulative daily average temperature deviation below 18 °C", unit)) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  ungroup() %>% 
  select(cluster_name, variable, unit, mean, p25, p75) %>% 
  arrange(variable)

result

library(knitr)
library(xtable)

print(xtable(result, 
             caption = "Summary Statistics by Cluster", 
             align = c("l", "l", "l", "l", "r", "r", "r")), 
      include.rownames = FALSE, 
      tabular.environment = "longtable",
      floating = FALSE)



################################################################################
# map and attributes 2
################################################################################

desc_dat

summary(desc_dat$GHS_population_density_growth)
summary(desc_dat$GHS_population_density)
summary(desc_dat$CL_B12_CUR_2010)
summary(desc_dat$hdd)
summary(desc_dat$cdd)

desc_dat_long <- desc_dat %>%
  mutate(
    GHS_population = case_when(
      GHS_population <= .3 ~ "Small\n(<0.3m)",
      GHS_population > .3 & GHS_population <= 1 ~ "Medium\n(0.3-1m)",
      GHS_population > 1 & GHS_population <= 10 ~ "Large\n(1-10m)",
      GHS_population > 10 ~ "Mega\n(>10m)"
    ),
    GHS_population_density = case_when(
      GHS_population_density <= 0.01 ~ "Low\n(<0.01m/km²)",
      GHS_population_density > 0.01 & GHS_population_density <= .03 ~ "Lower-middle\n(0.01-0.03m/km²)",
      GHS_population_density > .03 & GHS_population_density <= .05 ~ "Upper-middle\n(0.03-0.05m/km²)",
      GHS_population_density > .05 ~ "High\n(>0.05m/km²)"
    ),
    GHS_population_growth = case_when(
      GHS_population_growth <= 0 ~ "Decline\n(<0% p.a.)",
      GHS_population_growth > 0 & GHS_population_growth <= 1 ~ "Slow\n(0-1% p.a.)",
      GHS_population_growth > 1 & GHS_population_growth <= 2 ~ "Moderate\n(1-2% p.a.)",
      GHS_population_growth > 2 ~ "Fast\n(>2% p.a.)"
    ),
    GHS_population_density_growth = case_when(
      GHS_population_density_growth <= -1 ~ "Extreme sprawl\n(<-1% p.a.)",
      GHS_population_density_growth > -1 & GHS_population_density_growth <= -.5 ~ "Much sprawl\n(-1 to -0.5% p.a.)",
      GHS_population_density_growth > -.5 & GHS_population_density_growth <= 0 ~ "Some sprawl\n(-0.5 to 0% p.a.)",
      GHS_population_density_growth > 0 ~ "Compactification\n(>0% p.a.)"
    ),
    GHS_GDP_PPP = case_when(
      GHS_GDP_PPP <= 2 ~ "Low\n(<2k$ p.c.)",
      GHS_GDP_PPP > 2 & GHS_GDP_PPP <= 6 ~ "Lower-middle\n(2-6k$ p.c.)",
      GHS_GDP_PPP > 6 & GHS_GDP_PPP <= 15 ~ "Upper-middle\n(6-15k$ p.c.)",
      GHS_GDP_PPP > 15 ~ "High\n(>15k$ p.c.)"
    ),
    GHS_GDP_PPP_growth = case_when(
      GHS_GDP_PPP_growth <= 0 ~ "Decline\n(<0% p.a.) ",
      GHS_GDP_PPP_growth > 0 & GHS_GDP_PPP_growth <= 2.5 ~ "Slow\n(0-2.5% p.a.)",
      GHS_GDP_PPP_growth > 2.5 & GHS_GDP_PPP_growth <= 6.7 ~ "Moderate\n(2.5-6.7% p.a.)",
      GHS_GDP_PPP_growth > 6.7 ~ "Fast\n(>6.7% p.a.)"
    ),
    CL_B12_CUR_2010 = case_when(
      CL_B12_CUR_2010 <= 250 ~ "Arid\n(<250mm p.a.)",
      CL_B12_CUR_2010 > 250 & CL_B12_CUR_2010 <= 750 ~ "Semi-arid\n(250-750mm p.a.)",
      CL_B12_CUR_2010 > 750 & CL_B12_CUR_2010 <= 1500 ~ "Temperate\n(750-1500mm p.a.)",
      CL_B12_CUR_2010 > 1500 ~ "Humid\n(>1500mm p.a.)"
    ),
    hdd = case_when(
      hdd <= 30 ~ "Very mild\n(<30°-days)",
      hdd > 30 & hdd <= 50 ~ "Mild\n(30-50°-days)",
      hdd > 50 & hdd <= 200 ~ "Cold\n(50-200°-days)",
      hdd > 200 ~ "Very cold\n(>200°-days)"
    ),
    cdd = case_when(
      cdd <= 80 ~ "Very mild\n(<80°-days)",
      cdd > 80 & cdd <= 180 ~ "Mild\n(80-180°-days)",
      cdd > 180 & cdd <= 300 ~ "Hot\n(180-300°-days)",
      cdd > 300 ~ "Very hot\n(>300°-days)"
    )
  ) %>%
  pivot_longer(all_of(co_vars), names_to = "co_vars", values_to = "co_var_value") %>%
  mutate(co_var_value = factor(co_var_value, levels = c(
    # Population
    "Small\n(<0.3m)", "Medium\n(0.3-1m)", "Large\n(1-10m)", "Mega\n(>10m)",
    # Density
    "Low\n(<0.01m/km²)", "Lower-middle\n(0.01-0.03m/km²)", "Upper-middle\n(0.03-0.05m/km²)", "High\n(>0.05m/km²)",
    # Growth
    "Decline\n(<0% p.a.)", "Slow\n(0-1% p.a.)", "Moderate\n(1-2% p.a.)", "Fast\n(>2% p.a.)",
    # Density Growth
    "Extreme sprawl\n(<-1% p.a.)", "Much sprawl\n(-1 to -0.5% p.a.)", "Some sprawl\n(-0.5 to 0% p.a.)", "Compactification\n(>0% p.a.)",
    # GDP PPP
    "Low\n(<2k$ p.c.)", "Lower-middle\n(2-6k$ p.c.)", "Upper-middle\n(6-15k$ p.c.)", "High\n(>15k$ p.c.)",
    # GDP Growth
    "Decline\n(<0% p.a.) ", "Slow\n(0-2.5% p.a.)", "Moderate\n(2.5-6.7% p.a.)", "Fast\n(>6.7% p.a.)",
    # Climate
    "Arid\n(<250mm p.a.)", "Semi-arid\n(250-750mm p.a.)", "Temperate\n(750-1500mm p.a.)", "Humid\n(>1500mm p.a.)",
    # HDD
    "Very mild\n(<30°-days)", "Mild\n(30-50°-days)", "Cold\n(50-200°-days)", "Very cold\n(>200°-days)",
    # CDD
    "Very mild\n(<80°-days)", "Mild\n(80-180°-days)", "Hot\n(180-300°-days)", "Very hot\n(>300°-days)"
  )))


## fill in empty factor values for combinations where the count is 0 for a given tpye, 
# e.g a type does not have any mega cities so that these results are reflected in the matrix
completed <- list()
for (var in unique(desc_dat_long$co_vars)) {
  completed[[var]] <- desc_dat_long %>% 
    filter(co_vars == {{var}}) %>% 
    rename_co_vars(co_vars) %>%
    mutate(
      co_var_value = droplevels(co_var_value),
      consensus_label_majority = as.factor(consensus_label_majority)  # important for complete()
    ) %>%
    group_by(co_vars, co_var_value, consensus_label_majority) %>%
    count(name = "n") %>%
    ungroup() %>%
    
    # complete long df: if some bins do not occur, they need to be zero
    complete(co_vars, consensus_label_majority, co_var_value, fill = list(n = 0)) %>%
    
    # share of the covariate bins across all values,
    # e.g. share of all small cities in cluster 1
    group_by(co_vars, co_var_value) %>%
    mutate(
      share_bin = n / sum(n),
      fill_val_bin = n / sum(n),
      max_value_bin = n == max(n)
    ) %>% 
    
    # share of the covariate bins across the cluster,
    # e.g. share of small cities in cluster 1
    group_by(co_vars, consensus_label_majority) %>%
    mutate(
      share_clus = n / sum(n),
      fill_val_clus = n / sum(n),
      max_value_clus = n == max(n)
    ) %>% 
    ungroup()
}

desc_dat_long <- do.call(rbind, completed) %>% 
  mutate(co_vars = factor(gsub(" ", "\n", co_vars), levels = !!gsub(" ", "\n", co_vars_formatted)),)

desc_geo <- ghsl %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025) %>%
  left_join(clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) 

# Compute centroids for each region
desc_geo$centroid <- st_centroid(desc_geo$geom)


box_plot_list <- list()
min_cl <- min(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
max_cl <- max(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
for (cluster in min_cl:max_cl) {
  
  # Plot the maps with centroids
  p_types_map <- desc_geo %>% 
    filter(consensus_label_majority == cluster) %>% 
    arrange(representative_city) %>% 
    ggplot() +
    geom_sf(data = world, fill = "grey90", color = "white") +  # World map with light gray color
    geom_sf(aes(geometry = centroid, col = representative_city), size = .5, alpha = .7) +  # Plot centroids as red points
    ggrepel::geom_label_repel(
      aes(label = ifelse(representative_city == T, GC_UCN_MAI_2025, NA), geometry = centroid),
      stat = "sf_coordinates", alpha=.5, size = 2, 
    ) +
    scale_color_manual(values = c("#ffb84d", "black")) +
    geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
    coord_sf(crs = proj_robin) + 
    annotate(
      "label",
      x = -Inf, y = Inf,
      label = cluster_names$cluster_name[cluster_names$consensus_label_majority == cluster],
      hjust = 1, vjust = 1.2,
      size = 3,
      fill = "white",
      label.size = 0.3
    ) + 
    theme_SM() +
    theme(
      panel.border = element_rect(color = NA),
      legend.position = "none",
    ) +
    labs(col = "Cities", x = "", y = "")
  
  p_heat <- list()
  for (co_var in unique(desc_dat_long$co_vars)){
    p_heat[[co_var]] <- desc_dat_long %>%
      filter(consensus_label_majority == cluster,
             co_vars == co_var) %>%
      pivot_longer(
        cols = c(
          # share_bin, share_clus, fill_val_bin, 
                 fill_val_clus, max_value_bin, max_value_clus
                 ),
        names_to = c(".value", "type"),  # .value keeps share/fill_val/max_value as columns
        names_sep = "_(?=[^_]+$)",
      ) %>%
      ggplot(aes(x = type, y = co_var_value, fill = fill_val)) +
      geom_tile(width=.95) +
      geom_tile(width=.95, data = . %>% filter(max_value), col = "black", size = .2) +
      # facet_grid(co_vars~co_var_value, scales = "free") +
      facet_wrap(co_var_value~.,
                   ncol = 1,
                   scales = "free",
                   # axes = "all"
                 ) +
      scale_fill_gradient2(low="white", mid="#ffc266", high="#963d03", 
                           limits = c(0, 1), oob = scales::squish) + 
      geom_text(aes(label = paste0(#co_var_value, "\n", 
        round(share, 2)*100, "%"), 
        col = max_value), 
        size = 1.6, lineheight = .8) +
      scale_color_manual(values = c("black", "white")) +
      theme_SM() + labs(y = "", x = "", title = co_var) +
      theme(
        title = element_text(hjust = .5, size = 5, vjust = -1),
        plot.title = element_text(hjust = .5, size = 5, vjust = 0),
        legend.position = "none", 
        strip.text = element_text(size = 5,
                                  margin = margin(0, 0, 0, 0, "pt")),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.5, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,-2,-1,-2), "cm"),
      )
    
    p_cov <- left_join(coastal %>% 
                         select(cluster_name, share) %>% 
                         rename("Share of coastal cities" = share), 
                       gender %>% 
                         select(cluster_name, consensus_label_majority, mean) %>% 
                         rename("Mean female gender index" = mean), 
                       by = "cluster_name") %>% 
      left_join(infra %>% 
                  select(cluster_name, mean) %>% 
                  rename("Mean critical infrastructure index" = mean), by = "cluster_name") %>% 
      left_join(hdi %>% 
                  select(cluster_name, mean) %>% 
                  rename("Mean human Development Index" = mean), by = "cluster_name") %>% 
      left_join(emm %>%
                  select(cluster_name, share) %>%
                  rename("Share of urban CO2 Emissions" = share), by = "cluster_name") %>%
      filter(consensus_label_majority == cluster) %>% 
      ungroup() %>% 
      pivot_longer(-c(cluster_name, consensus_label_majority)) %>% 
      # mutate(name = ifelse(name == "coastal_share"))
      ggplot(aes(x = cluster_name, y = value)) +
      geom_col(col = "black", fill = "lightblue") +
      # geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
      #               width = 0.2, color = "black") +
      coord_flip() +
      facet_wrap(~name, ncol = 1) + 
      geom_text(aes(label = paste0(round(value, 2))),
                size = 2.3, lineheight = .8, col = "black", hjust = -0.2) +
      theme_SM() +
      ylim(c(0,1)) + 
      labs(x = "") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), 
            text = element_text(size = 7),
            panel.spacing = unit(0.1,'lines'),
            axis.text.x = element_text(size = 5),
            strip.text = element_text(size = 5,
                                      margin = margin(0, 0, 0, 0, "pt")),
            axis.ticks.length = unit(.1, "cm"))
    
  }
  
  p_heat_combined <- plot_grid(plotlist = p_heat, ncol = length(p_heat), align = "h")
  
  plot.with.inset <-
    ggdraw() +
    draw_plot(p_types_map, y = .15) +
    draw_plot(p_heat_combined, x = 0, y = 0, width = 1, height = .5) + 
    draw_plot(p_cov, x = 0, y = .49, width = .2, height = .43) + 
    theme(plot.margin = margin(c(15,5,0,5), "cm"))
  plot.with.inset
  
  box_plot_list[[cluster+1]] <- plot.with.inset
}

multi_panel_plot <- plot_grid(plotlist = box_plot_list, ncol = 2, labels = "auto", align = "v")
ggsave(multi_panel_plot, filename = paste0("plots/multi_panel_plot.pdf"), width = 10, height = 9)

pop_by_type <- ghsl %>%
  as.data.frame() %>%
  left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority, GHS_population), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  group_by(continent, consensus_label_majority) %>%
  reframe(pop = sum(GHS_population)) %>%
  mutate(pop_share = pop/sum(pop)) %>%
  group_by(consensus_label_majority) %>%
  mutate(pop_share_norm = pop_share/mean(pop_share))

box_plot_list[[5]] <- clust %>%
  mutate(continent = ifelse(continent %in% c("Oceania", "Australia"), "Oceania", continent),
         continent = factor(continent, levels = c("North America", "South America", "Europe", "Africa", "Asia", "Oceania"))) %>% 
  group_by(consensus_label_majority, continent) %>%
  reframe(number_of_studies = sum(n_studies)) %>%
  group_by(continent) %>%
  mutate(research_share = number_of_studies/sum(number_of_studies),
         research_share_norm = research_share/mean(research_share)) %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
  left_join(pop_by_type, by = c("consensus_label_majority", "continent")) %>%
  mutate(research_share_to_pop_share = research_share_norm/pop_share_norm) %>%
  ggplot(aes(continent, cluster_name, fill = log2(research_share_to_pop_share))) +
  geom_tile(height = .95, width = .95) +
  scale_fill_gradient2(
    low = "darkred", mid = "grey", high = "darkslateblue", midpoint = 1,
  ) +
  geom_text(aes(label = paste0(number_of_studies, "\n", "(", round(research_share_to_pop_share, 2), ")")),
            size = 2.3, lineheight = .8, col = "white") +
  scale_color_manual(values = c("black", "white")) +
  theme_SM() +
  labs(y = "", x = "", fill = "Studies") +
  theme(legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_text(),
        axis.text = element_text(size = 7),
        legend.text = element_text(angle = 45, hjust = 1))

box_plot_list[[5]]
ggsave(box_plot_list[[5]], file = "plots/heat.pdf", width = 5, height = 4)

##################################################################
# exp post characterisation
##################################################################

###########
# emissions
###########

# Compute median emissions per continent × cluster_name
emmissions_dat <- emmissions %>%
  as.data.frame() %>% 
  left_join(
    clust %>% select(consensus_label_majority, continent, GHS_urban_area_id, GHS_population),
    by = c("ID_UC_G0" = "GHS_urban_area_id")
  ) %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
  left_join(ghsl %>% select(GC_POP_TOT_2025, ID_UC_G0), by = c("ID_UC_G0")) %>% 
  mutate(
    odiac_norm = ODIAC / GC_POP_TOT_2025,
    continent = ifelse(continent %in% c("Oceania", "Australia"), "Oceania", continent),
    continent = factor(continent, levels = c("North America", "South America", "Europe", "Africa", "Asia", "Oceania"))
  )

emmissions_box_dat <- emmissions_dat %>% 
  filter(Year == 2022) 

# Calculate medians for coloring
median_data <- emmissions_box_dat %>%
  group_by(continent, cluster_name) %>%
  summarize(median_odiac = median(odiac_norm, na.rm = TRUE), .groups = "drop")

# Join medians back
emmissions_box_dat <- emmissions_box_dat %>%
  left_join(median_data, by = c("continent", "cluster_name")) %>% 
  group_by(continent, cluster_name) %>%
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

box_plot_list[[6]] <- ggplot(emmissions_box_dat, aes(x = cluster_name, y = odiac_norm, fill = median_odiac)) +
  geom_hline(yintercept = median(median_data$median_odiac, na.rm = TRUE), lty = 3, col = "grey50") +
  
  # Half violin on the left
  geom_violinhalf(outliers = FALSE, trim = TRUE, 
              side = "l",
              alpha = 0.6, 
              lwd = 0.2, 
              flip = TRUE
              ) +
  
  # Boxplot on the right
  geom_boxplot(outliers = FALSE, outlier.size = 0.5, alpha = 0.8, width = 0.3, na.rm = TRUE, lwd = 0.2, position = position_nudge(x = 0.15)) +
  
  # scale_y_log10(labels = function(x) {
  #   sapply(x, function(val) {
  #     if (is.na(val)) {
  #       return(NA)
  #     } else if (val >= 1) {
  #       format(val, big.mark = ",", scientific = FALSE, trim = TRUE)
  #     } else {
  #       format(val, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 3)
  #     }
  #   })
# }, n.breaks = 8) +

# annotation_logticks(sides = "l",
#                     short = unit(.5, "mm"),
#                     mid = unit(1, "mm"),
#                     long = unit(2, "mm"),
#                     color = "grey50") +
scale_fill_gradient2(
  low = "#a0c4ff", mid = "white", high = "#FFADAD",
  midpoint = median(median_data$median_odiac, na.rm = TRUE),
  name = "Median Emissions"
) +
  labs(
    x = "",
    y = "Emissions p.c. (t CO₂)"
  ) +
  facet_wrap(~continent, nrow = 1) +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(box_plot_list[[6]], file = "plots/box.pdf", width = 10, height = 5)



calc_avg_growth <- function(x) {
  # Remove leading NAs only
  if(all(is.na(x))) return(NA)  # Entire series is NA
  x <- x[seq(from = which(!is.na(x))[1], to = length(x))]
  
  # If less than 2 valid values after trimming, return NA
  if(sum(!is.na(x)) < 2) return(NA)
  
  # Compute growth using available consecutive values
  diffs <- diff(log(x), lag = 1)
  mean(diffs, na.rm = TRUE)
}

# Compute growth rates per group for ODIAC and EDGAR
growth_rates <- emmissions_dat %>%
  group_by(ID_UC_G0, GC_CNT_GAD_2025, GC_UCN_MAI_2025) %>%
  filter(Year >=2017) %>% 
  arrange(Year, .by_group = T) %>% 
  summarise(
    ODIAC_avg_growth = calc_avg_growth(ODIAC),
    EDGAR_avg_growth = calc_avg_growth(EDGAR),
    .groups = 'drop'
  ) %>%
  mutate(
    ODIAC_avg_growth_pct = (exp(ODIAC_avg_growth) - 1) * 100,
    EDGAR_avg_growth_pct = (exp(EDGAR_avg_growth) - 1) * 100
  )

growth_rates %>% 
  left_join(clust %>% select(consensus_label_majority, GHS_urban_area_id), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
  group_by(consensus_label_majority) %>% 
  arrange(ODIAC_avg_growth_pct, .by_group = TRUE) %>% 
  slice(1:10) %>% 
  as.data.frame()

##################################################################
# Coastal cities
##################################################################
ghsl_full <- read_sf("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.gpkg")
# 1. Ensure both layers are in EPSG:4326 for geodesic distances
ghsl_full <- st_transform(ghsl_full, 4326)
world <- st_transform(world, 4326)

# 2. Coastline as single polygon
coast <- world %>%
  st_union() %>%
  st_boundary() %>%  
  st_as_sf()

# 3. Compute centroids
ghsl_full_cent <- ghsl_full %>%
  st_make_valid() %>% 
  mutate(geom = st_centroid(geom))

# 4. Compute geodesic distances (meters)
ghsl_full_cent <- ghsl_full_cent %>%
  mutate(dist_to_coast = st_distance(geom, coast)) 

# 5. Convert to km
ghsl_full_cent$dist_to_coast_km <- as.numeric(ghsl_full_cent$dist_to_coast) / 1000

# 6. Flag coastal cities (<= 50 km)
coastal_cities <- ghsl_full_cent %>%
  as.data.frame() %>%
  mutate(is_coastal = dist_to_coast_km <= 50) %>%
  select(is_coastal, dist_to_coast_km, ID_UC_G0)

table(coastal_cities$is_coastal)


coastal <- clust %>% 
  left_join(coastal_cities, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name, is_coastal) %>% 
  count() %>% 
  group_by(cluster_name) %>% 
  mutate(total = sum(n),
         share = n / total) %>% 
  filter(is_coastal) %>% 
  mutate(se = sqrt(share * (1 - share) / total))

coastal %>% 
  ggplot(aes(x = cluster_name, y = share)) + 
  geom_bar(stat = "identity", position = "stack", col = "black", fill = "lightblue") +
  geom_text(aes(label = paste0(round(share*100), "%")),
            size = 2.3, lineheight = .8, col = "black", vjust = -0.2) +
  theme_SM()


ghsl_full_infra <- readxl::read_xlsx("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "INFRASTRUCTURES")
 
infra <- clust %>% 
  left_join(ghsl_full_infra %>% select(IN_CIS_ALL_2020, ID_UC_G0), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name) %>%
  summarise(sd = sd(IN_CIS_ALL_2020),
            mean = mean(IN_CIS_ALL_2020)) 

infra %>% 
  ggplot(aes(x = cluster_name, y = mean)) + 
  geom_bar(stat = "identity", position = "stack", col = "black", fill = "lightblue") +
  geom_text(aes(label = paste0(round(mean*100), "%")),
            size = 2.3, lineheight = .8, col = "black", vjust = -0.2) +
  theme_SM() + 
  labs(title = "Critical infrastructure spatial index", x = "Mean", x = "")
  
ghsl_full_gender <- readxl::read_xlsx("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "SOCIOECONOMIC")

gender <- clust %>% 
  left_join(ghsl_full_gender %>% select(SC_SEC_GDF_2020, ID_UC_G0), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  mutate(SC_SEC_GDF_2020 = as.numeric(SC_SEC_GDF_2020)) %>% 
  group_by(cluster_name, consensus_label_majority) %>%
  summarise(sd = sd(SC_SEC_GDF_2020, na.rm = T),
            mean = mean(SC_SEC_GDF_2020, na.rm = T)) 

gender %>% 
  ggplot(aes(x = cluster_name, y = mean)) + 
  geom_bar(stat = "identity", position = "stack", col = "black", fill = "lightblue") +
  geom_text(aes(label = paste0(round(mean*100), "%")),
            size = 2.3, lineheight = .8, col = "black", vjust = -0.2) +
  theme_SM() + 
  labs(title = "Female Gender Development Index", x = "Mean", x = "")

ghsl_full_hdi <- readxl::read_xlsx("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "SOCIOECONOMIC")

hdi <- clust %>% 
  left_join(ghsl_full_hdi %>% select(SC_SEC_HDI_2020, ID_UC_G0), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  mutate(SC_SEC_HDI_2020 = as.numeric(SC_SEC_HDI_2020)) %>% 
  group_by(cluster_name) %>%
  summarise(sd = sd(SC_SEC_HDI_2020, na.rm = T),
            mean = mean(SC_SEC_HDI_2020, na.rm = T)) 

hdi %>% 
  ggplot(aes(x = cluster_name, y = mean)) + 
  geom_bar(stat = "identity", position = "stack", col = "black", fill = "lightblue") +
  geom_text(aes(label = paste0(round(mean*100), "%")),
            size = 2.3, lineheight = .8, col = "black", vjust = -0.2) +
  theme_SM() + 
  labs(title = "Female Gender Development Index", x = "Mean", x = "")

ghsl_full_hdi <- readxl::read_xlsx("/Users/simon/Downloads/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A.xlsx", sheet = "SOCIOECONOMIC")


emm <- emmissions_box_dat %>% 
  group_by(cluster_name) %>%
  # mutate(ODIAC = ODIAC/GC_POP_TOT_2025,) %>% 
  summarise(total = sum(ODIAC, na.rm = T)) %>% 
  mutate(share = total/sum(total))
  
# coastal cities
# critical infrastructure: IN_CIS_ALL, IN_CIS_TRA_
# gender: SC_SEC_GDM_XXXX (total), SC_SEC_GDM_XXXX (male), SC_SEC_GDF_XXXX (female)
# Human development index: SC_SEC_HDI_XXXX
# global relative deprivation
name <-  factor("Mega and large cities")

p_cov_simple <- left_join(coastal %>% 
            select(cluster_name, share) %>% 
            rename("Share of coastal cities\n(<25km to the coast)" = share), 
          gender %>% 
            select(cluster_name, mean) %>% 
            rename("Female gender index" = mean), 
          by = "cluster_name") %>% 
  left_join(infra %>% 
              select(cluster_name, mean) %>% 
              rename("Critical infrastructure spatial index" = mean), by = "cluster_name") %>% 
  left_join(hdi %>% 
              select(cluster_name, mean) %>% 
              rename("Human Development Index" = mean), by = "cluster_name") %>% 
  left_join(emm %>%
              select(cluster_name, share) %>%
              rename("Share of urban CO2 Emissions" = share), by = "cluster_name") %>%
  # filter(cluster_name == name) %>% 
  ungroup() %>% 
  pivot_longer(-cluster_name) %>% 
  # mutate(name = ifelse(name == "coastal_share"))
  ggplot(aes(x = cluster_name, y = value)) +
  geom_col(col = "black", fill = "lightblue") +
  # geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
  #               width = 0.2, color = "black") +
  coord_flip() +
  facet_wrap(.~name) + 
  geom_text(aes(label = paste0(round(value, 2))),
            size = 2.3, lineheight = .8, col = "black", hjust = -0.2) +
  theme_SM() +
  ylim(c(0,1)) +
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank()
        )

ggsave(p_cov_simple, file = "plots/p_cov_simple.pdf", width = 10, height = 5)

  

################################################################################
# case selection
################################################################################
min_max_scale <- function(x){
  res = (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(res)
}



best_30_per_group <- n_studies_per_city %>% 
  arrange(-n_studies) %>% 
  left_join(ghsl %>% select(GC_UCN_MAI_2025, ID_UC_G0), by = c("city_id" = "ID_UC_G0")) %>% 
  left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority, similarity), 
            by = c("city_id" = "GHS_urban_area_id")) %>% 
  # filter(city_id)
  filter(city_id != 2255) %>% 
  as.data.frame() %>% 
  group_by(consensus_label_majority) %>% 
  mutate(
    similarity_norm = min_max_scale(similarity),
    n_studies_norm = min_max_scale(n_studies),
    best_cases = similarity_norm * n_studies_norm
  ) %>% 
  mutate(best_cases = row_number() < 30) %>% 
  ungroup() %>% 
  filter(best_cases, !is.na(GC_UCN_MAI_2025)) %>% 
  left_join(cluster_names, by = "consensus_label_majority") 

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

case_selection <- list()
min_cl <- min(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
max_cl <- max(as.numeric(as.character(desc_dat_long$consensus_label_majority)))
for (cluster in min_cl:max_cl) {
  
  case_selection[[cluster+1]] <- desc_geo %>% 
    filter(consensus_label_majority == cluster) %>% 
    left_join(best_30_per_group %>% select(city_id, best_cases), by = c("ID_UC_G0" = "city_id")) %>% 
    mutate(best_cases = ifelse(is.na(best_cases), FALSE, best_cases)) %>% 
    ggplot() + 
    geom_sf(data = world, fill = "grey90", color = "white") +  # World map with light gray color
    geom_sf(aes(geometry = centroid, col = best_cases), size = .5, alpha = .3) +  
    ggrepel::geom_label_repel(
      aes(label = ifelse(best_cases == T, GC_UCN_MAI_2025, NA), geometry = centroid),
      stat = "sf_coordinates", alpha=.5, size = 1.5,  max.overlaps = 50,
    ) +
    scale_color_manual(values = c("#ffb84d", "black")) +
    geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
    coord_sf(crs = proj_robin) + 
    annotate(
      "label",
      x = -Inf, y = Inf,
      label = cluster_names$cluster_name[cluster_names$consensus_label_majority == cluster],
      hjust = 0, vjust = .9,
      size = 3,
      fill = "white",
      label.size = 0.3
    ) + 
    theme_SM() +
    theme(
      panel.border = element_rect(color = NA),
      legend.position = "none",
    ) +
    labs(col = "Cities", x = "", y = "")
}
  
case_selection_maps <- plot_grid(plotlist = case_selection, ncol = 2, labels = "auto", align = "v")
ggsave(case_selection_maps, filename = paste0("plots/case_selection_maps.pdf"), width = 10, height = 6)


case_selection_bar <- best_30_per_group %>% 
  # Use reorder with ID for ordering, but use GC_UCN_MAI_2025 for labels
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, n_studies), y = n_studies)) +
  
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  coord_flip() +
  facet_wrap(~cluster_name, scales = "free") +
  labs(
    x = "",
    y = "Number of Studies"
  ) +
  theme_SM()

ggsave(case_selection_bar, file = "plots/case_selection_bar.pdf", width = 10, height = 8)

library(ggplot2)
library(ggpubr)
library(scales)

# Population share plot
p_pop_share <- clust %>%
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name) %>%
  summarise(GHS_population = sum(GHS_population), .groups = "drop") %>%
  mutate(pop = GHS_population / sum(GHS_population)) %>%
  ggplot(aes(x = cluster_name, y = pop)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Cluster", y = "Population Share") +
  theme_SM() +
  theme(plot.title = element_blank())

# City share plot
p_city_share <- clust %>%
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name) %>%
  summarise(n_cities = n(), .groups = "drop") %>%
  mutate(share_cities = n_cities / sum(n_cities)) %>%
  ggplot(aes(x = cluster_name, y = share_cities)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Cluster", y = "City Share") +
  theme_SM() +
  theme(plot.title = element_blank())

# Combine plots
p_pop_and_city_share <- ggarrange(
  p_pop_share,
  p_city_share,
  labels = c("a", "b"),
  ncol = 2,
  align = "hv"
)

ggsave("plots/p_pop_and_city_share.pdf", p_pop_and_city_share, height = 7, width = 10)


library(scales)

# Population share by continent per cluster
p_pop_share_cont <- clust %>%
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name, continent) %>%
  summarise(GHS_population = sum(GHS_population), .groups = "drop") %>%
  mutate(pop_share = GHS_population / sum(GHS_population)) %>%
  ggplot(aes(x = continent, y = pop_share)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~cluster_name) +
  labs(x = "Continent", y = "Population Share") +
  theme_SM() +
  theme(plot.title = element_blank())

# City share by continent per cluster
p_city_share_cont <- clust %>%
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  group_by(cluster_name, continent) %>%
  summarise(n_cities = n(), .groups = "drop") %>%
  mutate(share_cities = n_cities / sum(n_cities)) %>%
  ggplot(aes(x = continent, y = share_cities)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~cluster_name) +
  labs(x = "Continent", y = "City Share") +
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

ipcc_regions <- st_transform(ipcc_regions, crs = 4326)
desc_geo <- st_transform(desc_geo, crs = 4326)

# Assuming desc_geo is a dataframe with lat/long and consensus_label_majority
desc_geo_sf <- st_as_sf(desc_geo, coords = c("longitude", "latitude"), crs = 4326)

# Assuming ipcc_regions is an sf object with the polygons of the regions
ipcc_regions <- st_make_valid(ipcc_regions)
desc_geo_sf <- st_make_valid(desc_geo_sf)
desc_geo_sf <- st_join(desc_geo_sf, ipcc_regions) 

desc_geo_sf <- desc_geo_sf %>% 
  mutate(Acronym = ifelse(Acronym %in% c("EPO", "NPO"), "PAC", Acronym)) 

cluster_share_most_prevalent <- desc_geo_sf %>%
  as.data.frame() %>%
  group_by(Acronym, consensus_label_majority) %>%
  summarise(n = n()) %>%
  group_by(Acronym) %>%
  mutate(share = n/sum(n)) %>% 
  arrange(-share, .by_group = T) %>% 
  slice(1:3) %>% 
  mutate(triangle_3_id = row_number())

ipcc_regions_hexa_split <- ipcc_regions_hexa_split %>% 
  # check this, probably needs to be broader and also include the other island cities
  mutate(label = ifelse(label %in% c("EPO", "NPO"), "PAC", label)) %>% 
  mutate(triangle_3_id  = case_when(triangle_id <=2 ~ 1, 
                                    triangle_id >2 & triangle_id <5 ~ 2,
                                    triangle_id >=5 ~ 3,)) %>% 
  left_join(cluster_share_most_prevalent, by = c("label" = "Acronym", "triangle_3_id")) 

# Function to scale triangle inward from centroid using share
scale_triangle <- function(triangle, share) {
  coords <- st_coordinates(triangle)[, 1:2]
  
  # Get points
  p1 <- coords[1, ]
  p2 <- coords[2, ]
  centroid <- coords[3, ]  # assumed order is [p1, p2, centroid, p1]
  
  # Compute scaled points
  s <- share
  new_p1 <- centroid + s * (p1 - centroid)
  new_p2 <- centroid + s * (p2 - centroid)
  
  # Make polygon
  new_coords <- rbind(new_p1, new_p2, centroid, new_p1)
  st_polygon(list(new_coords)) %>% st_sfc(crs = st_crs(triangle))
}

# Apply to each triangle
scaled_triangles <- purrr::pmap_dfr(ipcc_regions_hexa_split, function(geom, share, hex_id, triangle_id, label, triangle_3_id, consensus_label_majority,...) {
  if (is.na(share)) return(NULL)
  new_geom <- scale_triangle(geom, share)
  st_sf(geometry = new_geom, share = share, hex_id = hex_id, triangle_id = triangle_id, label = label, triangle_3_id = triangle_3_id, consensus_label_majority = consensus_label_majority)
})

# reassign crs
scaled_triangles <- st_set_crs(scaled_triangles, st_crs(ipcc_regions_hexa_split))

# group by the same types
scaled_triangles <- scaled_triangles %>% 
  group_by(hex_id, triangle_3_id) %>% 
  summarise(geometry = st_union(geometry), 
            share = first(share), 
            label = first(label), 
            consensus_label_majority = first(consensus_label_majority)) %>% 
  left_join(cluster_names, by = "consensus_label_majority")



library(ggpattern)
library(ggsci)
p_regional_types <- ggplot() + 
  geom_sf(data = ipcc_regions_hexa, fill = NA) + 
  # geom_sf(data = scaled_triangles, aes(fill = as.factor(consensus_label_majority))) +
  geom_sf_pattern(
    data = scaled_triangles,
    aes(pattern_colour = as.factor(cluster_name),
        pattern_angle = as.factor(cluster_name)
      ),
    pattern_density = .2,
    pattern_spacing = 0.01,
    # pattern_alpha = .5,
    pattern_size = .03,
    pattern_fill = "grey", 
    pattern_key_scale_factor = 0.6, 
    lwd = .5
  ) +
  scale_fill_npg() +
  geom_sf(data = ipcc_regions_hexa_groupings) +
  geom_label(
    data = ipcc_regions_hexa_groupings,
    aes(label = Name, x = auxiliary_storage_labeling_positionx, y = auxiliary_storage_labeling_positiony),
    alpha=.5, size = 2, label.size = NA
  ) + 
  theme_SM() + 
  labs(x= "", y = "") + 
  theme(panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "bottom")
ggsave(p_regional_types, filename = "plots/p_regional_types.pdf", width = 10, height = 5)

################################################################################
# learning potential by cluster overall
################################################################################



# p_cities_per_cluster_share <- clust %>% 
#   left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>% 
#   mutate(any_study_true = ifelse(n_studies > 1 & !is.na(n_studies), T, F)) %>% 
#   group_by(consensus_label_majority, any_study_true) %>% 
#   summarise(n_cities = n()) %>% 
#   ggplot(aes(x = consensus_label_majority, y = n_cities, fill = any_study_true)) + 
#   geom_bar(stat = "identity", position = "fill") +  
#   scale_fill_manual(values = c("#963d03", "#825d69")) +
#   coord_flip() +
#   scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
#   theme_SM() + 
#   labs(x = "Cluster", y = "Numer of Cities")
# p_cities_per_cluster_share
# ggsave(p_cities_per_cluster_share, file = "plots/p_cities_per_cluster_share.pdf", width = 10, height = 6)

library(ggpp)
p_cities_per_cluster_n <- clust %>%  
  # left_join(n_studies_per_city, by = c("GHS_urban_area_id" = "city_id")) %>%  
  mutate(any_study_true = ifelse(n_studies > 1 & !is.na(n_studies), "Has research", "No research")) %>%  
  group_by(consensus_label_majority, any_study_true) %>%  
  summarise(n_cities = n(), .groups = "drop") %>%  
  group_by(consensus_label_majority) %>%  
  mutate(
    total_cities = sum(n_cities),
    pct = round((n_cities / total_cities) * 100, 0),
    label = paste0(pct, "%")
  ) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  ggplot(aes(x = cluster_name, y = n_cities, fill = any_study_true)) +  
  geom_bar(stat = "identity", position = "stack", width = .5, col = "black", alpha = .5) +  
  geom_text(aes(label = label, y = ifelse(n_cities <200, n_cities + 300, n_cities)), 
            position = position_stacknudge(x = .4, vjust = 0.5), 
            size = 2.5, 
            color = "black") +
  scale_fill_manual(values = c("#1b9e77", "#8c8c8c")) +  
  coord_flip() +  
  scale_x_discrete(limits=rev) +
  theme_SM() + 
  theme(legend.title = element_text(), 
        legend.position = "none", 
        axis.title = element_text(size = 10)) +
  labs(x = "Cluster", y = "Number of Cities", fill = "Cities covered in case studies", subtitle = "Number of cities")
p_cities_per_cluster_n
ggsave(p_cities_per_cluster_n, file = "plots/p_cities_per_cluster_n.pdf", width = 5, height = 5)

labelled_topics <- readxl::read_xlsx("data/topic_model/labelled_topics_2.xlsx")
main_topic <- read.csv("data/topic_model/main_topic_220.csv")

clust_with_topics <- clust %>%
  left_join(clean_places, by = c("GHS_urban_area_id" = "city_id")) %>%
  left_join(main_topic, by = c("id" = "document")) %>%
  # left_join(labelled_topics %>% select(topic, name, group1, group2), by = c("topic")) %>% 
  left_join(oa %>% select(id, abstract, publication_year), by = "id") %>% 
  as_tibble()

n_case_studies_per_cluster <- clust_with_topics %>% 
  group_by(id) %>% 
  slice(1) %>% 
  group_by(consensus_label_majority) %>% 
  summarise(n_studies = n())

n_cities_per_cluster <- clust %>% 
  group_by(consensus_label_majority) %>% 
  summarise(n_cities = n())

n_pop_per_clust <- ghsl %>% 
  left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
  group_by(consensus_label_majority) %>% 
  summarise(n_pop = sum(GC_POP_TOT_2025))

p_over_under_researched_by_clust <- left_join(n_case_studies_per_cluster, n_cities_per_cluster, by = "consensus_label_majority") %>% 
  left_join(n_pop_per_clust, by = "consensus_label_majority") %>% 
  mutate(share_studies = n_studies/sum(n_studies),
         share_cities = n_cities/sum(n_cities),
         share_pop = n_pop/sum(n_pop),
         ratio_with_cities = share_studies/share_cities,
         ratio_with_pop = share_studies/share_pop,
  ) %>% 
  pivot_longer(c(
    ratio_with_cities,
                 ratio_with_pop), names_to = "ratio_type", values_to = "ratio") %>% 
  mutate(ratio_type = ifelse(ratio_type == "ratio_with_cities", "Share of cities", "Share of studies")) %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  ggplot(aes(x = cluster_name, y = ratio, shape = ratio_type, col = ratio_type)) +  
  geom_point(size = 3) + 
  scale_color_manual(values = c("#1b9e77", "#1b9e77")) +
  scale_shape_manual(values = c(1, 17)) +
  labs(x = "", y = expression(paste('log2', "(research share)")), col = "Normalization", shape = "Normalization", subtitle = "Normalised research share") +
  scale_y_continuous(transform = "log2", 
                     breaks = c(.125, .25, 0.5, 1, 2, 4, 8, 16),
                     labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8", "16")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  geom_hline(aes(yintercept = 1), lty = 2, alpha = 0.7, color = "gray50") +
  theme_SM() + 
  theme(legend.title = element_text(), 
        axis.title = element_text(size = 10),
        legend.position = c(.9,.5))

ggsave(p_over_under_researched_by_clust, file = "plots/p_over_under_researched_by_clust.pdf", width = 5, height = 5)

p_cities_and_over_under_per_clus <- ggarrange(p_cities_per_cluster_n, p_over_under_researched_by_clust, labels = c("a", "b"))
ggsave(p_cities_and_over_under_per_clus, file = "plots/p_cities_and_over_under_per_clus.pdf", width = 10, height = 5)

################################################################################
# learning potential by cluster by g1 topic
################################################################################

cities_g2_topics <- ghsl %>% 
  as.data.frame() %>% 
  select(ID_UC_G0, GC_UCN_MAI_2025) %>% 
  left_join(clean_places, by = c("ID_UC_G0" = "city_id")) %>% 
  left_join(main_topic %>% select(topic, gamma, document), by = c("id" = "document")) %>% 
  left_join(labelled_topics %>% select(topic, name, group1, group2), by = c("topic")) %>% 
  group_by(ID_UC_G0, group2, group1) %>% 
  summarise(n_studies = sum(!is.na(group2)), .groups = "drop") %>% 
  left_join(clust %>% select(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id")) 

# Step 1: Get all unique group2 values (excluding NA)
group2_vals <- cities_g2_topics %>%
  filter(!is.na(group2)) %>%
  distinct(group2)

# Step 3: Create a balanced panel of all combinations
balanced_panel <- expand.grid(
  ID_UC_G0 = unique(cities_g2_topics$ID_UC_G0),
  group2 = group2_vals$group2,
  stringsAsFactors = FALSE) %>%
  left_join(cities_g2_topics, by = c("ID_UC_G0", "group2")) %>%
  left_join(clust %>% distinct(GHS_urban_area_id, consensus_label_majority), by = c("ID_UC_G0" = "GHS_urban_area_id"), suffix = c("", "_from_cluster")) %>%
  mutate(
    n_studies = ifelse(is.na(n_studies), 0, n_studies),
    consensus_label_majority = coalesce(consensus_label_majority, consensus_label_majority_from_cluster)
  ) %>%
  select(ID_UC_G0, group2, n_studies, consensus_label_majority)

summary_df <- balanced_panel %>%
  mutate(research_presence = ifelse(n_studies > 0, "Has research", "No research")) %>%
  left_join(labelled_topics %>% distinct(group1, group2), by = "group2") %>% 
  group_by(consensus_label_majority, group1, group2, research_presence) %>%
  summarise(n_cities = n_distinct(ID_UC_G0), .groups = "drop") %>% 
  filter(!group1 %in% c("Other", "Impacts")) %>% 
  group_by(consensus_label_majority, group1, group2) %>%
  mutate(
    total_cities = sum(n_cities),
    pct = round((n_cities / total_cities) * 100, 0),
    label = paste0(pct, "%")
  ) %>%
  ungroup()

labels_vec <- setNames(str_replace_all(
  cluster_names$cluster_name,
  paste0("((?:\\S+\\s+){3})"),
  "\\1\n"
), as.character(cluster_names$consensus_label_majority))

p_learning_by_topic <- ggplot(summary_df, aes(x = group2, y = pct/100, fill = research_presence)) +
  geom_bar(stat = "identity", position = "fill", width = .5) +
  facet_grid(group1 ~ consensus_label_majority, scales = "free_y", space = "free", labeller = labeller(consensus_label_majority = as_labeller(labels_vec))) +
  geom_text(aes(label = ifelse(label == "0%", "", label)),
            position = position_stacknudge(x = .4, vjust = 0.5),
            size = 2.5,
            color = "black") +
  labs(
    title = "Cities with and without Research by Topic and Cluster",
    x = "",
    y = "Share",
    fill = "Research Status"
  ) +
  scale_fill_manual(values = c("#1b9e77", "#8c8c8c")) +  
  scale_y_continuous(labels = scales::percent) +
  coord_flip() + 
  theme_SM() +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 7),
        axis.text = element_text(size = 7))
ggsave(p_learning_by_topic, file = "plots/learning_by_topic.pdf", width = 10, height = 7)

################################################################################
# growth by group
################################################################################
# Assign IPCC phases
data_phase <- clust_with_topics %>%
  mutate(
    publication_year = as.numeric(publication_year),
    continent = ifelse(continent == "Australia", "Oceania", continent),
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

# Aggregate: n_studies per phase per cluster and continent
growth_by_phase <- data_phase %>%
  filter(!is.na(phase)) %>%
  group_by(phase, cluster_name, continent) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  mutate(phase = factor(phase, levels = c("AR1", "AR2", "AR3", "AR4", "AR5", "AR6"))) %>%
  arrange(cluster_name, continent, phase) %>%
  # summarise(n_studies = sum(n_studies)) %>% 
  group_by(cluster_name, continent) %>%
  mutate(
    baseline = first(n_studies),  # for normalized growth
    norm_growth = n_studies / baseline,
    # n_studies_lag = lag(n_studies),
    # pct_growth = round(100 * (n_studies - n_studies_lag) / n_studies_lag, 1)
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
    y = 520  # place slightly above the typical range of norm_growth
  )

# Final plot: normalized growth + annotations with phase-to-phase growth
p_g_rel <- ggplot(growth_by_phase, aes(x = year, y = norm_growth, color = continent, group = continent)) +
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
  ylim(c(0, 550)) + xlim(c(1990,2025)) +
  scale_color_npg() +
  labs(
    x = "Publication year",
    y = "Relative growth (normalized to AR1)"
  ) +
  theme_SM() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0),
    legend.position = c(.2, .75)
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

# Print
p_g_rel




# Step 1: Assign IPCC phase labels
data_phase <- clust_with_topics %>%
  mutate(
    publication_year = as.numeric(publication_year),
    continent = ifelse(continent == "Australia", "Oceania", continent),
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
    y = 3300  # start from 0; we’ll place it just above x-axis
  )

# Step 3: Main plot with bars, vlines, and annotations
p_g_abs <- data_phase %>%
  filter(publication_year >= 1990
         # & publication_year <= 2022
         ) %>%
  group_by(publication_year, cluster_name, continent) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  ggplot(aes(x = publication_year, y = n_studies, fill = continent)) +
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
  ylim(c(0, 3500)) +
  scale_fill_npg() +
  labs(x = "Publication year", y = "Studies") +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        legend.position = c(.2,.7)) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

p_growth <- ggarrange(p_g_abs, p_g_rel, ncol = 1, labels = c("a", "b"))
ggsave(p_growth, file = "plots/p_growth.pdf", height = 9, width = 10) 


# clust_with_topics %>% 
#   group_by(publication_year, consensus_label_majority) %>% 
#   filter(publication_year >= 1990 & publication_year <2025) %>% 
#   summarise(n_studies = n()) %>% 
#   ggplot(aes(x = publication_year, y = n_studies, fill = as.character(consensus_label_majority))) +
#   geom_col() +
#   # facet_wrap(~consensus_label_majority) +
#   scale_fill_npg() +
#   labs(x = "Publication year", y = "Studies") +
#   theme_SM()

# pop_2025_by_cluster <- ghsl %>% 
#   left_join(clust %>% select(consensus_label_majority, GHS_urban_area_id), by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
#   as.data.frame() %>% 
#   group_by(consensus_label_majority) %>% 
#   summarise(pop = sum(GC_POP_TOT_2025)) %>% 
#   mutate(pop_share = pop/sum(pop))
# 
# 
# clust_with_topics %>% 
#   filter(publication_year >= 1990 & publication_year <2025) %>% 
#   filter(!is.na(id)) %>% 
#   group_by(publication_year, consensus_label_majority) %>% 
#   summarise(n_studies = n()) %>% 
#   mutate(share_studies = n_studies/sum(n_studies)) %>% 
#   left_join(pop_2025_by_cluster, by = "consensus_label_majority") %>% 
#   mutate(ratio = share_studies/pop_share) %>% 
#   pivot_longer(c("share_studies", "pop_share")) %>% 
#   ggplot(aes(x = publication_year, y = value, col = name, group = name)) +
#   geom_line() +
#   facet_wrap(~consensus_label_majority) + 
#   scale_fill_npg() +
#   labs(x = "Publication year", y = "Studies") +
#   theme_SM()




# ################################################################################
# # aggregate learning potential for 4 solution types and ipcc hexas
# ################################################################################
# 
# keywords_compact <- c("compact city",
#               "walkability",
#               "walkable neighborhood",
#               "high density",
#               "mixed use",
#               "transit-oriented development",
#               "TOD",
#               "urban sprawl",
#               "active transport",
#               "15-minute city")
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
#   "zero-energy building"
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
#   "nature-based solution"
# )
# 
# solution_topics <- ghsl %>%
#   mutate(geometry = st_centroid(geom)) %>%
#   select(ID_UC_G0, geom) %>%
#   left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
#   mutate(
#     contains_compact = str_detect(abstract, str_c(fixed(keywords_compact), collapse = "|")),
#     contains_electrific = str_detect(abstract, str_c(fixed(keywords_electrific), collapse = "|")),
#     contains_build = str_detect(abstract, str_c(fixed(keywords_build), collapse = "|")),
#     contains_blue_green = str_detect(abstract, str_c(fixed(keywords_blue_green), collapse = "|"))
#   )
# 
# solution_topics %>% 
#   filter(contains_compact | contains_electrific | contains_build | contains_blue_green) %>% 
#   filter(stri_detect_fixed(abstract, "systematic review") | stri_detect_fixed(abstract, "evidence synthesis") | stri_detect_fixed(abstract, "meta analysis")) %>% 
#   select(abstract)
# 
# # A) which types on which hexa
# clust_with_city_geom <- left_join(ghsl %>% select(ID_UC_G0) %>% mutate(geom = st_centroid(geom)), clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
#   st_transform(4326) %>% 
#   st_make_valid() 
# 
# ipcc_cluster_presence <- ipcc_regions %>%
#   st_join(clust_with_city_geom) %>%
#   st_drop_geometry() %>%
#   group_by(Acronym, consensus_label_majority) %>%
#   summarise(cluster_present = any(!is.na(ID_UC_G0)), .groups = "drop") %>%
#   mutate(cluster_col = paste0("cluster_", consensus_label_majority)) %>%
#   select(-consensus_label_majority) %>%
#   pivot_wider(names_from = cluster_col, values_from = cluster_present, values_fill = FALSE) %>% 
#   pivot_longer(starts_with("cluster_"), names_to = "consensus_label_majority", values_to = "cluster_present") %>%
#   mutate(consensus_label_majority = as.numeric(gsub("cluster_", "", consensus_label_majority))) %>% 
#   filter(!is.na(consensus_label_majority))
# 
# ipcc_regions_clusters <- left_join(ipcc_regions, ipcc_cluster_presence, by = c("Acronym"))
# 
# ipcc_regions_hexa_clusters <- ipcc_regions_hexa %>% 
#   left_join(ipcc_regions_clusters %>% as.data.frame() %>% select(Acronym, consensus_label_majority, cluster_present), by = c("label" = "Acronym"))
# 
# # B) which solutions on which hexa
# 
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
#   select(ID_UC_G0, Name, consensus_label_majority, Acronym,
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
#                            contains_compact = "Compact City",
#                            contains_electrific = "Transport Electrification",
#                            contains_build = "Green Buildings",
#                            contains_blue_green = "Blue-Green Infrastructure"
#     )
#   ) %>% 
#   filter(!is.na(solution_type) & !is.na(consensus_label_majority))
# 
# labels_vec <- setNames(str_replace_all(
#   cluster_names$cluster_name,
#   paste0("((?:\\S+\\s+){4})"),
#   "\\1\n"
# ), as.character(cluster_names$consensus_label_majority))
# 
# 
# # Step 5: Plot
# p_solutions_by_ipcc_region_and_cluster <- ggplot() +
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
#   
#   facet_grid(consensus_label_majority ~ solution_type,   labeller = labeller(consensus_label_majority = as_labeller(labels_vec))) +
#   labs(
#     title = "Number of Studies Documenting Each Urban Solution by IPCC Region and Cluster",
#     subtitle = "Regions with no studies -> grey; regions with no cities in that cluster -> white",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_SM() +
#   theme(
#     panel.border = element_blank(),
#     strip.text.y = element_text(angle = 0),
#     axis.ticks = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     legend.position = "bottom"
#   )
# p_solutions_by_ipcc_region_and_cluster
# ggsave(p_solutions_by_ipcc_region_and_cluster, file = "plots/p_solutions_by_ipcc_region_and_cluster.pdf", width = 10, height = 7)
# 
# p_solutions_by_ipcc_region_and_cluster_bar <- solution_summary %>% 
#   group_by(consensus_label_majority, solution_type) %>% 
#   summarise(n_studies = sum(n_studies)) %>% 
#   mutate(
#     solution_type = recode(solution_type,
#                            contains_compact = "Compact City",
#                            contains_electrific = "Transport Electrification",
#                            contains_build = "Green Buildings",
#                            contains_blue_green = "Blue-Green"
#     )
#   ) %>% 
#   ggplot(aes(solution_type, n_studies, fill = n_studies)) +
#   geom_bar(stat = "identity", position = "stack", col ="black", fill = "lightblue", alpha = .1) + 
#   # scale_fill_viridis_c(
#   #   option = "C",
#   #   na.value = "grey90",
#   #   name = "Cities with\nResearch",
#   # ) +
#   labs(y = "Studies", x= "") + 
#   facet_grid(. ~consensus_label_majority ) +
#   theme_SM()+
#   # theme(plot.margin = margin(0,1,0,0, "cm")) +
#   theme(legend.position = "none")
# p_solutions_by_ipcc_region_and_cluster_bar
# ggsave(p_solutions_by_ipcc_region_and_cluster_bar, file = "plots/p_solutions_bar.pdf", width = 10, height = 8)
# 
# p_solutions_by_ipcc_region_and_cluster <- ggarrange(p_solutions_by_ipcc_region_and_cluster_map, p_solutions_by_ipcc_region_and_cluster_bar, labels = c("a", "b"), ncol = 1, heights = c(2,1.5))
# ggsave(p_solutions_by_ipcc_region_and_cluster, file = "plots/p_solutions_by_ipcc_region_and_cluster.pdf", width = 10, height = 8)




################################################################################
# agg learning index
################################################################################

library(dplyr)
library(sf)

# Set the cluster to analyze
n_cluster <- 0

# Step 1: Subset cities in cluster 1
cluster_cities <- ghsl %>%
  mutate(geom = st_centroid(geom)) %>%
  dplyr::select(ID_UC_G0, geom) %>%
  left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id"))
  # filter(consensus_label_majority == n_cluster)

# Step 2: Count publications per city-topic pair
pub_counts <- cluster_cities %>%
  as.data.frame() %>% 
  filter(!is.na(group2)) %>%
  group_by(ID_UC_G0, group2) %>% 
  summarise(topic_count = n())

# Step 3: Create city-topic matrix (wide format)
city_topic_matrix <- pub_counts %>%
  pivot_wider(names_from = "group2", values_from = "topic_count", values_fill = 0)
  # mutate(has_research = T)

city_topic_matrix <- ghsl %>% 
  dplyr::select(ID_UC_G0) %>% 
  left_join(city_topic_matrix, by = "ID_UC_G0") %>% 
  # mutate(has_research = ifelse(is.na(has_research), F, has_research)) %>% 
  mutate(ID_UC_G0 = as.character(ID_UC_G0)) %>% 
  mutate_if(is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)})

# Step 4: Compute research volume and evenness per city
research_metrics <- city_topic_matrix %>%
  as.data.frame() %>% 
  # mutate(ID_UC_G0 = as.character(ID_UC_G0)) %>% 
  rowwise() %>%
  mutate(
    research_volume = sum(c_across(where(is.numeric))),
    # research_evenness = {
    #   counts <- c_across(where(is.numeric))
    #   total <- sum(counts)
    #   if (total == 0) 0 else {
    #     p <- counts / total
    #     -sum(p * log(p + 1e-10)) / log(length(p))  # Normalized entropy
    #   }
    # },
    # Gini-based evenness
    research_evenness = {
      counts <- c_across(where(is.numeric))
      if (sum(counts) == 0) 0 else (1 - ineq::Gini(counts))
    }
  ) %>%
  ungroup() %>%
  dplyr::select(ID_UC_G0, research_volume, research_evenness)

# Step 5: Compute similarity scores between cities based on co_vars
co_mat <- cluster_cities %>%
  as.data.frame() %>% 
  dplyr::select(ID_UC_G0, all_of(co_vars), consensus_label_majority) %>%
  distinct() %>%
  inner_join(research_metrics %>% mutate(ID_UC_G0 = as.numeric(ID_UC_G0)), by = "ID_UC_G0") %>% 
  mutate(research_volume = min_max_scale(research_volume))# ensure alignment

# Scale and compute similarity
sim_matrix <- proxy::simil(
  as.matrix(co_mat %>% dplyr::select(all_of(co_vars)) %>% scale()),
  method = "cosine",
  by_rows = TRUE
)

similarity_sums <- rowSums(as.matrix(sim_matrix), na.rm = T)


# Final TPI and LPI
co_mat <-  co_mat %>%
  mutate(
    similarity_decile = min_max_scale(similarity_sums),  # ranks from .1 to 1
    teaching_volume_potential = similarity_decile * research_volume,
    teaching_diversity_potential = similarity_decile * research_evenness,
    learning_volume_potential = similarity_decile * mean(co_mat$research_volume, na.rm = TRUE),
    learning_diversity_potential = similarity_decile * mean(co_mat$research_evenness, na.rm = TRUE)
  )

co_mat %>%
  left_join(ghsl %>% dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = "ID_UC_G0") %>% 
  dplyr::select(consensus_label_majority, GC_UCN_MAI_2025, GC_CNT_GAD_2025, similarity_decile, research_volume, research_evenness,
         learning_volume_potential, learning_diversity_potential) %>%
  group_by(consensus_label_majority) %>% 
  arrange(desc(learning_volume_potential)) %>%
  slice(1:5) %>% 
  as.data.frame()

co_mat %>%
  left_join(ghsl %>% dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = "ID_UC_G0") %>% 
  dplyr::select(consensus_label_majority, GC_UCN_MAI_2025, GC_CNT_GAD_2025, similarity_decile, research_volume, research_evenness,
         teaching_volume_potential, teaching_diversity_potential) %>%
  group_by(consensus_label_majority) %>% 
  arrange(desc(teaching_volume_potential), ) %>%
  slice(1:5) %>% 
  as.data.frame()

teach_potential_volume <- co_mat %>%
  mutate(has_research = ifelse(research_volume == 0, F, T)) %>%
  filter(has_research) %>%
  group_by(consensus_label_majority, has_research) %>%
  summarise(teaching_volume_potential = sum(teaching_volume_potential)) %>%
  ggplot(aes(x = factor(consensus_label_majority), y = teaching_volume_potential, fill = has_research)) +
  geom_bar(stat = "identity", position = "stack", width = .5, col = "black", alpha = .5) +
  scale_fill_manual(values = c("#1b9e77")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  # scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
  theme_SM() +
  theme(legend.title = element_text(),
        legend.position = "none", 
        axis.title = element_text(size = 10)) +
  labs(x = "", y = "Number of studies", fill = "has research", subtitle = "Teaching: similarity-weighted\nnumber of studies")
teach_potential_volume

teach_potential_evenness <- co_mat %>%
  mutate(has_research = ifelse(research_volume == 0, F, T)) %>%
  filter(has_research) %>%
  group_by(consensus_label_majority, has_research) %>%
  summarise(teaching_diversity_potential = sum(teaching_diversity_potential)) %>%
  ggplot(aes(x = factor(consensus_label_majority), y = teaching_diversity_potential, fill = has_research)) +
  geom_bar(stat = "identity", position = "stack", width = .5, col = "black", alpha = .5) +
  scale_fill_manual(values = c("#1b9e77")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  # scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
  # scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  theme_SM() +
  theme(legend.title = element_text(),
        legend.position = "none", 
        axis.title = element_text(size = 10)) +
  labs(x = "", y = "Gini score", fill = "has research", subtitle = "Teaching: diversity\nof the topics")
teach_potential_evenness

learn_potential_volume <- co_mat %>%
  mutate(has_research = ifelse(research_volume == 0, F, T)) %>%
  group_by(consensus_label_majority, has_research) %>%
  summarise(learning_volume_potential = sum(learning_volume_potential)) %>%
  ggplot(aes(x = factor(consensus_label_majority), y = learning_volume_potential, fill = has_research)) +
  geom_bar(stat = "identity", position = "dodge", width = .6, col = "black", alpha = .5) +
  scale_fill_manual(values = c("#8c8c8c", "#1b9e77")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  # scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
  # scale_y_continuous(labels = scales::percent) +
  theme_SM() +
  theme(legend.title = element_text(),
        legend.position = c(.8,.1), 
        axis.title = element_text(size = 10)) +
  labs(x = "", y = "Number of studies", fill = "has research", subtitle = "Learning: similarity-weighted\nnumber of studies")
learn_potential_volume

learn_potential_evenness <- co_mat %>%
  mutate(has_research = ifelse(research_volume == 0, F, T)) %>%
  group_by(consensus_label_majority, has_research) %>%
  summarise(learning_diversity_potential = mean(learning_diversity_potential)) %>%
  ggplot(aes(x = factor(consensus_label_majority), y = learning_diversity_potential, fill = has_research)) +
  geom_bar(stat = "identity", position = "dodge", width = .6, col = "black", alpha = .5) +
  scale_fill_manual(values = c("#8c8c8c", "#1b9e77")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  # scale_x_continuous(n.breaks = length(unique(clust$consensus_label_majority))) +
  theme_SM() +
  theme(legend.title = element_text(),
        legend.position = "none", 
        axis.title = element_text(size = 10)) +
  labs(x = "", y = "Gini score", subtitle = "Learning: diversity\nof the topics")
learn_potential_evenness

p_learning_potential <- ggarrange(p_cities_per_cluster_n, learn_potential_volume, learn_potential_evenness,
                                  p_over_under_researched_by_clust, teach_potential_volume, teach_potential_evenness, 
                                  align = "h", labels = c("a", "b"), widths = c(1.9,1,1))
ggsave(p_learning_potential, file = "plots/p_learning_potential.pdf", width = 10, height = 8)


################################################################################
# learning potential on map 
################################################################################

learn_pot <- co_mat %>%
  left_join(ghsl %>% dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = "ID_UC_G0") %>% 
  dplyr::select(ID_UC_G0, consensus_label_majority, similarity_decile, research_volume, research_evenness,
         learning_volume_potential, learning_diversity_potential, teaching_volume_potential) %>%
  group_by(consensus_label_majority) %>% 
  arrange(desc(learning_volume_potential)) 

ghsl %>% 
  st_transform(proj_robin) %>% 
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025) %>% 
  mutate(geom = st_centroid(geom)) %>% 
  left_join(learn_pot, by = "ID_UC_G0") %>% 
  ggplot(aes(col = learning_volume_potential)) +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +  # World map with light gray color
  geom_sf() +
  geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
  theme_SM() + labs(y = "", x = "") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.15, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,0,0,0), "cm")
  ) 


library(ggplot2)
library(sf)
library(dplyr)

# Assume your dataset already contains the centroid geometries in `geom` and the variable `learning_volume_potential`
spike_scale <- 10000
ghsl %>%
  st_transform(proj_robin) %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025) %>%
  mutate(geom = st_centroid(geom)) %>%
  left_join(learn_pot, by = "ID_UC_G0") %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2],
    yend = y + learning_volume_potential * spike_scale  # scale this to control spike height
  ) %>%
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA, size = .3) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend), color = "red", linewidth = 0.3) +
  geom_sf(aes(geometry = geom, col = learning_volume_potential)) +
  geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
  theme_SM() +
  labs(y = "", x = "") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.text.y = element_blank(),
    text = element_text(size = 8),
    panel.spacing = unit(-0.15, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(c(-1,0,0,0), "cm")
  )

library(sf)
library(gstat)
library(raster)
library(ggplot2)
library(terra)

# Extract coordinates and create SpatialPointsDataFrame
ghsl_df <- ghsl %>%
  st_transform(proj_robin) %>%
  mutate(geom = st_centroid(geom)) %>%
  left_join(learn_pot %>% dplyr::select(ID_UC_G0, learning_volume_potential), by = "ID_UC_G0") %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2]
  ) %>%
  st_drop_geometry()  # Drop sf geometry to make a pure data.frame

# Convert to SpatialPointsDataFrame
coordinates(ghsl_df) <- ~x + y
proj4string(ghsl_df) <- CRS("+proj=robin")

gridded(ghsl_df) <- FALSE

# Define world extent in Robinson projection
# Approximate bounds for Robinson projection
x_range <- c(-17000000, 17000000)
y_range <- c(-9000000, 9000000)

# Define resolution (in meters, adjust as needed)
res <- 50000  # 

# Create grid
x_seq <- seq(x_range[1], x_range[2], by = res)
y_seq <- seq(y_range[1], y_range[2], by = res)
grid_df <- expand.grid(x = x_seq, y = y_seq)

coordinates(grid_df) <- ~x + y
proj4string(grid_df) <- CRS("+proj=robin")
gridded(grid_df) <- TRUE

# Interpolate (e.g., IDW)
idw_result <- idw(formula = learning_volume_potential ~ 1, locations = ghsl_df, newdata = grid_df)

# Convert to raster
r <- raster(idw_result)

# Optionally: terrain model
slope <- terrain(r, opt = 'slope')
aspect <- terrain(r, opt = 'aspect')
hill <- hillShade(slope, aspect, angle = 40, direction = 270)

# Plot in ggplot
ggplot() +
  geom_sf(data = world %>% st_union(), fill = "white", color = "black", size = .3, alpha = .2) +  # World map with light gray color
  geom_raster(data = as.data.frame(r, xy = TRUE), aes(x = x, y = y, fill = var1.pred), alpha = .9) +
  scale_fill_viridis_c() +
  geom_sf(data = bb, fill = NA, color = "grey30") +
  theme_void()

# Assume r is your RasterLayer
r_poly <- rasterToPolygons(r, dissolve = TRUE)
r_sf <- st_as_sf(r_poly)
r_sf <- st_set_crs(r_sf, proj_robin)
is_not_within <- st_intersects(r_sf, bb)
is_not_within <- sapply(is_not_within, is_empty)
r_sf2 <- r_sf %>% filter(!is_not_within)

# Plot in ggplot
p_aggregate_learning_potential <- ggplot() +
  geom_sf(data = world %>% st_union(), fill = "white", color = "black", size = .3, alpha = .2) +  # World map with light gray color
  geom_sf(data = r_sf2, aes(fill = var1.pred, col = var1.pred), alpha = .9) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data = bb, fill = NA, color = "grey30", linewidth = 1) +
  labs(fill = "Learning potential", col = "Learning potential") + 
  theme_SM() + 
  theme(legend.position = c(0.2,0),
        legend.direction = "horizontal",
        legend.title = element_text(),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.15, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,0,0,0), "cm")
  ) 

p_aggregate_learning_potential
ggsave(p_aggregate_learning_potential, file = "plots/p_aggregate_learning_potential.pdf", width = 10, height = 6)

p_learning <- ggarrange(p_aggregate_learning_potential,
          p_learning_potential, 
            # theme(axis.text = element_text(size = 10),
                  # strip.text = element_text(size = 10)),
          ncol = 1, heights = c(1,1.5))
ggsave(p_learning, file = "plots/p_learning.pdf", width = 10, height = 11)


################################################################################
# scalable climtate solutions
################################################################################


keywords_compact <- c("compact city",
                      "walkability",
                      "walkable neighborhood",
                      "high density",
                      "mixed use",
                      "transit-oriented development",
                      "TOD",
                      "urban sprawl",
                      "active transport",
                      "15-minute city")

keywords_electrific <- c(
  "electric vehicle",
  "electric bus",
  "EV adoption",
  "e-mobility",
  "low-carbon transport",
  "mode shift",
  "public transport electrification",
  "shared mobility",
  "zero-emission vehicle",
  "transport decarbonization"
)

keywords_build <- c(
  "energy-efficient building",
  "building retrofit",
  "passive house",
  "green building",
  "low-carbon material",
  "embodied carbon",
  "net-zero building",
  "building envelope",
  "HVAC efficiency",
  "zero-energy building"
)

keywords_blue_green <- c(
  "green infrastructure",
  "green roof",
  "green wall",
  "urban forest",
  "tree canopy",
  "blue infrastructure",
  "stormwater management",
  "permeable surface",
  "heat island mitigation",
  "nature-based solution"
)

solution_topics <- ghsl %>%
  mutate(geometry = st_centroid(geom)) %>%
  
  dplyr::select(ID_UC_G0, geom) %>%
  left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
  mutate(
    contains_compact = str_detect(abstract, str_c(fixed(keywords_compact), collapse = "|")),
    contains_electrific = str_detect(abstract, str_c(fixed(keywords_electrific), collapse = "|")),
    contains_build = str_detect(abstract, str_c(fixed(keywords_build), collapse = "|")),
    contains_blue_green = str_detect(abstract, str_c(fixed(keywords_blue_green), collapse = "|"))
  )

solution_topics

# A) which types on which hexa
clust_with_city_geom <- left_join(ghsl %>% dplyr::select(ID_UC_G0) %>% mutate(geom = st_centroid(geom)), clust, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>% 
  st_transform(4326) %>% 
  st_make_valid() 

ipcc_cluster_presence <- ipcc_regions %>%
  st_join(clust_with_city_geom) %>%
  st_drop_geometry() %>%
  group_by(Acronym, consensus_label_majority) %>%
  summarise(cluster_present = any(!is.na(ID_UC_G0)), .groups = "drop") %>%
  mutate(cluster_col = paste0("cluster_", consensus_label_majority)) %>%
  dplyr::select(-consensus_label_majority) %>% 
  pivot_wider(names_from = cluster_col, values_from = cluster_present, values_fill = FALSE) %>% 
  pivot_longer(starts_with("cluster_"), names_to = "consensus_label_majority", values_to = "cluster_present") %>%
  mutate(consensus_label_majority = as.numeric(gsub("cluster_", "", consensus_label_majority))) %>% 
  filter(!is.na(consensus_label_majority))

# ipcc_regions_clusters <- left_join(ipcc_regions, ipcc_cluster_presence, by = c("Acronym"))

ipcc_regions_hexa_clusters <- ipcc_regions_hexa %>% 
  left_join(ipcc_cluster_presence, by = c("label" = "Acronym"))

# B) which solutions on which hexa

solution_topics <- st_transform(solution_topics, 4326)
ipcc_regions_hexa <- st_transform(ipcc_regions_hexa, 4326)

# Step 1: Spatial join - assign each city point to an IPCC region
solution_with_region <- solution_topics %>%
  st_as_sf() %>%
  st_make_valid() %>% 
  st_join(ipcc_regions) 

# solution_with_region <- solution_with_region %>% 
#   mutate(Acronym = ifelse(Acronym %in% c("EPO", "NPO"), "PAC", Acronym)) 

# Step 2: Reshape from wide to long for the solution types
solution_summary <- solution_with_region %>%
  dplyr::select(ID_UC_G0, Name, consensus_label_majority, Acronym,
         contains_compact, contains_electrific, contains_build, contains_blue_green) %>%
  pivot_longer(
    cols = starts_with("contains_"),
    names_to = "solution_type",
    values_to = "has_solution"
  ) %>%
  as.data.frame() %>% 
  group_by(consensus_label_majority, Acronym, solution_type) %>% 
  summarise(n_studies = sum(has_solution, na.rm = T), .groups = "drop")

# Step 4: Join counts back to IPCC region polygons
ipcc_map_data <- ipcc_regions_hexa %>%
  left_join(solution_summary, by = c("label" = "Acronym"))  # 'Name' is region name

# Optional: clean up solution_type labels
ipcc_map_data <- ipcc_map_data %>%
  mutate(
    solution_type = recode(solution_type,
                           contains_compact = "Compact\nCity",
                           contains_electrific = "Transport\nElectrification",
                           contains_build = "Green\nBuildings",
                           contains_blue_green = "Blue-Green\nInfrastructure"
    )
  ) %>% 
  filter(!is.na(solution_type) & !is.na(consensus_label_majority))

labels_vec <- setNames(str_replace_all(
  cluster_names$cluster_name,
  paste0("((?:\\S+\\s+){2})"),
  "\\1\n"
), as.character(cluster_names$consensus_label_majority))

# Step 5: Plot
p_solutions_by_ipcc_region_and_cluster_map <- ggplot() +
  # First layer: IPCC regions with cluster presence as binary fill (white vs grey)
  geom_sf(data = ipcc_regions_hexa_clusters, aes(geometry = geom), fill = "white", color = "grey") +
  geom_sf(data = ipcc_regions_hexa_clusters %>% filter(cluster_present),
          aes(geometry = geom), fill = "grey90", color = NA) +
  geom_sf(data = ipcc_map_data,
          aes(geometry = geom, fill = log2(n_studies)), color = "grey") +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey90",
    name = "Cities with\nResearch",
    breaks = log2(2^(0:8)),
    labels = c(2^(0:8))
  ) +
  facet_grid(solution_type~consensus_label_majority,  
             labeller = labeller(consensus_label_majority = as_labeller(labels_vec))
  ) +
  labs(
    title = "Number of Studies Documenting Each Urban Solution by IPCC Region and Cluster",
    subtitle = "Regions with no studies -> grey; regions with no cities in that cluster -> white",
    x = NULL,
    y = NULL
  ) +
  theme_SM() +
  theme(plot.margin = margin(1,0,0,0, "cm"),
        strip.position="left") +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(1.1, 1), 
    strip.text.y = element_text(angle = 0),
    strip.text = element_text(size = 9)
  )
p_solutions_by_ipcc_region_and_cluster_map
ggsave(p_solutions_by_ipcc_region_and_cluster_map, file = "plots/p_solutions_by_ipcc_region_and_cluster_map.pdf", width = 10, height = 7)

p_solutions_by_ipcc_region_and_cluster_bar <- solution_summary %>% 
  group_by(consensus_label_majority, solution_type) %>% 
  summarise(Studies = sum(n_studies), .groups = "drop") %>% 
  mutate(
    solution_type = recode(solution_type,
                           contains_compact = "Compact City",
                           contains_electrific = "Transport Electrification",
                           contains_build = "Green Buildings",
                           contains_blue_green = "Blue-Green"
    )
  ) %>% 
  group_by(consensus_label_majority) %>% 
  mutate(
    Percentage = Studies / sum(Studies) * 100
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c("Studies", "Percentage"), 
    names_to = "value_type", 
    values_to = "value"
  ) %>% 
  mutate(value = round(value, 0)) %>% 
  ggplot(aes(x = solution_type, y = value)) +
  geom_bar(stat = "identity", position = "stack", col ="black", fill = "lightblue", alpha = .1) +
  geom_text(aes(x = solution_type, y = value, 
                label = ifelse(value_type ==  "Percentage", paste0(value, "%"), value)),
            vjust = -1, size = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  facet_grid(value_type ~ consensus_label_majority, scales = "free_y", switch = "x") +
  labs(x = "", y = NULL) +
  theme_SM() +
  theme(legend.position = "none", 
        strip.text.x = element_blank(),
        element_text(size = 10),
        strip.text.y = element_text(face = "bold"),
        plot.margin = margin(.6, 2.5, -1, 0, unit = "cm"),
  ) 
p_solutions_by_ipcc_region_and_cluster_bar
ggsave(p_solutions_by_ipcc_region_and_cluster_bar, file = "plots/p_solutions_bar.pdf", width = 10, height = 8)

p_solutions_by_ipcc_region_and_cluster <- ggarrange(p_solutions_by_ipcc_region_and_cluster_map, p_solutions_by_ipcc_region_and_cluster_bar, 
                                                    labels = c("a", "b"), ncol = 1, 
                                                    heights = c(4,2.5),
                                                    align = "h"
)
ggsave(p_solutions_by_ipcc_region_and_cluster, file = "plots/p_solutions_by_ipcc_region_and_cluster.pdf", width = 10, height = 8)


# # Optional: make prettier solution_type labels
# solution_summary <- solution_summary %>%
#   mutate(
#     solution_type = recode(solution_type,
#                            contains_compact = "Compact City",
#                            contains_electrific = "Transport Electrification",
#                            contains_build = "Green Buildings",
#                            contains_blue_green = "Blue-Green Infra"
#     )
#   )
# 
# # Step 4: Plot
# ggplot(solution_summary, aes(x = solution_type, y = n_cities, fill = as.factor(consensus_label_majority))) +
#   geom_col(position = "dodge") +
#   facet_wrap(~Name, scales = "free_y") +
#   labs(
#     title = "Cities Documenting Each Urban Solution by Cluster and IPCC Region",
#     x = "Solution Type",
#     y = "Number of Cities",
#     fill = "Cluster"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 


# ################################################################################
# # learning potential based on topics from the topic model 
# ################################################################################

generate_learning_potential_data <- function(ghsl, clust_with_topics, clusters, topic_groups, distance_based_exclusion = T) {
  
  panel_tags <- letters[1:length(cluster_list)]
  all_edges <- list()
  all_cities <- list()
  
  for (i in seq_along(clusters)) {
    
    n_cluster <- clusters[i]
    topic_of_interest <- topic_groups
    cluster_label <- paste0("Cluster ", n_cluster)
    topic_label <- paste(topic_of_interest, collapse = ", ")
    panel_label <- paste0("(", panel_tags[i], ") ", cluster_label)
    
    cities_classified <- ghsl %>%
      mutate(geom = st_centroid(geom)) %>%
      select(ID_UC_G0, geom) %>%
      left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
      filter(consensus_label_majority == n_cluster) %>%
      group_by(ID_UC_G0, consensus_label_majority) %>%
      summarise(contains_topic = any(name %in% topic_of_interest)) %>%
      ungroup()
    
    cities_with <- cities_classified %>% filter(contains_topic)
    cities_without <- cities_classified %>% filter(!contains_topic)
    
    if (nrow(cities_with) == 0 | nrow(cities_without) == 0) next
    
    edges <- tidyr::crossing(
      from = cities_without %>% as.data.frame() %>% select(from_id = ID_UC_G0, from_geom = geom),
      to = cities_with %>% as.data.frame() %>% select(to_id = ID_UC_G0, to_geom = geom)
    ) %>% unnest()
    
    # if (distance_based_exclusion == T){
    edges <- edges %>%
      mutate(dist = st_distance(from_geom, to_geom, by_element = TRUE)) %>%
      group_by(from_id) %>%
      slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
      ungroup()
    # }
    
    edges_lines <- edges %>%
      rowwise() %>%
      mutate(geometry = st_sfc(
        st_linestring(rbind(st_coordinates(from_geom), st_coordinates(to_geom))),
        crs = st_crs(from_geom))
      ) %>%
      st_as_sf() %>%
      mutate(panel = panel_label)
    
    all_edges[[i]] <- edges_lines
    
    cities_combined <- bind_rows(
      cities_with %>% mutate(type = "With topic"),
      cities_without %>% mutate(type = "Without topic")
    ) %>%
      mutate(panel = panel_label)
    
    all_cities[[i]] <- cities_combined
  }
  
  combined_edges <- bind_rows(all_edges)
  combined_cities <- bind_rows(all_cities)
  
  return(list(edges = combined_edges, cities = combined_cities))
}

plot_learning_potential <- function(edges, cities, world, bb) {
  ggplot() +
    geom_sf(data = world %>% st_union(), fill = "grey95", color = NA) +
    geom_sf(data = cities %>% filter(type == "Without topic"), 
            aes(color = type), size = 0.3, alpha = 0.4) +
    geom_sf(data = edges, aes(geometry = geometry), color = "#ffc266", size = 0.3, alpha = 0.3) +
    geom_sf(data = cities %>% filter(type == "With topic"), 
            aes(color = type), size = 0.3, alpha = 0.5, show.legend = FALSE) +
    scale_color_manual(
      values = c("With topic" =  "royalblue1", "Without topic" = "salmon"),
      name = "City Type"
    ) +
    geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
    facet_wrap(~ panel, ncol = 2) +
    theme_void() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8, face = "bold"),
      panel.spacing = unit(0.4, "lines"),
      plot.margin = margin(5, 5, 5, 5),
      text = element_text(size = 8)
    ) +
    labs(
      title = "Learning Potential: Nearest Neighbor Connections"
    )
  
}


#################
# a. walkability
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
# topic_groups <- c("mobility modes & cycling", "healthy built environment & walkability")

learning_data_walk <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, topic_groups, distance_based_exclusion = T
)

map_learning_potential_walk <- plot_learning_potential(
  edges = learning_data_walk$edges,
  cities = learning_data_walk$cities,
  world = world,
  bb = bb
)
map_learning_potential_walk
ggsave(map_learning_potential_walk, file = "plots/map_learning_potential_walk.pdf", width = 10, height = 8)

# Count how many cities learn from each city with the topic
learning_counts <- learning_data_walk$edges %>%
  st_drop_geometry() %>%
  group_by(to_id, panel) %>%
  summarise(n_cities_learning = n()) %>%
  group_by(panel) %>% 
  arrange(desc(n_cities_learning), .by_group = T) %>% 
  slice(1:20)

learning_counts_named <- learning_counts %>%
  left_join(ghsl %>% distinct(ID_UC_G0, GC_UCN_MAI_2025), by = c("to_id" = "ID_UC_G0"))

bar_learning_potential_walk <- ggplot(learning_counts_named, aes(x = reorder(GC_UCN_MAI_2025, n_cities_learning), y = n_cities_learning)) +
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  coord_flip() +
  facet_wrap(~panel, scales = "free_y") +
  labs(
    title = "Number of Cities That Can Learn from Each City (Mobility modes and walkabilty)",
    x = "City with Topic",
    y = "Number of Learning Cities"
  ) +
  theme_SM()
ggsave(bar_learning_potential_walk, file = "plots/bar_learning_potential_walk.pdf", width = 10, height = 6)

#################
# a. build
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
topic_groups <- c("wall efficiency, retrofit, insulation", "passive cooling & load reduction", "Green roof benefits and applications")

learning_data <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, topic_groups, distance_based_exclusion = T
)

map_learning_potential_build <- plot_learning_potential(
  edges = learning_data$edges,
  cities = learning_data$cities,
  world = world,
  bb = bb
)
map_learning_potential_build
ggsave(map_learning_potential_build, file = "plots/map_learning_potential_builds.pdf")

#################################
#
#################################









library(sf)
library(terra)
library(gstat)
library(dplyr)
library(ggplot2)

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Define Robinson projection
proj_robin <- "+proj=robin +over"

# 1. Prepare input points as `sf` and reproject
ghsl_sf <- ghsl %>%
  st_transform(proj_robin) %>%
  st_centroid() %>%
  left_join(learn_pot %>% dplyr::select(ID_UC_G0, learning_volume_potential), by = "ID_UC_G0") %>%
  filter(!is.na(learning_volume_potential))

# 2. Create a global raster grid in lon/lat and reproject to robinson
global_rast_ll <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, resolution = 1)
crs(global_rast_ll) <- "EPSG:4326"

# 🔍 Project to Robinson
proj_robin <- "+proj=robin +datum=WGS84 +over"
global_rast_robin <- project(global_rast_ll, proj_robin)

# 🔍 Check if projected raster is valid and non-empty
print(global_rast_robin)

# global_rast_robin <- project(global_rast_ll, proj_robin)
grid_points <- as.points(global_rast_robin)

# Convert grid and ghsl points to `sf`
grid_sf <- st_as_sf(grid_points)

# 3. Custom IDW interpolation using sf and dplyr
idw_custom <- function(grid, points, value_col, power = 2, max_neighbors = 10) {
  # Extract coordinates for both sets
  grid_coords <- st_coordinates(grid)
  point_coords <- st_coordinates(points)
  values <- pull(points, {{ value_col }})
  
  # Interpolate manually
  interpolated_vals <- map_dbl(1:nrow(grid_coords), function(i) {
    dists <- sqrt((point_coords[, 1] - grid_coords[i, 1])^2 + (point_coords[, 2] - grid_coords[i, 2])^2)
    dists[dists == 0] <- 1e-12  # avoid division by zero
    nearest <- order(dists)[1:min(max_neighbors, length(dists))]
    weights <- 1 / (dists[nearest]^power)
    sum(weights * values[nearest]) / sum(weights)
  })
  
  grid$prediction <- interpolated_vals
  return(grid)
}

# 4. Run IDW
grid_sf <- idw_custom(grid_sf, ghsl_sf, learning_volume_potential, power = 2)

# 5. Convert to dataframe for plotting
grid_df <- grid_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(prediction = grid_sf$prediction)

# 6. Plot
ggplot() +
  geom_sf(data = st_union(world), fill = "white", color = "black", size = 0.3, alpha = 0.2) +
  geom_raster(data = grid_df, aes(x = X, y = Y, fill = prediction), alpha = 0.9) +
  scale_fill_viridis_c(name = "Learning Potential", na.value = NA) +
  geom_sf(data = bb, fill = NA, color = "grey30") +
  coord_sf(crs = st_crs(proj_robin)) +
  theme_void()

################################################################################
# learning potential based on key-words
################################################################################

generate_learning_potential_data <- function(ghsl, clust_with_topics, clusters, keywords, cluster_list, distance_based_exclusion = T) {
  
  panel_tags <- letters[1:length(cluster_list)]
  all_edges <- list()
  all_cities <- list()
  
  for (i in seq_along(clusters)) {
    
    n_cluster <- clusters[i]
    # topic_of_interest <- keywords
    cluster_label <- paste0("Cluster ", n_cluster)
    topic_label <- paste(keywords, collapse = ", ")
    panel_label <- paste0("(", panel_tags[i], ") ", cluster_label)
    
    cities_classified <- ghsl %>%
      mutate(geom = st_centroid(geom)) %>%
      dplyr::select(ID_UC_G0, geom) %>%
      left_join(clust_with_topics, by = c("ID_UC_G0" = "GHS_urban_area_id")) %>%
      filter(consensus_label_majority == n_cluster) %>%
      group_by(ID_UC_G0, consensus_label_majority) %>%
      summarise(contains_topic = any(str_detect(abstract, str_c(fixed(keywords), collapse = "|")))) %>%
      ungroup()
    
    cities_with <- cities_classified %>% filter(contains_topic)
    cities_without <- cities_classified %>% filter(!contains_topic)
    
    if (nrow(cities_with) == 0 | nrow(cities_without) == 0) next
    
    edges <- tidyr::crossing(
      from = cities_without %>% as.data.frame() %>% dplyr::select(from_id = ID_UC_G0, from_geom = geom),
      to = cities_with %>% as.data.frame() %>% dplyr::select(to_id = ID_UC_G0, to_geom = geom)
    ) %>% unnest()
    
    # if (distance_based_exclusion == T){
    edges <- edges %>%
      mutate(dist = st_distance(from_geom, to_geom, by_element = TRUE)) %>%
      group_by(from_id) %>%
      slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
      ungroup()
    # }
    
    edges_lines <- edges %>%
      rowwise() %>%
      mutate(geometry = st_sfc(
        st_linestring(rbind(st_coordinates(from_geom), st_coordinates(to_geom))),
        crs = st_crs(from_geom))
      ) %>%
      st_as_sf() %>%
      mutate(panel = panel_label)
    
    all_edges[[i]] <- edges_lines
    
    cities_combined <- bind_rows(
      cities_with %>% mutate(type = "With topic"),
      cities_without %>% mutate(type = "Without topic")
    ) %>%
      mutate(panel = panel_label)
    
    all_cities[[i]] <- cities_combined
  }
  
  combined_edges <- bind_rows(all_edges)
  combined_cities <- bind_rows(all_cities)
  
  return(list(edges = combined_edges, cities = combined_cities))
}

plot_learning_potential <- function(edges, cities, world, bb, title) {
  ggplot() +
    geom_sf(data = world %>% st_union(), fill = "grey95", color = NA) +
    geom_sf(data = cities %>% filter(type == "Without topic"), 
            aes(color = type), size = 0.3, alpha = 0.4) +
    geom_sf(data = edges, aes(geometry = geometry), color = "#ffc266", size = 0.3, alpha = 0.3) +
    geom_sf(data = cities %>% filter(type == "With topic"), 
            aes(color = type), size = 0.3, alpha = 0.5, show.legend = FALSE) +
    scale_color_manual(
      values = c("With topic" =  "royalblue1", "Without topic" = "salmon"),
      name = "City Type"
    ) +
    geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
    facet_wrap(~ panel, ncol = 2) +
    theme_void() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8, face = "bold"),
      panel.spacing = unit(0.4, "lines"),
      plot.margin = margin(5, 5, 5, 5),
      text = element_text(size = 8)
    ) +
    labs(
      title = title
    )
  
}

library(stringr)

#################
# a. Compact and Walkable Design
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
keywords <- c("compact city",
              "walkability",
              "walkable neighborhood",
              "high density",
              "mixed use",
              "transit-oriented development",
              "TOD",
              "urban sprawl",
              "active transport",
              "15-minute city")
learning_data_walk <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, keywords, clusters, distance_based_exclusion = T
)

p_learning_map_walk <- plot_learning_potential(
  edges = learning_data_walk$edges,
  cities = learning_data_walk$cities,
  world = world,
  bb = bb,
  "Compact and Walkable Design"
)
p_learning_map_walk
ggsave(p_learning_map_walk, file = "plots/p_learning_map_walk.pdf", width = 10, height = 8)


n_studies_walkable <- clust_with_topics %>% 
  group_by(GHS_urban_area_id, id, consensus_label_majority) %>%
  summarise(contains_topic = any(str_detect(abstract, str_c(fixed(keywords), collapse = "|")))) %>% 
  left_join(ghsl %>% as.data.frame() %>% dplyr::select(ID_UC_G0, GC_UCN_MAI_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  group_by(consensus_label_majority, GHS_urban_area_id, GC_UCN_MAI_2025) %>% 
  summarise(contains_topic = sum(contains_topic, na.rm = T)) %>% 
  group_by(consensus_label_majority) %>% 
  arrange(desc(contains_topic), .by_group = T) %>% 
  slice(1:20) %>% 
  ungroup() 

labels_vec <- setNames(n_studies_walkable$GC_UCN_MAI_2025, n_studies_walkable$GHS_urban_area_id)

p_learning_bar_walk <- n_studies_walkable %>% 
  ggplot(aes(x = reorder(GHS_urban_area_id, contains_topic), y = contains_topic)) +
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  scale_x_discrete(labels = labels_vec) +
  coord_flip() +
  facet_wrap(~consensus_label_majority, scales = "free_y") +
  labs(
    title = "Number of Cities That Can Learn from Each City (Compact and Walkable Design)",
    x = "City with Topic",
    y = "Number of Learning Cities"
  ) +
  theme_SM()
ggsave(p_learning_bar_walk, file = "plots/p_learning_bar_walk.pdf", width = 10, height = 6)

#################
# b. Electrification and Low-Carbon Transport
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
keywords <- c(
  "electric vehicle",
  "electric bus",
  "EV adoption",
  "e-mobility",
  "low-carbon transport",
  "mode shift",
  "public transport electrification",
  "shared mobility",
  "zero-emission vehicle",
  "transport decarbonization"
)
learning_data_electr <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, keywords, clusters, distance_based_exclusion = T
)

p_learning_map_electr <- plot_learning_potential(
  edges = learning_data_electr$edges,
  cities = learning_data_electr$cities,
  world = world,
  bb = bb,
  "Electrification and Low-Carbon Transport"
)
p_learning_map_electr
ggsave(p_learning_map_electr, file = "plots/p_learning_map_electr.pdf", width = 10, height = 8)


n_studies_electr <- clust_with_topics %>% 
  group_by(GHS_urban_area_id, id, consensus_label_majority) %>%
  summarise(contains_topic = any(str_detect(abstract, str_c(fixed(keywords), collapse = "|")))) %>% 
  left_join(ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_UCN_MAI_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  group_by(consensus_label_majority, GHS_urban_area_id, GC_UCN_MAI_2025) %>% 
  summarise(contains_topic = sum(contains_topic, na.rm = T)) %>% 
  group_by(consensus_label_majority) %>% 
  arrange(desc(contains_topic), .by_group = T) %>% 
  slice(1:20) %>% 
  ungroup() 

labels_vec <- setNames(n_studies_electr$GC_UCN_MAI_2025, n_studies_electr$GHS_urban_area_id)

p_learning_bar_electr <- n_studies_electr %>% 
  ggplot(aes(x = reorder(GHS_urban_area_id, contains_topic), y = contains_topic)) +
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  scale_x_discrete(labels = labels_vec) +
  coord_flip() +
  facet_wrap(~consensus_label_majority, scales = "free_y") +
  labs(
    title = "Number of Cities That Can Learn from Each City (Electrification and Low-Carbon Transport)",
    x = "City with Topic",
    y = "Number of Learning Cities"
  ) +
  theme_SM()
ggsave(p_learning_bar_electr, file = "plots/p_learning_bar_electr.pdf", width = 10, height = 6)

#################
# c. Building Efficiency and Low-Carbon Construction
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
keywords <- c(
  "energy-efficient building",
  "building retrofit",
  "passive house",
  "green building",
  "low-carbon material",
  "embodied carbon",
  "net-zero building",
  "building envelope",
  "HVAC efficiency",
  "zero-energy building"
)
learning_data_build <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, keywords, clusters, distance_based_exclusion = T
)

p_learning_map_build <- plot_learning_potential(
  edges = learning_data_build$edges,
  cities = learning_data_build$cities,
  world = world,
  bb = bb,
  "Building Efficiency and Low-Carbon Construction"
)
p_learning_map_build
ggsave(p_learning_map_build, file = "plots/p_learning_map_build.pdf", width = 10, height = 8)


n_studies_build <- clust_with_topics %>% 
  group_by(GHS_urban_area_id, id, consensus_label_majority) %>%
  summarise(contains_topic = any(str_detect(abstract, str_c(fixed(keywords), collapse = "|")))) %>% 
  left_join(ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_UCN_MAI_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  group_by(consensus_label_majority, GHS_urban_area_id, GC_UCN_MAI_2025) %>% 
  summarise(contains_topic = sum(contains_topic, na.rm = T)) %>% 
  group_by(consensus_label_majority) %>% 
  arrange(desc(contains_topic), .by_group = T) %>% 
  slice(1:20) %>% 
  ungroup() 

labels_vec <- setNames(n_studies_build$GC_UCN_MAI_2025, n_studies_build$GHS_urban_area_id)

p_learning_bar_build <- n_studies_build %>% 
  ggplot(aes(x = reorder(GHS_urban_area_id, contains_topic), y = contains_topic)) +
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  scale_x_discrete(labels = labels_vec) +
  coord_flip() +
  facet_wrap(~consensus_label_majority, scales = "free_y") +
  labs(
    title = "Number of Cities That Can Learn from Each City (Building Efficiency and Low-Carbon Construction)",
    x = "City with Topic",
    y = "Number of Learning Cities"
  ) +
  theme_SM()
ggsave(p_learning_bar_build, file = "plots/p_learning_bar_build.pdf", width = 10, height = 6)

#################
# d. Green and Blue Infrastructure
#################

clusters <- sort(unique(cluster_cities$consensus_label_majority))
keywords <- c(
  "green infrastructure",
  "green roof",
  "green wall",
  "urban forest",
  "tree canopy",
  "blue infrastructure",
  "stormwater management",
  "permeable surface",
  "heat island mitigation",
  "nature-based solution"
)
learning_data_blugre <- generate_learning_potential_data(
  ghsl, clust_with_topics, clusters, keywords, clusters, distance_based_exclusion = T
)

p_learning_map_blugre <- plot_learning_potential(
  edges = learning_data_blugre$edges,
  cities = learning_data_blugre$cities,
  world = world,
  bb = bb,
  "Green and Blue Infrastructure"
)
p_learning_map_blugre
ggsave(p_learning_map_blugre, file = "plots/p_learning_map_blugre.pdf", width = 10, height = 8)


n_studies_blugre <- clust_with_topics %>% 
  group_by(GHS_urban_area_id, id, consensus_label_majority) %>%
  summarise(contains_topic = any(str_detect(abstract, str_c(fixed(keywords), collapse = "|")))) %>% 
  left_join(ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_UCN_MAI_2025), by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  group_by(consensus_label_majority, GHS_urban_area_id, GC_UCN_MAI_2025) %>% 
  summarise(contains_topic = sum(contains_topic, na.rm = T)) %>% 
  group_by(consensus_label_majority) %>% 
  arrange(desc(contains_topic), .by_group = T) %>% 
  slice(1:20) %>% 
  ungroup() 

labels_vec <- setNames(n_studies_blugre$GC_UCN_MAI_2025, n_studies_blugre$GHS_urban_area_id)

p_learning_bar_blugre <- n_studies_blugre %>% 
  ggplot(aes(x = reorder(GHS_urban_area_id, contains_topic), y = contains_topic)) +
  geom_bar(stat = "identity", fill = "#963d03", col = "black") +
  scale_x_discrete(labels = labels_vec) +
  coord_flip() +
  facet_wrap(~consensus_label_majority, scales = "free_y") +
  labs(
    title = "Number of Cities That Can Learn from Each City (Green and Blue Infrastructure)",
    x = "City with Topic",
    y = "Number of Learning Cities"
  ) +
  theme_SM()
ggsave(p_learning_bar_blugre, file = "plots/p_learning_bar_blugre.pdf", width = 10, height = 6)











teaching_potential %>% 
  group_by(most_p)
  ggplot(aes(y = research_evenness)) + 
  geom_histogram() + 
  coord_flip() + 
  scale_x_continuous(scales::scientific(1))

ghsl %>% 
  select(ID_UC_G0) %>% 
  mutate(geom = st_centroid(geom)) %>% 
  inner_join(teaching_potential %>% select(ID_UC_G0, LPI_high, LPI), by = c("ID_UC_G0")) %>% 
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA) +  # World map with light gray color
  geom_sf(aes(col = LPI_high, size = LPI), alpha = 0.5) +
  scale_size_continuous(range = c(0.2, 3)) +   
  geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
  theme_SM() + labs(y = "", x = "") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.15, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,0,0,0), "cm")
  ) 

ghsl %>% 
  select(ID_UC_G0) %>% 
  mutate(geom = st_centroid(geom)) %>% 
  inner_join(teaching_potential %>% select(ID_UC_G0, TPI_high, TPI), by = c("ID_UC_G0")) %>% 
  ggplot() +
  geom_sf(data = world %>% st_union(), fill = "grey95", color = NA) +  # World map with light gray color
  geom_sf(aes(col = TPI_high, size = TPI), alpha = 0.2) +
  scale_size_continuous(range = c(0.2, 3)) +  
  geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
  theme_SM() + labs(y = "", x = "") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.15, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,0,0,0), "cm")
  ) 







ipcc_regions_hexa <- st_transform(ipcc_regions_hexa, crs = proj_robin)
n_studies_per_city_all_cities <- st_transform(n_studies_per_city_all_cities, crs = proj_robin)
joined_sf <- st_join(n_studies_per_city_all_cities, ipcc_regions_hexa, join = st_within)


# Create edges from each 'no_studies' city to all 'with_studies' cities in the same cluster
edges <- cities_without_topic %>%
  # filter(consensus_label_majority == 2) %>% 
  inner_join(cities_with_topic, by = "consensus_label_majority", suffix = c("_from", "_to")) %>%
  mutate(edge_id = paste(GHS_urban_area_id_from, GHS_urban_area_id_to, sep = "_")) %>%
  select(edge_id, from = GHS_urban_area_id_from, to = GHS_urban_area_id_to
         # , geom_from = geom_from, geom_to = geom_to
  )


# Optionally, make LINESTRING geometries for visualization
edges_sf <- edges %>%
  rowwise() %>%
  mutate(
    geometry = st_sfc(st_cast(st_union(c(geom_from, geom_to)), "LINESTRING"), crs = st_crs(joined_sf))
  ) %>%
  st_as_sf()

# # Reproject to Mollweide (make sure both are in same CRS)
# clean_places <- st_transform(n_studies_city, 4326)
ghsl <- st_transform(ghsl, proj_robin)

n_studies_per_city_all_cities <- left_join(ghsl, n_studies_per_city, by = c("ID_UC_G0" = "city_id")) %>% 
  mutate(geom = st_centroid(geom)) %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies),
         no_studies = ifelse(n_studies == 0, T, F)) %>% 
  select(n_studies, no_studies, ID_UC_G0) %>% 
  left_join(clust %>% select(consensus_label_majority, GHS_urban_area_id), by = c("ID_UC_G0" = "GHS_urban_area_id"))



ipcc_regions_hexa <- st_transform(ipcc_regions_hexa, crs = proj_robin)
n_studies_per_city_all_cities <- st_transform(n_studies_per_city_all_cities, crs = proj_robin)
joined_sf <- st_join(n_studies_per_city_all_cities, ipcc_regions_hexa, join = st_within)

library(dplyr)
library(sf)

# Assume your dataframe is called 'cities'

# Separate the nodes
nodes_with_studies <- joined_sf %>% 
  as.data.frame() %>% 
  filter(!no_studies) %>% 
  select(consensus_label_majority, ID_UC_G0, geom)

nodes_without_studies <- joined_sf %>% 
  as.data.frame() %>% 
  filter(no_studies) %>% 
  select(consensus_label_majority, ID_UC_G0, geom)

# Create edges from each 'no_studies' city to all 'with_studies' cities in the same cluster
edges <- nodes_without_studies %>%
  filter(consensus_label_majority == 2) %>% 
  inner_join(nodes_with_studies, by = "consensus_label_majority", suffix = c("_from", "_to")) %>%
  mutate(edge_id = paste(ID_UC_G0_from, ID_UC_G0_to, sep = "_")) %>%
  select(edge_id, from = ID_UC_G0_from, to = ID_UC_G0_to
         , geom_from = geom_from, geom_to = geom_to
         )

# Optionally, make LINESTRING geometries for visualization
edges_sf <- edges %>%
  rowwise() %>%
  mutate(
    geometry = st_sfc(st_cast(st_union(c(geom_from, geom_to)), "LINESTRING"), crs = st_crs(joined_sf))
  ) %>%
  st_as_sf()

# Plot
edges_sf  %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry), color = "gray40", linewidth = 0.6, alpha = 0.7) +
  geom_sf(data = joined_sf %>%  filter(consensus_label_majority == 2) %>% filter(!no_studies), 
          aes(geometry = geom), 
          color = "blue", 
          size = 2.5, 
          shape = 21, 
          fill = "lightblue") +
  geom_sf(data = joined_sf %>%  filter(consensus_label_majority == 2) %>% filter(no_studies), 
          aes(geometry = geom), 
          color = "red", 
          size = 2.5, 
          shape = 21, 
          fill = "salmon") +
  labs(title = "Edges from Cities with No Studies to Studied Cities in Same Cluster",
       subtitle = "Blue = Studied Cities, Red = No-Study Cities",
       caption = "Edges represent intra-cluster connections") +
  theme_minimal() +
  theme(legend.position = "none")



# Compute centroids for arrows
centroids <- st_centroid(ipcc_regions_hexa)

# Join 'from' and 'to' coordinates to edges
edges_coords <- edges %>%
  left_join(centroids %>% st_coordinates() %>% 
              bind_cols(id = ipcc_regions_hexa$id), 
            by = c("from" = "id")) %>%
  rename(x_from = X, y_from = Y) %>%
  left_join(centroids %>% st_coordinates() %>%
              bind_cols(id = ipcc_regions_hexa$id), 
            by = c("to" = "id")) %>%
  rename(x_to = X, y_to = Y)

ggplot() +
  geom_sf(data = ipcc_regions_hexa, fill = "grey90", color = "white") +
  geom_curve(data = edges_coords,
             aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
             arrow = arrow(length = unit(0.15, "inches")),
             color = "steelblue", curvature = 0.2, size = 1) +
  coord_sf(crs = st_crs(ipcc_regions_hexa)) +
  theme_SM() + 
  labs(x= "", y = "") + 
  theme(panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "bottom")







# Now 'edges_sf' contains LINESTRINGs connecting relevant cities












edges <- joined_sf %>%
  st_drop_geometry() %>%
  filter(consensus_label_majority == 0) %>%
  summarise(
    zero_ids = list(id[n_studies == 0]),
    nonzero_ids = list(id[n_studies > 0])
  ) %>%
  filter(lengths(zero_ids) > 0 & lengths(nonzero_ids) > 0) %>%
  mutate(edges = map2(zero_ids, nonzero_ids, ~expand.grid(from = .x, to = .y))) %>%
  pull(edges) %>%
  bind_rows() %>% filter(!is.na(from) & !is.na(to) & from != to)


# Compute centroids for arrows
centroids <- st_centroid(ipcc_regions_hexa)

# Join 'from' and 'to' coordinates to edges
edges_coords <- edges %>%
  left_join(centroids %>% st_coordinates() %>% 
              bind_cols(id = ipcc_regions_hexa$id), 
            by = c("from" = "id")) %>%
  rename(x_from = X, y_from = Y) %>%
  left_join(centroids %>% st_coordinates() %>%
              bind_cols(id = ipcc_regions_hexa$id), 
            by = c("to" = "id")) %>%
  rename(x_to = X, y_to = Y)


ggplot() +
  geom_sf(data = ipcc_regions_hexa, fill = "grey90", color = "white") +
  geom_curve(data = edges_coords,
             aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
             arrow = arrow(length = unit(0.15, "inches")),
             color = "steelblue", curvature = 0.2, size = 1) +
  coord_sf(crs = st_crs(ipcc_regions_hexa)) +
  theme_SM() + 
  labs(x= "", y = "") + 
  theme(panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "bottom")

td_gammas <- readRDS("data/topic_model/td_gammas.rds")
out_t <- readRDS("data/topic_model/out_t.rds")

n_topics = 220
main_topics <- td_gammas %>%
  dplyr::filter(K == n_topics) %>%
  unnest(model) %>% 
  arrange(document) %>% 
  arrange(document, -gamma) %>% 
  group_by(document) %>% 
  slice(1) 


main_topics




df



























ggplot(ipcc_regions_hexa) +
  geom_sf(fill = NA) +
  geom_label(
    aes(label = label, geometry = geom),
    stat = "sf_coordinates", alpha=.5, size = 2, label.size = NA
  ) + 
  geom_sf(data = ipcc_regions_hexa_groupings) +
  geom_label(
    data = ipcc_regions_hexa_groupings,
    aes(label = Name, x = auxiliary_storage_labeling_positionx, y = auxiliary_storage_labeling_positiony),
    alpha=.5, size = 2, label.size = NA
  ) + 
  theme_SM() + 
  labs(x= "", y = "") + 
  theme(panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())




################################################################################
# pies on a map by region
################################################################################

# Assuming desc_geo is a dataframe with lat/long and consensus_label_majority
desc_geo_sf <- st_as_sf(desc_geo, coords = c("longitude", "latitude"), crs = proj_robin)

# Assuming ipcc_regions is an sf object with the polygons of the regions
desc_geo_intersect <- st_join(ipcc_regions, desc_geo_sf )

# reg_df <- as.data.frame(desc_geo_intersect)[,c("Africa","Asia", "Europe", "North.America", "Oceania", "South.America")]
# # Summarize the data by region, assuming the region column is called 'region_id'
# region_summary <- desc_geo_intersect %>%
#   # as.data.frame() %>% 
#   mutate(region = max.col(reg_df, "first")) %>% 
#   group_by(region, consensus_label_majority) %>%
#   summarise(total_y_pred = sum(consensus_label_majority, na.rm = TRUE)) %>%
#   ungroup()
# 
ipcc_centroids <- st_centroid(ipcc_regions)
# 
# # Create a new column for the share of each region's 'consensus_label_majority'
# region_summary <- region_summary %>%
#   group_by(region) %>%
#   mutate(share = total_y_pred / sum(total_y_pred))
# 
# region_summary
# Merge the region summary with centroids
region_summary <- st_join(ipcc_centroids, region_summary)
region_summary$share

library(scatterpie)
ggplot() +
  # geom_sf(data = ipcc_regions, fill = "lightblue", color = "black") +
  # geom_sf(data = region_summary, aes(geometry = geometry, fill = factor(region_id))) +
  geom_scatterpie(aes(x = st_coordinates(geometry)[, 1], 
                      y = st_coordinates(geometry)[, 2], 
                      r = 0.05,  # Adjust radius based on your data
                      cols = share, 
                      ), 
                  data = region_summary,
                  long_format=F,
                  color = "white") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Pie Charts Showing the Share of consensus_label_majority by Region")
