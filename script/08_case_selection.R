
R.version
# _                           
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
library(arrow)
library(rnaturalearth)
library(showtext)
library(scales)
library(ggpubr)
library(openxlsx2)

##########################
# load and transform data
##########################

# ghsl + clustering data
clust <- read.csv("data/clustering_results/dec_clusters_k4.csv") %>% as_tibble()
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
ghsl_clean <- read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet")

# world data
world <- ne_countries(scale = "medium", returnclass = "sf")
bb <- ne_download(type = "wgs84_bounding_box", category = "physical", returnclass = "sf") 

# IPCC data
cites_ipcc_regions <- read.csv("data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv")  %>% as_tibble()
ipcc_cont <- st_read("data/IPCC-WGII-continental-regions_shapefile")

# studies per city
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv") %>% as_tibble()

# OpenAlex data
file_names <- list.files(
  path = "/Users/simon/Documents/repo/cities-learning/data/OpenAlex/05_deduplicated",
  pattern = "^city_works_df_NA_abstr_added_dedup_\\d+\\.csv$",
  full.names = TRUE
)
df_list <- lapply(file_names, read.csv)
oa <- do.call(rbind, df_list)

# project to robinson
proj_robin <- "+proj=robin"
ghsl <- st_transform(ghsl, proj_robin)
ipcc_cont <- st_transform(ipcc_cont, proj_robin)
bb <- st_transform(bb, proj_robin)
world <- st_transform(world, proj_robin)

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Mitigation first",
    "Mega all in",
    "Development first", 
    "Urban planning first"
  )) %>% 
  mutate(cluster_name = factor(cluster_name, levels = c("Development first",
                                                        "Mitigation first",
                                                        "Urban planning first",
                                                        "Mega all in")))

##########################
# custom plotting theme 
##########################

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

##########################
# rename ids for easy joins
##########################

ghsl <- ghsl %>% rename(city_id = ID_UC_G0)
ghsl_clean <- ghsl_clean %>% rename(city_id = GHS_urban_area_id)
clust <- clust %>% rename(city_id = GHS_urban_area_id)
cites_ipcc_regions <- cites_ipcc_regions %>% rename(city_id = ID_UC_G0) %>% select(-X)

ghsl <- ghsl %>% 
  mutate(geom = st_centroid(geom))

##########################
# studies per city
##########################

clean_places <- clean_places %>% 
  filter(!is.na(city_intersection_id) & !is.na(city_word_match_id)) %>%
  filter(id %in% oa$id) %>% # only deduplicated count
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id)

n_studies_per_city <- clean_places %>% 
  group_by(city_id) %>% 
  summarise(n_studies = n())

####################################################
# case selection: prepare data
####################################################

clust_stud_pop <- clust %>% 
  dplyr::select(city_id, consensus_label_majority, starts_with("mean_prob_cluster_")) %>%
  pivot_longer(
    cols = starts_with("mean_prob_cluster_"),
    names_to = "cluster",
    values_to = "probability"
  ) %>%
  mutate(cluster = as.numeric(gsub("mean_prob_cluster_", "", cluster))) %>% 
  filter(consensus_label_majority == cluster) %>% 
  select(city_id, consensus_label_majority, probability) %>% 
  left_join(ghsl %>% as_tibble() %>% select(city_id, GC_POP_TOT_2025, GC_UCN_MAI_2025, GC_CNT_GAD_2025), by = "city_id") %>% 
  left_join(n_studies_per_city, by = "city_id") %>% 
  left_join(cluster_names, by = "consensus_label_majority") %>% 
  mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)) %>% 
  rename(pop = GC_POP_TOT_2025,
         city_name = GC_UCN_MAI_2025,
         country = GC_CNT_GAD_2025) %>% 
  mutate(pop = round(pop/1000000,3)) %>% 
  left_join(cites_ipcc_regions, by = "city_id")

write.csv(clust_stud_pop, "data/clustering_results/cities_by_regional_type_clean.csv")

################################################################################
# overall over and under-researched by cluster
################################################################################

shares_long <- clust_stud_pop %>%
  group_by(Region, consensus_label_majority) %>%
  summarise(
    total_studies = sum(n_studies, na.rm = TRUE),
    n_cities = n_distinct(city_id),
    total_pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    research_share = total_studies / sum(total_studies),
    city_share = n_cities / sum(n_cities),
    pop_share = total_pop / sum(total_pop),
    research_to_city_ratio = research_share / city_share,
    research_to_pop_ratio = research_share / pop_share
  ) %>%
  ungroup() %>%
  select(Region, consensus_label_majority, research_to_city_ratio, research_to_pop_ratio) %>%
  pivot_longer(cols = starts_with("research_to_"), names_to = "metric", values_to = "value")


p_over_under <- shares_long %>% 
  mutate(value = ifelse(is.infinite(value) | value == 0, NA, value)) %>% 
  ggplot(aes(x = factor(consensus_label_majority), y = value, color = metric, shape = metric)) +
  geom_hline(yintercept = 1, lty = 3, col = "grey50") +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#ccaf81", "#963d03")) +
  scale_shape_manual(values = c(1, 17)) +
  facet_grid( ~ Region) +
  labs(
    x = "Cluster",
    y = "Normalized Research Share",
    color = "Cluster",
    shape = "Cluster",
    title = "Normalized Research Share vs. City and Population Shares by Region and Cluster"
  ) +
  theme_SM() +
  scale_y_continuous(
    trans = "log2",
    breaks = c(0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
    labels = c("1/32", "1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16", "32"),
    # limits = c(0.0125, 32)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_over_under
ggsave(p_over_under, file = "plots/p_over_under.pdf", width = 10, height = 5)

################################################################################
# case selection: functions
################################################################################

###### a) cites, studies, and population by cluster and region
plot_cluster_region_heatmap <- function(data, value_col, fill_label = NULL, title = NULL, percent = FALSE, round = FALSE, low = "#fff1cc", high = "#963d03") {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  value_col_sym <- rlang::ensym(value_col)
  
  lvs_cl <- c(levels(data$cluster_name), "Total")
  lvs_reg <- rev(c("North America", "South America", "Europe", "Africa", "Asia", "Australasia", "Small Islands", "Total"))
  
  # Base data
  heatmap_data <- data %>%
    group_by(Region, cluster_name) %>%
    summarise(value = sum(!!value_col_sym, na.rm = TRUE), .groups = "drop") %>%
    mutate(cluster_name = as.character(cluster_name))
  
  # Totals
  row_totals <- heatmap_data %>%
    group_by(Region) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(cluster_name = "Total")
  
  col_totals <- heatmap_data %>%
    group_by(cluster_name) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Region = "Total")
  
  # Combine
  heatmap_data_totals <- bind_rows(heatmap_data, row_totals, col_totals)
  
  # Percentage transformation
  if (percent) {
    grand_total <- sum(heatmap_data$value, na.rm = TRUE)
    
    heatmap_data_totals <- heatmap_data_totals %>%
      mutate(
        value_pct = 100 * value / grand_total,
        label = sprintf("%.1f%%", value_pct),
        fill_value = value_pct
      )
  } else {
    heatmap_data_totals <- heatmap_data_totals %>%
      rowwise() %>%
      mutate(
        label = value,
        label = ifelse(round == T, round(label, 1), label),
        fill_value = value
      )
  }
  
  # Set factor levels
  heatmap_data_totals <- heatmap_data_totals %>%
    mutate(
      cluster_name = factor(cluster_name, levels = lvs_cl),
      Region = factor(Region, levels = lvs_reg)
    )
  
  # Line positions
  row_line <- 1.5
  col_line <- 4.5

  print(heatmap_data_totals %>%
          select(Region, cluster_name, label) %>% 
          pivot_wider(names_from = cluster_name, values_from = label))

  # Plot
  ggplot(heatmap_data_totals, aes(x = cluster_name, y = Region, fill = sqrt(fill_value))) +
    geom_tile(color = "white", width = .95, height = .95) +
    geom_text(aes(label = label), color = "black", size = 3) +
    scale_fill_gradient(
      low = low, high = high,
      name = fill_label %||% if (percent) "% of Total" else rlang::as_label(value_col_sym)
    ) +
    labs(
      x = "",
      y = "",
      title = title %||% paste0("Heatmap of ", rlang::as_label(value_col_sym), " by Cluster and Region")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = row_line, color = "grey20", size = 0.6) +
    geom_vline(xintercept = col_line, color = "grey20", size = 0.6)
}


sample_cities_by_share <- function(data, 
                                   n_total_cities = 100, 
                                   share_type = c("pop", "n_studies", "cities"), 
                                   seed = NULL) {
  
  share_type <- match.arg(share_type)
  
  # Step 1: Aggregate by cluster and region
  share_data <- data %>%
    group_by(consensus_label_majority, Region)
  
  share_data <- switch(
    share_type,
    "pop" = share_data %>%
      summarise(share_value = sum(pop, na.rm = TRUE), .groups = "drop"),
    "n_studies" = share_data %>%
      summarise(share_value = sum(n_studies, na.rm = TRUE), .groups = "drop"),
    "cities" = share_data %>%
      summarise(share_value = n_distinct(city_id), .groups = "drop")
  )
  
  print("=== Population shares: ===")
  print(share_data %>% 
          group_by(consensus_label_majority) %>% 
          summarise(share_value = sum(share_value)) %>% 
          mutate(share_value = share_value/sum(share_value)))
  
  # Step 2: Compute region totals and identify small-share regions (<1%)
  region_totals <- share_data %>%
    group_by(Region) %>%
    summarise(region_value = sum(share_value), .groups = "drop") %>%
    mutate(region_share = region_value / sum(region_value),
           is_small = region_share < 0.01)
  
  print("=== Small regions identified (under 1% share): ===")
  print(region_totals %>% filter(is_small))
  
  # Step 3: Assign exactly 5 cities to each small region, distributed across clusters
  small_alloc_detailed <- share_data %>%
    semi_join(region_totals %>% filter(is_small), by = "Region") %>%
    group_by(Region) %>%
    mutate(share_within_region = share_value / sum(share_value)) %>%
    mutate(base_n = floor(5 * share_within_region),
           remainder = 5 * share_within_region - base_n) %>%
    arrange(desc(remainder)) %>%
    mutate(extra = if_else(row_number() <= 5 - sum(base_n), 1, 0),
           n_cities_sample = base_n + extra) %>%
    ungroup() %>%
    select(-base_n, -remainder, -extra, -share_within_region)
  
  print("=== Allocations for small-share regions: ===")
  print(small_alloc_detailed %>% group_by(Region) %>% summarise(n = sum(n_cities_sample)))
  
  # Step 4: Remaining cities to allocate
  n_allocated_small <- sum(small_alloc_detailed$n_cities_sample)
  n_remaining <- n_total_cities - n_allocated_small
  
  print(paste("Cities allocated to small regions:", n_allocated_small))
  print(paste("Remaining cities to allocate:", n_remaining))
  
  # Step 5: Allocate remaining cities to non-small regions
  share_data_rest <- share_data %>%
    anti_join(small_alloc_detailed, by = c("Region", "consensus_label_majority"))
  
  # Calculate actual population per cluster-region in full data
  pop_per_group <- data %>%
    semi_join(share_data_rest, by = c("Region", "consensus_label_majority")) %>%
    group_by(consensus_label_majority, Region) %>%
    summarise(group_pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  print("=== Population per region cluster cell: ===")
  print(pop_per_group)
  
  total_pop_rest <- sum(pop_per_group$group_pop)
  
  share_data_rest <- share_data_rest %>%
    left_join(pop_per_group, by = c("Region", "consensus_label_majority")) %>%
    mutate(pop_share = group_pop / total_pop_rest,
           base_n = floor(n_remaining * pop_share),
           remainder = n_remaining * pop_share - base_n)
  
  n_missing <- n_remaining - sum(share_data_rest$base_n)
  
  share_data_rest <- share_data_rest %>%
    arrange(desc(remainder)) %>%
    mutate(extra = if_else(row_number() <= n_missing, 1, 0),
           n_cities_sample = base_n + extra) %>%
    select(-base_n, -remainder, -extra, -pop_share)
  
  print("=== Allocations for large-share regions: ===")
  print(share_data_rest %>% group_by(Region) %>% summarise(n = sum(n_cities_sample)) %>% mutate(share = n/sum(n)))
  
  # Step 6: Combine allocations
  final_allocations <- bind_rows(small_alloc_detailed, share_data_rest)
  
  print("=== Final allocation by region: ===")
  print(final_allocations %>% group_by(Region) %>% summarise(total = sum(n_cities_sample)))
  
  # Step 7: Sample cities
  sampled_data <- data %>%
    inner_join(final_allocations, by = c("consensus_label_majority", "Region")) %>%
    group_by(consensus_label_majority, Region) %>%
    arrange(probability, .by_group = TRUE) %>%
    mutate(row = row_number()) %>%
    filter(row <= n_cities_sample) %>%
    ungroup() %>%
    select(-row)
  
  # Step 8: Ensure exactly n_total_cities are sampled
  sampled_data <- sampled_data %>%
    distinct(city_id, .keep_all = TRUE)
  
  actual_n <- nrow(sampled_data)
  
  if (actual_n < n_total_cities) {
    remaining_needed <- n_total_cities - actual_n
    
    print(paste("Topping up", remaining_needed, "cities due to shortfall..."))
    
    additional_data <- data %>%
      filter(!city_id %in% sampled_data$city_id) %>%
      arrange(probability) %>%
      slice_head(n = remaining_needed)
    
    sampled_data <- bind_rows(sampled_data, additional_data)
  }
  
  return(sampled_data %>%
           select(city_id, city_name, country, Region,
                  consensus_label_majority, pop, n_studies, everything()))
}


plot_sample_vs_full_pop_share <- function(sampled_data, full_data, title) {
  
  # 1. Compute population share in sampled cities
  sampled_region_share <- sampled_data %>%
    group_by(Region) %>%
    summarise(sampled_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
    mutate(sampled_share = sampled_pop / sum(sampled_pop))
  
  # 2. Compute population share in full data
  full_region_share <- full_data %>%
    group_by(Region) %>%
    summarise(full_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
    mutate(full_share = full_pop / sum(full_pop))
  
  # 3. Join and compute ratio
  comparison <- left_join(sampled_region_share, full_region_share, by = "Region") %>%
    mutate(ratio = sampled_share / full_share)
  
  # 4. Compute RMSE
  rmse <- sqrt(mean((comparison$sampled_share - comparison$full_share)^2, na.rm = TRUE))
  
  # 5. Plot log2 ratio with RMSE annotation
  ggplot(comparison, aes(x = reorder(Region, ratio), y = ratio)) +
    geom_point(size = 4, color = "steelblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_y_continuous(trans = "log2", labels = label_number(accuracy = 0.01)) +
    labs(
      title = title,
      subtitle = paste0("RMSE = ", round(rmse, 4)),
      x = "Region",
      y = "log2(Sampled Share / Full Share)"
    ) +
    # ylim(c(.15,4.5)) +
    theme_SM()
}


################################################################################
# case selection: compute all stats 
################################################################################

# 1) Overall distribution of population, population share, cities and studies

# Population
p_pop_per_cluster_region <- plot_cluster_region_heatmap(
  clust_stud_pop, pop, round = T,
  title = "Population (in m)")

# Population share
p_pop_share_per_cluster_region <- plot_cluster_region_heatmap(
  clust_stud_pop, pop, 
  title = "Population share", 
  percent = T)

# Plot using `n_cities`
p_cities_per_cluster_region <- plot_cluster_region_heatmap(
  clust_stud_pop %>% mutate(city_yes = 1), 
  city_yes, 
  title = "Cities")

# Plot using `n_studies`
p_studies_per_cluster_region <- plot_cluster_region_heatmap(
  clust_stud_pop, 
  n_studies, 
  title = "Studies")


# 2) Cities and studies in the selection -- get 300 representative cities and refine assignment

# get cities
sampled_cities <- sample_cities_by_share(clust_stud_pop, 500, "pop")


# cities covered
p_population_selected_covered_cities_per_cluster_region <- plot_cluster_region_heatmap(
  sampled_cities %>% mutate(city_yes = 1), 
  city_yes, 
  title = "Selected cities",
  low = "#deebf7",
  high = "#08519c")
# studies covered
p_population_selected_covered_studies_per_cluster_region <- plot_cluster_region_heatmap(
  sampled_cities, 
  n_studies, 
  title = "Selected studies",
  low = "#deebf7",
  high = "#08519c")

p_selection <- ggarrange(
  p_pop_share_per_cluster_region, p_pop_per_cluster_region, 
  p_cities_per_cluster_region, p_studies_per_cluster_region, 
  p_population_selected_covered_cities_per_cluster_region,
  p_population_selected_covered_studies_per_cluster_region,
  nrow = 3,
  ncol = 2,
  labels = "auto")
ggsave(p_selection, file = "plots/p_selection.pdf", width = 10, height = 10)

# analyse biases
p_bias <- plot_sample_vs_full_pop_share(sampled_cities, clust_stud_pop, title = "Before iterative population-based adjustment")
ggsave(p_bias, file = "plots/p_selection_bias.pdf", width = 10, height = 4)

p_selection_map <- ghsl %>%
  select(city_id) %>%
  left_join(clust_stud_pop) %>%
  left_join(sampled_cities %>% mutate(sampled_yes = "In the sample") %>% select(city_id, sampled_yes)) %>%
  mutate(sampled_yes = ifelse(is.na(sampled_yes), "Not in the sample", sampled_yes)) %>%
  mutate(sampled_yes = factor(
    sampled_yes,
    levels = c("Not in the sample", "In the sample")
  )) %>% 
  ggplot() +
  geom_sf(data = world %>% st_union(), col = "white") +
  geom_sf(aes(geometry = geom,
              col = sampled_yes, fill = sampled_yes, alpha = sampled_yes), 
          lwd = 0, 
          size = .5
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  ggrepel::geom_label_repel(
    data = ~ subset(., sampled_yes == "In the sample") %>% 
      group_by(consensus_label_majority, Region) %>% 
      arrange(probability, .by_group = T) %>% 
      slice(1:5),
    aes(label = city_name, geometry = geom),
    stat = "sf_coordinates",
    alpha = 0.5, size = 2, max.overlaps = 300
  ) +
  scale_color_manual(values = c( "#ffe0a3", "black")) +
  labs(col = "Case selection", fill = "Case selection", alpha = "Case selection") +
  # scale_color_gradient2(low="white", mid="#ffe0a3", high="#963d03",
  #                       limits = c(0, 1), oob = scales::squish) +
  scale_size_continuous(range = c(.05, 1)) +
  facet_wrap(~cluster_name) +
  geom_sf(data = bb, col = "grey40", fill = "transparent", linewidth = 1) +
  theme_SM() + labs(y = "", x = "") +
  theme(legend.position = c(.57,.44),
        legend.title = element_text(),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.y = element_blank(),
        text = element_text(size = 8),
        panel.spacing = unit(-0.15, "lines"),
        panel.border = element_blank(),
        plot.margin = margin(c(-1,0,0,0), "cm")
  )
ggsave(p_selection_map, file = "plots/p_selection_map.pdf", width = 10, height = 5.8)

sampled_cities


# Panel A: Distribution of n_studies across world regions
p_a <- ggplot(sampled_cities, aes(x = Region, y = n_studies)) +
  geom_boxplot(fill = "#3182bd", alpha = 0.7, outliers = FALSE) +
  labs(title = "Distribution of Studies by Region",
       x = "", y = "Number of Studies") +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Panel B: Distribution of n_studies across cluster_name (city types)
p_b <- ggplot(sampled_cities, aes(x = cluster_name, y = n_studies)) +
  geom_boxplot(fill = "#3182bd", alpha = 0.7, outliers = FALSE) +
  labs(title = "Distribution of Studies by City Type",
       x = "", y = "Number of Studies") +
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_selection_n_studies <- ggarrange(p_a, p_b, labels = "auto", align = "h", widths = c(7,4))
ggsave(p_selection_n_studies, file = "plots/p_selection_n_studies.pdf", width = 10, height = 5)



################################################################################
# store case selection
################################################################################

# Create a new workbook
wb <- wb_workbook()

# First sheet: Case selection
wb$add_worksheet("Case selection")
wb$add_data(
  sheet = "Case selection",
  x = sampled_cities %>%
    left_join(clust_stud_pop) %>%
    select(
      city_id, city_name, country, region = Region,
      city_type = cluster_name, number_of_studies = n_studies
    ) %>% 
    arrange(city_type, region, -number_of_studies)
)

wb$save("data/case_selection/case_selection_and_literature.xlsx", overwrite = TRUE)
