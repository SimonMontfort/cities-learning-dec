
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

setwd("/Users/simon/Documents/repo/cities-learning-dec/robustness/appraisal")

library(sf)
library(tmap)
library(raster)
library(geosphere)
library(tmap)
library(units)
library(rmapshaper)
library(dplyr)

##########################
# load and transform data
##########################

ipcc_cont <- st_read("data/IPCC-WGII-continental-regions_shapefile")
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")

proj_robin <- "+proj=robin"
ghsl <- st_transform(ghsl, proj_robin)
ipcc_cont <- st_transform(ipcc_cont, proj_robin)

##########################
# make valid polygons
##########################

ipcc_cont$geometry[6] <- st_make_valid(ipcc_cont[6, ])$geometry 
ipcc_cont <- st_as_sf(ipcc_cont)

ipcc_cont <- ipcc_cont %>% 
  mutate(Region = ifelse(Region == "Central and South America", "South America", Region))

ipcc_cont_split <- ipcc_cont %>%
  filter(Region == "Small Islands") %>%
  st_cast("POLYGON") %>%
  rowwise() %>%
  mutate(max_lat = max(st_coordinates(geometry)[,1])) %>%  # latitude is Y
  ungroup() %>%
  arrange(desc(max_lat)) %>%  # highest latitude first
  mutate(poly_id = row_number()) %>%
  dplyr::select(-max_lat)  # remove helper column if not needed

tm_shape(ipcc_cont_split) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(1:10))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(11))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(12))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(13))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(14))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(15))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(17))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(18))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(19:21))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(22:26))) + tm_polygons()
tm_shape(ipcc_cont_split %>% filter(poly_id %in% c(27))) + tm_polygons()

ipcc_cont_split <- ipcc_cont_split %>%
  mutate(Region = ifelse(poly_id == 12, "Africa", Region), # Madagascar
         Region = ifelse(poly_id == 13, "South America", Region), # Falkland
         Region = ifelse(poly_id == 15, "North America", Region), # Falkland
  )

ipcc_cont <- ipcc_cont_split %>%
  bind_rows(ipcc_cont %>% filter(Region != "Small Islands")) %>% 
  group_by(Region) %>% 
  summarise(geometry = st_union(geometry))

st_write(ipcc_cont, "data/IPCC-WGII-continental-regions_shapefile/IPCC-WGII-continental-regions_shapefile_clean.shp", delete_dsn = TRUE)

##########################
# look at the data
##########################

# 1. Join and compute centroid
joined_data <- ghsl %>%
  mutate(geom = st_centroid(geom)) %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>%
  st_join(ipcc_cont) %>%
  mutate(region_missing = is.na(Region))  # TRUE for NAs

table(joined_data$region_missing)

joined_data %>% 
  filter(region_missing) %>% 
  as.data.frame() %>% 
  dplyr::select(GC_UCN_MAI_2025, GC_CNT_GAD_2025)

tmap_mode("view")
# 2. Plot with tmap
tm_shape(st_make_valid(ipcc_cont)) +
  tm_polygons(col = "grey90") +
  tm_shape(st_buffer(ipcc_cont, 10000)) +
  tm_polygons(alpha = .2, col = "red") +
  tm_shape(joined_data) +
  tm_dots(
    col = "region_missing",
    palette = c("blue", "red"),  # FALSE = blue (has region), TRUE = red (NA)
    labels = c("Region present", "Region missing"),
    title = "Region assigned?",
    size = 0.1
  ) +
  tm_layout(
    legend.outside = TRUE,
    main.title = "Urban Centers with and without IPCC Region",
    main.title.size = 1.2
  ) + 
  tmap_options(check.and.fix = TRUE)


##########################
# assign to continents
##########################

# Step 1: Compute centroids
ghsl_centroids <- ghsl %>%
  mutate(geom = st_centroid(geom)) %>%
  dplyr::select(ID_UC_G0)

# Step 2: Initial st_join (1-to-1)
initial_join <- st_join(ghsl_centroids, ipcc_cont, left = TRUE)

# Step 3: Identify unmatched cities
unmatched <- initial_join %>%
  filter(is.na(Region)) 

# Step 4: Buffer those unmatched by ~10 km (~0.1 degrees if in lat/lon)
unmatched_buffered <- unmatched %>%
  st_buffer(dist = units::set_units(15000, "m")) 

# Step 5: Try matching again using the buffer (left = FALSE to detect all overlaps)
buffered_matches <- st_join(unmatched_buffered %>% dplyr::select(-Region), ipcc_cont, left = FALSE)

# Step 6: Check for multiple matches
multi_counts <- buffered_matches %>%
  group_by(ID_UC_G0) %>%
  summarise(region_count = n(), .groups = "drop") %>%
  filter(region_count > 1)

multi_counts

# Step 7: Final region assignment: combine initial + buffered (only 1 match per city)

# Keep only cities that got a single match from buffered pass
buffered_single <- buffered_matches %>%
  filter(!(ID_UC_G0 %in% multi_counts$ID_UC_G0)) %>%
  group_by(ID_UC_G0) %>%
  slice(1) %>%  # just in case
  ungroup()

buffered_single

# Merge back with original (filling NAs only)
final <- initial_join %>%
  left_join(
    buffered_single %>% as.data.frame() %>% dplyr::select(ID_UC_G0, Region_buff = Region),
    by = "ID_UC_G0"
  ) %>%
  mutate(
    Region_final = ifelse(is.na(Region), Region_buff, Region)
  )

# Step 8: add SIDS and SM cities 
sids_and_si_city_ids <- read.csv("data/islands/sids_and_si_city_ids.csv")[,-1] 

final$Region_final <- ifelse(final$ID_UC_G0 %in% sids_and_si_city_ids, "Small Islands", final$Region_final)

any(is.na(final$Region_final))

# Step 9: final reporting
# Cities still unmatched after buffer
still_unmatched <- final %>%
  filter(is.na(Region_final)) %>% 
  left_join(ghsl %>% as.data.frame(), by = "ID_UC_G0") %>% 
  as.data.frame() %>% 
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025)

nrow(still_unmatched)
# [1] 28

nrow(multi_counts)
# [1] 4

# Manual region assignments using direct indexing
final$Region_final[final$ID_UC_G0 == 127]  <- "Asia" # Khan Yunis, Palestine
final$Region_final[final$ID_UC_G0 == 333]  <- "Asia" # Dwarka, India
final$Region_final[final$ID_UC_G0 == 410]  <- "Europe" # Odense, Denmark
final$Region_final[final$ID_UC_G0 == 487]  <- "Asia" # Mithapur, India
final$Region_final[final$ID_UC_G0 == 2621] <- "Africa" # Ceuta, Spain 
final$Region_final[final$ID_UC_G0 == 4178] <- "Africa" # Fnideq, Morocco
final$Region_final[final$ID_UC_G0 == 4940] <- "Asia" # Virac, Philippines
final$Region_final[final$ID_UC_G0 == 5266] <- "Asia" # Muntok, Indonesia
final$Region_final[final$ID_UC_G0 == 5487] <- "Europe" # Sirius, Russia
final$Region_final[final$ID_UC_G0 == 5546] <- "Asia" # Pangkalpinang, Indonesia
final$Region_final[final$ID_UC_G0 == 5593] <- "Asia" # Sungailiat, Indonesia
final$Region_final[final$ID_UC_G0 == 5616] <- "Asia" # Toboali, Indonesia
final$Region_final[final$ID_UC_G0 == 7216] <- "Asia" # Lalmohan, Bangladesh
final$Region_final[final$ID_UC_G0 == 7240] <- "Asia" # Tazumoddin, Bangladesh
final$Region_final[final$ID_UC_G0 == 7251] <- "Asia" # Charfasson, Bangladesh
final$Region_final[final$ID_UC_G0 == 7356] <- "Africa" # Hurghada, Egypt
final$Region_final[final$ID_UC_G0 == 7381] <- "Asia" # Hatiya, Bangladesh
final$Region_final[final$ID_UC_G0 == 7602] <- "Asia" # Sampang, Indonesia
final$Region_final[final$ID_UC_G0 == 7642] <- "Asia" # Proppo, Indonesia
final$Region_final[final$ID_UC_G0 == 7674] <- "Asia" # Pamekasan, Indonesia
final$Region_final[final$ID_UC_G0 == 7698] <- "Asia" # Sumenep, Indonesia
final$Region_final[final$ID_UC_G0 == 7705] <- "Asia" # Kalianget, Indonesia
final$Region_final[final$ID_UC_G0 == 7979] <- "Asia" # Gunung Lingkas, Indonesia
final$Region_final[final$ID_UC_G0 == 8332] <- "Asia" # Ambon, Indonesia
final$Region_final[final$ID_UC_G0 == 10095] <- "Asia" # 赵家沟, China
final$Region_final[final$ID_UC_G0 == 11643] <- "Asia" # Zhoushan, China
final$Region_final[final$ID_UC_G0 == 11656] <- "Asia" # Putuo District, China
final$Region_final[final$ID_UC_G0 == 11686] <- "Asia" # Pingtan, China

final$Region_final[final$ID_UC_G0 == 2897] <- "Asia" # Kobani, Syria
final$Region_final[final$ID_UC_G0 == 7356] <- "Africa" # Hurghada, Egypt

any(is.na(final$Region_final))

################################################################################
# checks and final export
################################################################################

final <- final %>%
  as.data.frame() %>%
  dplyr::select(ID_UC_G0, Region = Region_final)

### checks
final_test <- ghsl %>%
  left_join(final %>% as.data.frame())

for (region in unique(final_test$Region)) {
  countries_per_region <- final_test %>%
    filter(Region == region) %>%
    pull(GC_CNT_GAD_2025) %>%
    unique() %>% sort()
  print(
    paste(region)
  )
  print(
    countries_per_region
  )
}

# final_test %>% filter(GC_CNT_GAD_2025 == "Northern Cyprus") %>% pull(Region)
final_test %>% group_by(Region) %>% summarise()

# validate
check_1 <- final %>%
  filter(ID_UC_G0 %in% unmatched$ID_UC_G0) %>%
  left_join(ghsl %>% dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025)) %>%
  dplyr::select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region)

check_1 %>% as.data.frame()

final %>% group_by(Region) %>% count()

write.csv(final, "data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv")

