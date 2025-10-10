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

# packages
library(sf)
library(dplyr)
library(raster)
library(geosphere)
library(tmap)
library(units)
library(rmapshaper)

setwd("/Users/simon/Documents/repo/cities-learning-dec")


##########################
# load data
##########################

# Path to your geodatabase
# gdb_path <- "/Users/simon/Documents/big_data/USGSEsriWCMC_GlobalIslands_v3_mpk/USGSEsriWCMC_GlobalIslands_v3/v108/globalislandsfix.gdb"
# st_layers(gdb_path)
# 
# very_small_islands <- st_read(gdb_path, "USGSEsriWCMC_GlobalIslandsv2_VerySmallIslands")
# small_islands <- st_read(gdb_path, "USGSEsriWCMC_GlobalIslandsv2_SmallIslands")
# big_islands <- st_read(gdb_path, "USGSEsriWCMC_GlobalIslandsv2_BigIslands")
# mainland <- st_read(gdb_path, "USGSEsriWCMC_GlobalIslandsv2_Continents")
# 
# st_write(very_small_islands, "data/islands/global_islands_db/very_small_islands.gpkg", layer_options = c("GEOMETRY_ENCODING=WKB"), delete_dsn = T)
# st_write(small_islands, "data/islands/global_islands_db/small_islands.gpkg", layer_options = c("GEOMETRY_ENCODING=WKB"), delete_dsn = T)
# st_write(big_islands, "data/islands/global_islands_db/big_islands.gpkg", layer_options = c("GEOMETRY_ENCODING=WKB"), delete_dsn = T)
# st_write(mainland, "data/islands/global_islands_db/mainland.gpkg", layer_options = c("GEOMETRY_ENCODING=WKB"), delete_dsn = T)
# 
very_small_islands <- st_read("data/islands/global_islands_db/very_small_islands.gpkg")
small_islands <- st_read("data/islands/global_islands_db/small_islands.gpkg")
big_islands <- st_read("data/islands/global_islands_db/big_islands.gpkg")
mainland <- st_read("data/islands/global_islands_db/mainland.gpkg")

# cities
ghsl <- st_read("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")

# largest very small island is too small for a 50k city
largest_ver_small_island <- very_small_islands %>% st_area() %>% sort() %>% tail(n = 1)/1e6 
smallest_city <- ghsl %>% st_area() %>% sort() %>% head(n= 1)

# largest very small island is 0.07 km2 and a lot smaller than the smallest city -- hence I exclude very_small_islands to make computations faster
largest_ver_small_island < smallest_city

# simplify the geometries
simplify_in_chunks <- function(sf_obj, chunk_size = 5000, keep = 0.05) {
  n <- nrow(sf_obj)
  chunks <- split(sf_obj, rep(1:ceiling(n/chunk_size), each = chunk_size, length.out = n))
  simplified_chunks <- lapply(chunks, function(x) {
    ms_simplify(x, keep = keep, keep_shapes = TRUE)
  })
  do.call(rbind, simplified_chunks)
}

small_islands <- small_islands %>% mutate(area = as.numeric(st_area(Shape))/1e6)
big_islands <- big_islands %>% mutate(area = as.numeric(st_area(Shape))/1e6)

small_islands <- simplify_in_chunks(small_islands, chunk_size = 2000, keep = 0.05)
big_islands <- st_simplify(big_islands, dTolerance = 2000, preserveTopology = TRUE)
mainland <- st_simplify(mainland, dTolerance = 2000, preserveTopology = TRUE)

sf_use_s2(FALSE)
tm_shape(mainland) + tm_polygons(alpha = .5) +
  tm_shape(big_islands) + tm_polygons(alpha = .5, col = "red") 

################################################################################
# robinson projection 
################################################################################
crs_proj <- "+proj=robin"
small_islands <- st_transform(small_islands, crs_proj)
big_islands <- st_transform(big_islands, crs_proj)
mainland <- st_transform(mainland, crs_proj)
ghsl <- st_transform(ghsl, crs_proj)

################################################################################
# islands of interest 
################################################################################

# size
islands <- rbind(big_islands, small_islands) %>% 
  filter(area <= 5000)

mainland_with_big_islands <- bind_rows(mainland,
      big_islands %>% filter(area > 5000))

# isolation
nearest_idx <- st_nearest_feature(islands, mainland_with_big_islands)
distances <- st_distance(islands, mainland_with_big_islands[nearest_idx, ], by_element = TRUE)

isolated_islands <- islands[as.numeric(distances) > 10000, ]
non_isolated_islands <- islands[as.numeric(distances) <= 10000, ]

mainland_with_big_non_isolated_islands <- mainland_with_big_islands %>%
  bind_rows(non_isolated_islands)

################################################################################
# small island cities
################################################################################
# cities intersecting isolated small islands 
idx <- st_intersects(ghsl, isolated_islands, sparse = TRUE)
# cities not intersecting any non-isolated, big islands or mainland  
idy <- st_disjoint(ghsl, mainland_with_big_non_isolated_islands, sparse = TRUE)
cities_on_isolated_island_ids <- ghsl$ID_UC_G0[lengths(idx) > 0 & lengths(idy) == nrow(mainland_with_big_non_isolated_islands)]

# look at an example
tmap_mode("view")
sf_use_s2(FALSE)
tm_shape(ghsl %>%
           mutate(geom = st_centroid(geom)) ) + tm_dots(col = "black") + 
  tm_scale_bar() +
  tm_shape(ghsl %>%
             filter(ID_UC_G0 %in% cities_on_isolated_island_ids) %>% 
             mutate(geom = st_centroid(geom)) ) + tm_dots(col = "yellow") + 
  tm_scale_bar()

not_islands_city_ids <- c(4870, 5060, # Mayanmar
                        6738, 6843, 7251, 7216, 7240, 6996 # Bangladesh
                        )

cities_on_isolated_island_ids <- cities_on_isolated_island_ids[!cities_on_isolated_island_ids %in% not_islands_city_ids]

################################################################################
# SIDS
################################################################################
sids <- read.csv("data/islands/global_islands_db/SIDS/SIDS.csv")
library(countrycode)

sids <- sids %>% 
  mutate(iso3a = countrycode(country, origin = "country.name", destination = "iso3c"))

ghsl <- ghsl %>% 
  mutate(iso3a = countrycode(GC_CNT_GAD_2025, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX", "México" = "MEX")))

sids_city_ids <- ghsl$ID_UC_G0[ghsl$iso3a %in% sids$iso3a]

################################################################################
# final SIDS and SI cities
################################################################################

sids_and_si_city_ids <- sort(unique(c(sids_city_ids, cities_on_isolated_island_ids)))

sids_and_si_city_ids

write.csv(sids_and_si_city_ids, "data/islands/sids_and_si_city_ids.csv")
