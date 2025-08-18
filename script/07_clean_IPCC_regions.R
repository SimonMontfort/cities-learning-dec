
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

library(sf)
library(dplyr)
library(tmap)

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

st_write(ipcc_cont, "data/IPCC-WGII-continental-regions_shapefile/IPCC-WGII-continental-regions_shapefile_clean.shp", delete_dsn = TRUE)

##########################
# look at the data
##########################

# 1. Join and compute centroid
joined_data <- ghsl %>%
  mutate(geom = st_centroid(geom)) %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025) %>%
  st_join(ipcc_cont) %>%
  mutate(region_missing = is.na(Region))  # TRUE for NAs

table(joined_data$region_missing)

joined_data %>% 
  filter(region_missing) %>% 
  as.data.frame() %>% 
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025)

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
  select(ID_UC_G0)

# Step 2: Initial st_join (1-to-1)
initial_join <- st_join(ghsl_centroids, ipcc_cont, left = TRUE)

# Step 3: Identify unmatched cities
unmatched <- initial_join %>%
  filter(is.na(Region)) 

# Step 4: Buffer those unmatched by ~10 km (~0.1 degrees if in lat/lon)
unmatched_buffered <- unmatched %>%
  st_buffer(dist = units::set_units(15000, "m")) 

# Step 5: Try matching again using the buffer (left = FALSE to detect all overlaps)
buffered_matches <- st_join(unmatched_buffered %>% select(-Region), ipcc_cont, left = FALSE)

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
    buffered_single %>% as.data.frame() %>% select(ID_UC_G0, Region_buff = Region),
    by = "ID_UC_G0"
  ) %>%
  mutate(
    Region_final = ifelse(is.na(Region), Region_buff, Region)
  )

# Step 8: Final reporting

# Cities still unmatched after buffer
still_unmatched <- final %>%
  filter(is.na(Region_final)) %>% 
  left_join(ghsl %>% as.data.frame(), by = "ID_UC_G0") %>% 
  as.data.frame() %>% 
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025)

nrow(still_unmatched)
# [1] 96

nrow(multi_counts)
# [1] 4

# Manual region assignments using direct indexing
final$Region_final[final$ID_UC_G0 == 1]    <- "Small Islands" # Apia, Samoa
final$Region_final[final$ID_UC_G0 == 2]    <- "Small Islands" # Nuku'alofa, Tonga
final$Region_final[final$ID_UC_G0 == 4]    <- "Small Islands" # Papeete, French Polynesia
final$Region_final[final$ID_UC_G0 == 16]   <- "South America" # San Andrés, Colombia
final$Region_final[final$ID_UC_G0 == 19]   <- "Small Islands" # Nassau, Bahamas
final$Region_final[final$ID_UC_G0 == 23]   <- "Small Islands" # Oranjestad, Aruba
final$Region_final[final$ID_UC_G0 == 26]   <- "Small Islands" # Willemstad, Curaçao
final$Region_final[final$ID_UC_G0 == 31]   <- "Small Islands" # Fort-de-France, Martinique
final$Region_final[final$ID_UC_G0 == 32]   <- "Small Islands" # Bridgetown, Barbados
final$Region_final[final$ID_UC_G0 == 38]   <- "Small Islands" # Mindelo, Cabo Verde
final$Region_final[final$ID_UC_G0 == 42]   <- "Europe" # Los Cristianos, Spain
final$Region_final[final$ID_UC_G0 == 44]   <- "Europe" # Funchal, Portugal
final$Region_final[final$ID_UC_G0 == 59]   <- "Europe" # St. Helier, Jersey
final$Region_final[final$ID_UC_G0 == 71]   <- "Africa" # São Tomé, São Tomé and Príncipe
final$Region_final[final$ID_UC_G0 == 76]   <- "Africa" # Malabo, Equatorial Guinea
final$Region_final[final$ID_UC_G0 == 87]   <- "Europe" # Valletta, Malta
final$Region_final[final$ID_UC_G0 == 127]  <- "Asia" # Khan Yunis, Palestine
final$Region_final[final$ID_UC_G0 == 148]  <- "Africa" # Moroni, Comoros
final$Region_final[final$ID_UC_G0 == 150]  <- "Africa" # Mamoudzou, Mayotte
final$Region_final[final$ID_UC_G0 == 152]  <- "Asia" # Manama, Bahrain
final$Region_final[final$ID_UC_G0 == 157]  <- "Africa" # Le Port, Réunion
final$Region_final[final$ID_UC_G0 == 159]  <- "Small Islands" # Port Louis, Mauritius
final$Region_final[final$ID_UC_G0 == 165]  <- "Small Islands" # Malé, Maldives
final$Region_final[final$ID_UC_G0 == 191]  <- "Small Islands" # Port Vila, Vanuatu
final$Region_final[final$ID_UC_G0 == 219]  <- "Small Islands" # João Teves, Cabo Verde
final$Region_final[final$ID_UC_G0 == 223]  <- "Europe" # La Orotava, Spain
final$Region_final[final$ID_UC_G0 == 320]  <- "Small Islands" # Fomboni, Comoros
final$Region_final[final$ID_UC_G0 == 327]  <- "Asia" # Le Tampon, Réunion
final$Region_final[final$ID_UC_G0 == 333]  <- "Asia" # Dwarka, India
final$Region_final[final$ID_UC_G0 == 382]  <- "Europe" # Santa Cruz de Tenerife, Spain
final$Region_final[final$ID_UC_G0 == 410]  <- "Europe" # Odense, Denmark
final$Region_final[final$ID_UC_G0 == 481]  <- "Asia" # Saint-Denis, Réunion
final$Region_final[final$ID_UC_G0 == 487]  <- "Asia" # Mithapur, India
final$Region_final[final$ID_UC_G0 == 530]  <- "Europe" # Cuartería El Uno, Spain
final$Region_final[final$ID_UC_G0 == 623]  <- "Europe" # Saint-André, Réunion
final$Region_final[final$ID_UC_G0 == 663]  <- "Small Islands" # Anse-à-Galets, Haiti
final$Region_final[final$ID_UC_G0 == 672]  <- "Europe" # Las Palmas de Gran Canaria, Spain
final$Region_final[final$ID_UC_G0 == 806]  <- "Europe" # Telde, Spain
final$Region_final[final$ID_UC_G0 == 1020] <- "Asia" # Gunungsitoli, Indonesia
final$Region_final[final$ID_UC_G0 == 1245] <- "Asia" # Teluk Dalam, Indonesia
final$Region_final[final$ID_UC_G0 == 2065] <- "Africa" # Hell-Ville, Madagascar
final$Region_final[final$ID_UC_G0 == 2621] <- "Europe" # Ceuta, Spain
final$Region_final[final$ID_UC_G0 == 3158] <- "Asia" # Jeju-si, South Korea
final$Region_final[final$ID_UC_G0 == 3673] <- "Africa" # Zanzibar City, Tanzania
final$Region_final[final$ID_UC_G0 == 4178] <- "Africa" # Fnideq, Morocco
final$Region_final[final$ID_UC_G0 == 4210] <- "Asia" # Jolo, Philippines
final$Region_final[final$ID_UC_G0 == 4366] <- "South America" # Porlamar, Venezuela
final$Region_final[final$ID_UC_G0 == 4396] <- "Asia" # Tanjung Balai Karimun, Indonesia
final$Region_final[final$ID_UC_G0 == 4512] <- "Asia" # Naha, Japan
final$Region_final[final$ID_UC_G0 == 4694] <- "Asia" # Batam City, Indonesia
final$Region_final[final$ID_UC_G0 == 4767] <- "Asia" # Tanjung Piayu, Indonesia
final$Region_final[final$ID_UC_G0 == 4779] <- "Europe" # Ibiza, Spain
final$Region_final[final$ID_UC_G0 == 4807] <- "Asia" # Masbate City, Philippines
final$Region_final[final$ID_UC_G0 == 4838] <- "Asia" # Tanjung Pinang, Indonesia
final$Region_final[final$ID_UC_G0 == 4871] <- "Asia" # Bandar Seri Bentan, Indonesia
final$Region_final[final$ID_UC_G0 == 4940] <- "Asia" # Virac, Philippines
final$Region_final[final$ID_UC_G0 == 4999] <- "Asia" # Dabo, Indonesia
final$Region_final[final$ID_UC_G0 == 5156] <- "Asia" # Tagbilaran, Philippines
final$Region_final[final$ID_UC_G0 == 5161] <- "Europe" # Palma de Mallorca, Spain
final$Region_final[final$ID_UC_G0 == 5266] <- "Asia" # Muntok, Indonesia
final$Region_final[final$ID_UC_G0 == 5487] <- "Europe" # Sirius, Russia
final$Region_final[final$ID_UC_G0 == 5546] <- "Asia" # Pangkalpinang, Indonesia
final$Region_final[final$ID_UC_G0 == 5593] <- "Asia" # Sungailiat, Indonesia
final$Region_final[final$ID_UC_G0 == 5616] <- "Asia" # Toboali, Indonesia
final$Region_final[final$ID_UC_G0 == 6039] <- "Asia" # Tanjung Pandan, Indonesia
final$Region_final[final$ID_UC_G0 == 7216] <- "Asia" # Lalmohan, Bangladesh
final$Region_final[final$ID_UC_G0 == 7240] <- "Asia" # Tazumoddin, Bangladesh
final$Region_final[final$ID_UC_G0 == 7251] <- "Asia" # Charfasson, Bangladesh
final$Region_final[final$ID_UC_G0 == 7356] <- "Asia" # Hurghada, Egypt
final$Region_final[final$ID_UC_G0 == 7381] <- "Asia" # Hatiya, Bangladesh
final$Region_final[final$ID_UC_G0 == 7602] <- "Asia" # Sampang, Indonesia
final$Region_final[final$ID_UC_G0 == 7642] <- "Asia" # Proppo, Indonesia
final$Region_final[final$ID_UC_G0 == 7674] <- "Asia" # Pamekasan, Indonesia
final$Region_final[final$ID_UC_G0 == 7698] <- "Asia" # Sumenep, Indonesia
final$Region_final[final$ID_UC_G0 == 7705] <- "Asia" # Kalianget, Indonesia
final$Region_final[final$ID_UC_G0 == 7867] <- "Asia" # Mataram, Indonesia
final$Region_final[final$ID_UC_G0 == 7888] <- "Asia" # Praya, Indonesia
final$Region_final[final$ID_UC_G0 == 7895] <- "Asia" # Kecamatan Kopang, Indonesia
final$Region_final[final$ID_UC_G0 == 7902] <- "Asia" # Selong, Indonesia
final$Region_final[final$ID_UC_G0 == 7909] <- "Asia" # Kecamatan Pringgabaya, Indonesia
final$Region_final[final$ID_UC_G0 == 7979] <- "Asia" # Gunung Lingkas, Indonesia
final$Region_final[final$ID_UC_G0 == 8230] <- "Asia" # Lewoleba, Indonesia
final$Region_final[final$ID_UC_G0 == 8260] <- "Asia" # Kalabahi, Indonesia
final$Region_final[final$ID_UC_G0 == 8312] <- "Asia" # Sanana, Indonesia
final$Region_final[final$ID_UC_G0 == 8322] <- "Asia" # Labuha, Indonesia
final$Region_final[final$ID_UC_G0 == 8332] <- "Asia" # Ambon, Indonesia
final$Region_final[final$ID_UC_G0 == 8352] <- "Asia" # Tual, Indonesia
final$Region_final[final$ID_UC_G0 == 8372] <- "Asia" # Biak Kota, Indonesia
final$Region_final[final$ID_UC_G0 == 8377] <- "Asia" # Serui Kota, Indonesia
final$Region_final[final$ID_UC_G0 == 10095] <- "Asia" # 赵家沟, China
final$Region_final[final$ID_UC_G0 == 11627] <- "Asia" # Daishan County, China
final$Region_final[final$ID_UC_G0 == 11628] <- "Asia" # Port Blair, India
final$Region_final[final$ID_UC_G0 == 11643] <- "Asia" # Zhoushan, China
final$Region_final[final$ID_UC_G0 == 11656] <- "Asia" # Putuo District, China
final$Region_final[final$ID_UC_G0 == 11683] <- "Asia" # Dongtou, China
final$Region_final[final$ID_UC_G0 == 11686] <- "Asia" # Pingtan, China

final$Region_final[final$ID_UC_G0 == 2897] <- "Asia" # Kobani, Syria
final$Region_final[final$ID_UC_G0 == 7356] <- "Africa" # Hurghada, Egypt

final <- final %>% 
  as.data.frame() %>% 
  select(ID_UC_G0, Region = Region_final)

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

final_test %>% group_by(Region) %>% summarise()

# validate
check <- final %>% 
  filter(ID_UC_G0 %in% unmatched$ID_UC_G0) %>% 
  left_join(ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025)) %>% 
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, Region) 

check %>% as.data.frame()
  

tmap_mode("view")
# 2. Plot with tmap
tm_shape(st_make_valid(ipcc_cont)) +
  tm_polygons(col = "grey90") +
  tm_shape(st_buffer(ipcc_cont, 10000)) +
  tm_polygons(alpha = .2, col = "Region") +
  tm_shape(final_test %>% filter(GC_CNT_GAD_2025 == "El Salvador" & Region == "South America")) +
  tm_dots(size = 0.1) +
  tm_layout(
    legend.outside = TRUE,
    main.title = "Urban Centers with and without IPCC Region",
    main.title.size = 1.2
  ) + 
  tmap_options(check.and.fix = TRUE)



write.csv(final, "data/IPCC-WGII-continental-regions_shapefile/cities_ids_with_ipcc_regions.csv")

