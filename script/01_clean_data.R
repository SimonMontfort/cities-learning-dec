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

setwd("/Users/simon/Documents/repo/cities-learning-dec")

library(arrow)       # For reading/writing parquet files
library(dplyr)       # For data manipulation
library(sf)          # For reading GPKG files
library(readr)       # For reading CSV files
library(caret)       # For scaling/standardization
library(nngeo)       # nearest neighbour

################################################################################
# join data
################################################################################
# Load cities dataset
cities_clean <- read_parquet("data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation.parquet")

# Load GHSL geometry (if needed later)
ghsl <- st_read("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")

# Add precipitation (climate) data
climate_df <- read_csv("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_THEME_CLIMATE_GLOBE_R2024A.csv") %>%
  select(ID_UC_G0, CL_B12_CUR_2010) %>% 
  rename(GHS_precipitation = CL_B12_CUR_2010)


# load additional data
load_data <- function(file_name, value_col, new_name) {
  read_csv(file.path("data/GHS_UCDB_GLOBE_R2024A_V1_0", file_name)) %>%
    select(ID_UC_G0, all_of(value_col)) %>%
    mutate(across(all_of(value_col), as.numeric)) %>% 
    rename(!!new_name := value_col)
}

# === Load datasets with custom names ===
infra  <- load_data("infrastructures.csv", "IN_CIS_ALL_2020", "GHS_critical_infra")
# gender <- load_data("socioeconomic.csv",  "SC_SEC_GDF_2020", "GHS_female_gender_index")
# hdi    <- load_data("socioeconomic.csv",  "SC_SEC_HDI_2020", "GHS_HDI")
# lecz   <- load_data("exposure.csv",       "EX_L10_B23_2025", "GHS_builtup_below_10m")
green  <- load_data("greenness.csv",      "GR_AVG_GRN_2025", "GHS_greenness_index")

# === Check for missing values ===
sum(is.na(infra$GHS_critical_infra))
sum(is.na(green$GHS_greenness_index))
# two missing values for greennesss,

################################################################################
# impute the 2 NAs
################################################################################

# 1. Join green to ghsl by ID_UC_G0
ghsl_green <- ghsl %>%
  left_join(green, by = "ID_UC_G0") %>%
  mutate(geom = st_centroid(geom))  # Ensure centroids

# 2. Separate missing and non-missing
missing_pts <- ghsl_green %>% filter(is.na(GHS_greenness_index))
non_missing_pts <- ghsl_green %>% filter(!is.na(GHS_greenness_index))

# 3. Find nearest non-missing city for each missing city
nearest_idx <- st_nearest_feature(missing_pts, non_missing_pts)

# 4. Get corresponding values
imputed_values <- non_missing_pts$GHS_greenness_index[nearest_idx]

which(is.na(green$GHS_greenness_index))
which(is.na(ghsl_green$GHS_greenness_index))

# 5. Update ghsl_green
green$GHS_greenness_index[is.na(green$GHS_greenness_index)] <- imputed_values

# Check result
if (sum(is.na(green$GHS_greenness_index)) == 0){
  print("all NAs have been imputed")
} 

################################################################################
# join data
################################################################################

# Merge with cities_clean
cities_clean <- cities_clean %>%
  left_join(climate_df, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(infra, by = c("GHS_urban_area_id" = "ID_UC_G0")) %>% 
  left_join(green, by = c("GHS_urban_area_id" = "ID_UC_G0")) 

# Add continent dummies
cities_clean <- cities_clean %>%
  mutate(
    `North America` = as.integer(continent == "North America"),
    `South America` = as.integer(continent == "South America"),
    Europe = as.integer(continent == "Europe"),
    Africa = as.integer(continent == "Africa"),
    Asia = as.integer(continent == "Asia"),
    Oceania = as.integer(continent %in% c("Oceania", "Australia"))
  )

# Select variables for scaling
variables <- c(
  "GHS_population", "GHS_population_growth",
  "GHS_population_density", "GHS_population_density_growth",
  "GHS_GDP_PPP", "GHS_GDP_PPP_growth", "GHS_critical_infra", "GHS_greenness_index", 
  "GHS_precipitation", "hdd", "cdd"
)

cities_clean_sub <- cities_clean %>%
  select(all_of(variables), GHS_urban_area_id)

# check that no NAs are in the data
cities_clean_sub %>% 
  summarise_all(.funs = function(x) sum(is.na(x))) %>% 
  c()

write_parquet(cities_clean_sub, "data/clustering_data_clean/GHS_UCDB_2024_preproc_2025_04_09_uci_and_nan_imputation_add_vars_included.parquet")
