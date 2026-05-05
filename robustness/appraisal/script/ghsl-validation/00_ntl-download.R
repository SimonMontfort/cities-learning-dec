# stage1_ntl.R
# Compute mean NTL per UCDB polygon using blackmarbler
# Replaces stage1_ntl.py entirely

library(blackmarbler)
library(sf)
library(dplyr)

setwd("/Users/simon/Documents/repo/cities-learning-dec/robustness/appraisal")

# ── 1. Authentication ──────────────────────────────────────────
# Get bearer token — only need to do this once, it's stored for reuse
# Requires a free NASA Earthdata account: https://urs.earthdata.nasa.gov
bearer <- get_nasa_token(username = "",
                         password = "")

# ── 2. Load UCDB polygons ──────────────────────────────────────
ucdb <- st_read("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") %>%
  st_transform(4326)  # must be WGS84

ucdb <- ucdb %>% 
  st_make_valid()

# ── 3. Extract mean NTL per polygon ───────────────────────────
# bm_extract downloads only the tiles needed, mosaics them,
# and returns mean NTL per polygon — no manual tile management needed.
# For ~10,000 global polygons this will take a while (hours),
# so save intermediate results.

ntl_df <- bm_extract(
  roi_sf      = st_buffer(ucdb[1:5, ], 0),
  product_id  = "VNP46A4",
  date        = 2020,
  bearer      = bearer,
  file_dir   = "path_to_Gdrive/repo/cities-learning-dec-data/night-time-lights", 
  file_skip_if_exists = TRUE,
  check_all_tiles_exist = FALSE
  # Default variable is NearNadir_Composite_Snow_Free — correct for our use
)

# ── 4. Tidy and export ─────────────────────────────────────────
# bm_extract returns the original sf with a new NTL column
ntl_out <- ntl_df %>%
  st_drop_geometry() %>%
  select(ucdb_id = ID_HDC_G0,
         ntl_mean = NearNadir_Composite_Snow_Free) %>%
  mutate(ntl_mean = as.numeric(ntl_mean))

write.csv(ntl_out, "data/ntl_zonal_stats.csv", row.names = FALSE)
cat("Done.", nrow(ntl_out), "polygons written to data/ntl_zonal_stats.csv\n")