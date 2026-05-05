library(ClimActor)
library(sf)
library(dplyr)
library(tidyverse)
library(stringr)
library(stringi)
library(countrycode)

# ── 0. Load data ──────────────────────────────────────────────────────────────
load("/Users/simon/Documents/repo/ClimActor/data/contextuals.rda")

# Entity-level exclusions:
#   - entity_type != "City": removes regions, states, provinces
#   - ctx_not_city: ClimActor entries mislabelled as City (counties, regions)
#   - ctx_bad_coords: entries whose coordinates are demonstrably wrong and
#     cause spatial mismatches (verified case-by-case)
ctx_not_city <- c(
  "lancashire, uk",   # county, mislabelled City
  "dane county, wi"   # county, mislabelled City
)

ctx_bad_coords <- tribble(
  ~norm_name,  ~norm_country,
  "holon",     "israel",       # Israeli city geocoded to Spain (Oviedo polygon)
  "xalapa",    "mexico"        # Xalapa coords fall inside Pachuca GHSL polygon
)

contextuals_v100 <- contextuals_v100 %>%
  as_tibble() %>%
  filter(entity_type == "City") %>%
  filter(!normalise(name) %in% ctx_not_city)

load_key_dict()

ghsl <- st_read("/Users/simon/Documents/repo/cities-learning-dec/data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") %>%
  as_tibble() %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, geom)

cat(sprintf("ClimActor city entries (after exclusions): %d\n", nrow(contextuals_v100)))

# ── 1. Helpers ────────────────────────────────────────────────────────────────
count_initiatives <- function(x) {
  map_int(x, function(s) {
    if (is.na(s) || s == "") return(0L)
    parts <- str_split(s, ";;|;")[[1]]
    length(parts[nzchar(trimws(parts))])
  })
}

to_iso3 <- function(x) {
  x <- case_match(x,
                  "México"          ~ "Mexico",
                  "Kosovo"          ~ "Kosovo",
                  "Hong Kong"       ~ "Hong Kong SAR China",
                  "Northern Cyprus" ~ "Cyprus",
                  .default = x
  )
  countrycode(x, origin = "country.name", destination = "iso3c", warn = FALSE)
}

normalise <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_trim()
}

# ── 2. Diagnostic helpers ─────────────────────────────────────────────────────
inspect_city <- function(norm_name, iso3 = NULL) {
  cat("\n── GHSL side ──\n")
  g <- ghsl %>%
    mutate(.nk  = normalise(GC_UCN_MAI_2025),
           .iso = to_iso3(GC_CNT_GAD_2025)) %>%
    filter(.nk == norm_name)
  if (!is.null(iso3)) g <- g %>% filter(.iso == iso3)
  if (nrow(g) == 0) cat("  (no match)\n") else
    print(g %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, .iso))
  
  cat("\n── ClimActor side (city entities only) ──\n")
  c <- contextuals_v100 %>%
    mutate(.nk  = normalise(name),
           .iso = to_iso3(country)) %>%
    filter(.nk == norm_name)
  if (!is.null(iso3)) c <- c %>% filter(.iso == iso3)
  if (nrow(c) == 0) cat("  (no match)\n") else
    print(c %>% select(name, country, .iso, lat, lng, initiatives_committed))
  
  invisible(list(ghsl = g, ctx = c))
}

print_dropped <- function(log, reason = NULL) {
  if (!is.null(reason)) log <- log %>% filter(drop_reason == reason)
  cat(sprintf("\n── Dropped name matches: %d cities ──\n", nrow(log)))
  log %>%
    select(ghsl_city, ghsl_country, ghsl_iso3,
           ctx_name, ctx_country, ctx_iso3,
           drop_reason, initiatives_committed) %>%
    arrange(drop_reason, ghsl_country) %>%
    print(n = 200)
}

# ── 3. Interactive review function ───────────────────────────────────────────
review_ambiguous <- function(audit_log, ctx, ghsl_raw,
                             step = c("name", "spatial", "both")) {
  step <- match.arg(step)
  
  cases <- audit_log %>%
    { if (step == "both") . else filter(., match_step == step) } %>%
    filter(audit_reason == "unambiguous_name_country_mismatch") %>%
    distinct(ID_UC_G0, .keep_all = TRUE)
  
  cat(sprintf("\n%d cases to review (step: %s)\n", nrow(cases), step))
  cat("Commands: k = keep, d = drop, s = skip, q = quit\n")
  cat("Tip: run inspect_city(\"<norm_name>\") for more detail\n\n")
  
  decisions <- vector("list", nrow(cases))
  
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    
    ctx_rows <- ctx %>%
      filter(normalise(name) == normalise(row$ctx_name),
             normalise(country) == normalise(row$ctx_country)) %>%
      select(name, country, lat, lng, initiatives_committed)
    
    cat(rep("-", 60), "\n", sep = "")
    cat(sprintf("[%d/%d] GHSL: %s | %s (iso3=%s)\n",
                i, nrow(cases),
                row$ghsl_city, row$ghsl_country,
                ifelse(is.na(row$ghsl_iso3), "NA", row$ghsl_iso3)))
    cat(sprintf("        CTX:  %s | %s (iso3=%s)\n",
                row$ctx_name, row$ctx_country,
                ifelse(is.na(row$ctx_iso3), "NA", row$ctx_iso3)))
    cat(sprintf("        Initiatives: %s\n",
                ifelse(is.na(row$initiatives_committed), "none",
                       str_trunc(row$initiatives_committed, 100))))
    if (nrow(ctx_rows) > 0 && !is.na(ctx_rows$lat[1]))
      cat(sprintf("        CTX lat/lng: %.3f, %.3f\n",
                  ctx_rows$lat[1], ctx_rows$lng[1]))
    
    repeat {
      ans <- readline("  Decision [k/d/s/q]: ")
      ans <- str_to_lower(str_trim(ans))
      if (ans %in% c("k", "d", "s", "q")) break
      cat("  Enter k (keep), d (drop), s (skip), or q (quit)\n")
    }
    
    if (ans == "q") { cat("Quitting review.\n"); break }
    
    decisions[[i]] <- tibble(
      ID_UC_G0     = row$ID_UC_G0,
      ghsl_city    = row$ghsl_city,
      ghsl_country = row$ghsl_country,
      ghsl_iso3    = row$ghsl_iso3,
      ctx_name     = row$ctx_name,
      ctx_country  = row$ctx_country,
      decision     = case_match(ans,
                                "k" ~ "keep", "d" ~ "drop", "s" ~ "skip")
    )
  }
  
  out <- bind_rows(decisions)
  cat(sprintf("\nReview complete: %d decisions recorded.\n", nrow(out)))
  if (nrow(out) > 0) {
    print(count(out, decision))
    keeps <- out %>% filter(decision == "keep")
    if (nrow(keeps) > 0) {
      cat("\nAdd these to manual_decisions:\n")
      keeps %>%
        mutate(line = sprintf('  "%s", "%s", "keep",',
                              normalise(ghsl_city), ghsl_iso3)) %>%
        pull(line) %>% cat(sep = "\n")
    }
  }
  invisible(out)
}

# ── 4. European CoM country ISO3 set ─────────────────────────────────────────
com_europe_iso3 <- c(
  "ESP", "ITA", "BEL", "FRA", "PRT", "GRC", "DEU", "AUT", "NLD",
  "SWE", "DNK", "FIN", "NOR", "POL", "CZE", "SVK", "HUN", "ROU",
  "BGR", "HRV", "SVN", "LTU", "LVA", "EST", "LUX", "MLT", "CYP",
  "IRL", "GBR", "CHE", "ISL"
)

# ── 5. Manual keep decisions ──────────────────────────────────────────────────
# Default is DROP for all country mismatches.
# Only entries listed here with action = "keep" survive.
# Verify with: inspect_city("<norm_name>", "<iso3>")
manual_decisions <- tribble(
  ~norm_ghsl_city, ~ghsl_iso3, ~action,
  "hong kong",     "CHN",      "keep",   # HKSAR; ClimActor=HKG, GHSL=CHN
  # Add further confirmed keeps after review:
  # "<normalised_name>", "<ghsl_iso3>", "keep",
)

# ── 6. Add ISO3 and normalised name key ───────────────────────────────────────
ghsl_clean <- ghsl %>%
  mutate(
    .norm_key  = normalise(GC_UCN_MAI_2025),
    .iso3_ghsl = to_iso3(GC_CNT_GAD_2025)
  )

ctx_clean <- contextuals_v100 %>%
  mutate(
    .norm_key = normalise(name),
    .iso3_ctx = to_iso3(country)
  )

# ── 7. Deduplication function ─────────────────────────────────────────────────
drop_log <- tibble()

dedup_matches <- function(df, step_label, ambiguity_log) {
  
  df <- df %>%
    mutate(
      .country_match = !is.na(.iso3_ctx) & !is.na(.iso3_ghsl) &
        (.iso3_ghsl == .iso3_ctx),
      .n_initiatives = count_initiatives(initiatives_committed)
    )
  
  candidate_counts <- df %>%
    group_by(ID_UC_G0) %>%
    summarise(
      .n_candidates    = n(),
      .n_country_match = sum(.country_match, na.rm = TRUE),
      .groups = "drop"
    )
  
  df <- df %>% left_join(candidate_counts, by = "ID_UC_G0")
  
  new_audit <- bind_rows(
    df %>% filter(.n_candidates > 1, .n_country_match > 1) %>%
      mutate(.audit_reason = "multiple_country_matches"),
    df %>% filter(.n_candidates > 1, .n_country_match == 0) %>%
      mutate(.audit_reason = "no_country_match_used_initiatives"),
    df %>% filter(.n_candidates == 1, !.country_match) %>%
      mutate(.audit_reason = "unambiguous_name_country_mismatch")
  ) %>%
    transmute(
      match_step        = step_label,
      audit_reason      = .audit_reason,
      ID_UC_G0,
      ghsl_city         = GC_UCN_MAI_2025,
      ghsl_country      = GC_CNT_GAD_2025,
      ghsl_iso3         = .iso3_ghsl,
      ctx_name          = name,
      ctx_country       = country,
      ctx_iso3          = .iso3_ctx,
      initiatives_committed,
      n_candidates      = .n_candidates,
      n_country_matches = .n_country_match
    )
  
  ambiguity_log <<- bind_rows(ambiguity_log, new_audit)
  
  resolved <- df %>%
    group_by(ID_UC_G0) %>%
    arrange(desc(.country_match), desc(.n_initiatives)) %>%
    slice(1) %>%
    ungroup()
  
  if (step_label == "name") {
    
    resolved <- resolved %>%
      left_join(manual_decisions,
                by = c(".norm_key" = "norm_ghsl_city",
                       ".iso3_ghsl" = "ghsl_iso3"))
    
    if (!"action" %in% names(resolved)) resolved$action <- NA_character_
    resolved$action <- ifelse(is.na(resolved$action), "none", resolved$action)
    
    resolved$.com_fp <- !is.na(resolved$.iso3_ctx) &
      (resolved$.iso3_ctx %in% com_europe_iso3) &
      (is.na(resolved$.iso3_ghsl) |
         !(resolved$.iso3_ghsl %in% com_europe_iso3))
    
    resolved$.kosovo <- resolved$.norm_key == "pristina" &
      is.na(resolved$.iso3_ghsl) &
      is.na(resolved$.iso3_ctx)
    
    # DROP BY DEFAULT: keep only confirmed country match, explicit keep, Kosovo
    resolved$.keep <- resolved$action == "keep" |
      resolved$.kosovo |
      (resolved$action != "drop" & resolved$.country_match)
    
    resolved$.keep <- ifelse(resolved$action == "drop", FALSE, resolved$.keep)
    resolved$.keep <- ifelse(is.na(resolved$.keep), FALSE, resolved$.keep)
    
    dropped <- resolved %>%
      filter(!.keep, !.country_match) %>%
      transmute(
        ghsl_city    = GC_UCN_MAI_2025,
        ghsl_country = GC_CNT_GAD_2025,
        ghsl_iso3    = .iso3_ghsl,
        ctx_name     = name,
        ctx_country  = country,
        ctx_iso3     = .iso3_ctx,
        initiatives_committed,
        drop_reason  = case_when(
          action == "drop" ~ "manual_drop",
          .com_fp          ~ "com_europe_fp",
          TRUE             ~ "country_mismatch_default_drop"
        )
      )
    drop_log <<- bind_rows(drop_log, dropped)
    
    resolved <- resolved[resolved$.keep, ]
    resolved$action <- NULL
  }
  
  resolved %>% select(-starts_with("."))
}

# ── 8. STEP 1 — Name-based merge ─────────────────────────────────────────────
ambiguity_log <- tibble()
drop_log      <- tibble()

name_joined <- ghsl_clean %>%
  inner_join(
    ctx_clean %>% select(.norm_key, .iso3_ctx, name, country, initiatives_committed),
    by = ".norm_key",
    relationship = "many-to-many"
  )

name_matched_raw <- dedup_matches(name_joined, step_label = "name", ambiguity_log) %>%
  mutate(match_method = "name")

# ── Resolve GHSL-side duplicates by distance ──────────────────────────────────
# A single ClimActor entry can match multiple GHSL cities with the same name
# within the same country (e.g. two "Udaipur" polygons in India, two
# "Banjarmasin" polygons in Indonesia). Keep only the GHSL city whose centroid
# is closest to the ClimActor point. For entries with missing ClimActor
# coordinates, keep the first row arbitrarily (no basis for distance ranking).
name_matched <- name_matched_raw %>%
  mutate(
    .norm_key = normalise(name),
    .iso3_ctx = to_iso3(country)
  ) %>%
  left_join(
    ctx_clean %>% select(.norm_key, .iso3_ctx, lat, lng) %>%
      rename(ctx_lat = lat, ctx_lng = lng),
    by = c(".norm_key", ".iso3_ctx")
  ) %>%
  st_as_sf() %>%
  mutate(
    .centroid = st_transform(st_centroid(geom), 4326),
    .ghsl_lng = st_coordinates(.centroid)[, 1],
    .ghsl_lat = st_coordinates(.centroid)[, 2],
    .dist_km  = mapply(function(clat, clng, glat, glng) {
      if (any(is.na(c(clat, clng, glat, glng)))) return(Inf)
      R    <- 6371
      dlat <- (glat - clat) * pi / 180
      dlon <- (glng - clng) * pi / 180
      a    <- sin(dlat/2)^2 +
        cos(clat * pi/180) * cos(glat * pi/180) * sin(dlon/2)^2
      R * 2 * atan2(sqrt(a), sqrt(1 - a))
    }, ctx_lat, ctx_lng, .ghsl_lat, .ghsl_lng)
  ) %>%
  st_drop_geometry() %>%
  group_by(name, country) %>%
  slice_min(.dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-ctx_lat, -ctx_lng, -.centroid, -.ghsl_lng, -.ghsl_lat, -.dist_km)

n_dupes_resolved <- nrow(name_matched_raw) - nrow(name_matched)

cat(sprintf("Name-matched       : %d / %d ghsl entries\n",
            nrow(name_matched), nrow(ghsl)))
cat(sprintf("  Duplicate GHSL rows removed by distance: %d\n", n_dupes_resolved))
cat(sprintf("Dropped mismatches : %d\n", nrow(drop_log)))
cat(sprintf("  com_europe_fp            : %d\n",
            sum(drop_log$drop_reason == "com_europe_fp")))
cat(sprintf("  manual_drop              : %d\n",
            sum(drop_log$drop_reason == "manual_drop")))
cat(sprintf("  country_mismatch_default : %d\n",
            sum(drop_log$drop_reason == "country_mismatch_default_drop")))

# ── 8b. Manual recall overrides from string-similarity review ────────────────
# ClimActor entries that failed name/spatial matching due to name variants
# (transliterations, state suffixes, administrative suffixes, adjacent cities).
# Reviewed manually: 41 confirmed correct matches mapped to GHSL ID directly.
# Where multiple ClimActor entries map to the same GHSL ID (e.g. Kasulu),
# initiatives are combined.
# Read reviewed correct matches from file
recall_overrides <- read_csv(
  "data/c2cNW/ghsl_recall_reviewed_all.csv",
  show_col_types = FALSE
) %>%
  filter(verdict == "correct") %>% 
  select(ctx_name, ctx_country, ghsl_id = best_ghsl_id)

# Pull initiatives from ClimActor for these entries
recall_ctx <- contextuals_v100 %>%
  inner_join(recall_overrides, by = c("name" = "ctx_name", "country" = "ctx_country")) %>%
  select(ghsl_id, initiatives_committed)

# Combine initiatives for same GHSL ID 
recall_matched <- recall_ctx %>%
  group_by(ghsl_id) %>%
  summarise(
    initiatives_committed = paste(
      unique(na.omit(initiatives_committed)), collapse = ";;"
    ),
    .groups = "drop"
  ) %>%
  # Only add if not already matched in name_matched
  filter(!ghsl_id %in% name_matched$ID_UC_G0) %>%
  left_join(
    ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, geom),
    by = c("ghsl_id" = "ID_UC_G0")
  ) %>%
  rename(ID_UC_G0 = ghsl_id) %>%
  mutate(
    name         = GC_UCN_MAI_2025,
    country      = GC_CNT_GAD_2025,
    match_method = "recall_override"
  )

cat(sprintf("Recall overrides added: %d cities\n", nrow(recall_matched)))

# ── 9. STEP 2 — Spatial intersection (same-country, city entities only) ───────
ghsl_unmatched <- ghsl_clean %>%
  filter(!ID_UC_G0 %in% name_matched$ID_UC_G0,
         !ID_UC_G0 %in% recall_matched$ID_UC_G0)

cat(sprintf("Rows to spatial    : %d\n", nrow(ghsl_unmatched)))

ghsl_sf <- ghsl_unmatched %>%
  { if (inherits(., "sf")) . else st_as_sf(.) } %>%
  st_transform(4326)

# Exclude bad-coordinate entries from spatial join
ctx_sf <- ctx_clean %>%
  anti_join(ctx_bad_coords,
            by = c(".norm_key" = "norm_name",
                   # normalise country for matching
                   ".iso3_ctx" = "norm_country") %>%
              { . }) %>%
  { 
    # anti_join on normalised country string, not iso3
    bad <- ctx_bad_coords %>%
      mutate(.iso3_ctx = to_iso3(norm_country))
    anti_join(ctx_clean, bad,
              by = c(".norm_key" = "norm_name", ".iso3_ctx"))
  } %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

spatial_join_raw <- st_join(
  ghsl_sf %>% st_make_valid() %>%
    select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, .norm_key, .iso3_ghsl),
  ctx_sf %>% select(name, country, .iso3_ctx, initiatives_committed),
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  filter(
    !is.na(initiatives_committed),
    !is.na(.iso3_ghsl), !is.na(.iso3_ctx),
    .iso3_ghsl == .iso3_ctx
  )

n_audit_before <- nrow(ambiguity_log)

spatial_matched <- dedup_matches(spatial_join_raw, step_label = "spatial", ambiguity_log) %>%
  mutate(match_method = "spatial")

cat(sprintf("Spatially matched  : %d rows\n", nrow(spatial_matched)))
cat(sprintf("Audit log (spatial): %d rows across %d cities\n",
            nrow(ambiguity_log) - n_audit_before,
            n_distinct(filter(ambiguity_log, match_step == "spatial")$ID_UC_G0)))

# ── 10. STEP 3 — Combine + derive columns ────────────────────────────────────
all_matched <- bind_rows(name_matched, recall_matched, spatial_matched) %>%
  mutate(
    initiatives_committed_binary = if_else(
      !is.na(initiatives_committed) & initiatives_committed != "", 1L, 0L
    ),
    initiatives_committed_count = count_initiatives(initiatives_committed)
  )

# ── 11. STEP 4 — Full ghsl output ────────────────────────────────────────────
ghsl_final <- ghsl %>%
  left_join(
    all_matched %>%
      select(ID_UC_G0, initiatives_committed,
             initiatives_committed_binary,
             initiatives_committed_count,
             match_method),
    by = "ID_UC_G0"
  ) %>%
  mutate(
    initiatives_committed_binary = replace_na(initiatives_committed_binary, 0L),
    initiatives_committed_count  = replace_na(initiatives_committed_count,  0L),
    match_method = replace_na(match_method, "unmatched")
  ) %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025,
         initiatives_committed, initiatives_committed_binary,
         initiatives_committed_count, match_method)

# ── 12. Summary ───────────────────────────────────────────────────────────────
cat(sprintf("\n========== SUMMARY ==========\n"))
cat(sprintf("Total ghsl rows      : %d\n", nrow(ghsl_final)))
cat(sprintf("Matched (name)       : %d\n", sum(ghsl_final$match_method == "name")))
cat(sprintf("Matched (recall)     : %d\n", sum(ghsl_final$match_method == "recall_override")))
cat(sprintf("Matched (spatial)    : %d\n", sum(ghsl_final$match_method == "spatial")))
cat(sprintf("Unmatched            : %d\n", sum(ghsl_final$match_method == "unmatched")))
cat(sprintf("With initiatives     : %d\n", sum(ghsl_final$initiatives_committed_binary)))

cat("\n-- Audit reasons by step --\n")
print(count(ambiguity_log, match_step, audit_reason))

cat("\n-- Spatial match entity types (should be City only) --\n")
spatial_matched %>%
  left_join(contextuals_v100 %>% select(name, country, entity_type),
            by = c("name", "country")) %>%
  count(entity_type) %>%
  print()

# ── 13. Diagnostic prints ─────────────────────────────────────────────────────
cat("\n========== Default-dropped name mismatches ==========\n")
drop_log %>%
  filter(drop_reason == "country_mismatch_default_drop") %>%
  select(ghsl_city, ghsl_country, ghsl_iso3,
         ctx_name, ctx_country, ctx_iso3, initiatives_committed) %>%
  arrange(ghsl_country) %>%
  print(n = 200)

cat("\n========== Spatial matches — full list ==========\n")
ghsl_final %>%
  filter(match_method == "spatial") %>%
  left_join(spatial_matched %>% select(ID_UC_G0, name, country), by = "ID_UC_G0") %>%
  select(GC_UCN_MAI_2025, GC_CNT_GAD_2025, name, initiatives_committed_count) %>%
  arrange(GC_CNT_GAD_2025, GC_UCN_MAI_2025) %>%
  print(n = 500)

# ── 14. Check file for manual one-by-one review ───────────────────────────────
# Matched rows only. Both city/country names, ClimActor lat/lng,
# GHSL centroid in WGS84 (lng/lat), initiatives, match method, distance.
check_file <- all_matched %>%
  left_join(
    contextuals_v100 %>%
      select(name, country, lat, lng) %>%
      rename(ctx_name = name, ctx_country = country,
             ctx_lat = lat, ctx_lng = lng),
    by = c("name" = "ctx_name", "country" = "ctx_country")
  ) %>%
  st_as_sf() %>%
  mutate(
    .centroid = st_transform(st_centroid(geom), 4326),
    ghsl_lng  = st_coordinates(.centroid)[, 1],
    ghsl_lat  = st_coordinates(.centroid)[, 2],
    dist_km   = mapply(function(clat, clng, glat, glng) {
      if (any(is.na(c(clat, clng, glat, glng)))) return(NA_real_)
      R    <- 6371
      dlat <- (glat - clat) * pi / 180
      dlon <- (glng - clng) * pi / 180
      a    <- sin(dlat/2)^2 +
        cos(clat * pi/180) * cos(glat * pi/180) * sin(dlon/2)^2
      R * 2 * atan2(sqrt(a), sqrt(1 - a))
    }, ctx_lat, ctx_lng, ghsl_lat, ghsl_lng) %>% round(1)
  ) %>%
  st_drop_geometry() %>%
  select(
    ID_UC_G0,
    ghsl_city    = GC_UCN_MAI_2025,
    ghsl_country = GC_CNT_GAD_2025,
    ctx_name     = name,
    ctx_country  = country,
    ctx_lat, ctx_lng,
    ghsl_lng, ghsl_lat,
    dist_km,
    initiatives_committed,
    initiatives_committed_count,
    match_method
  ) %>%
  arrange(match_method, ghsl_country, ghsl_city)

check_file %>%
  write_csv("data/c2cNW/ghsl_match_check.csv", na = "")

cat(sprintf("Check file rows: %d (matched cities only)\n", nrow(check_file)))

# ── 15. Save ──────────────────────────────────────────────────────────────────
write_csv(ghsl_final,    "data/c2cNW/ghsl_final_with_initiatives.csv")
write_csv(ambiguity_log, "data/c2cNW/ghsl_audit_log.csv")
write_csv(drop_log,      "data/c2cNW/ghsl_drop_log.csv")

cat("\nFiles written:\n")
cat("  ghsl_match_check.csv             — matched rows for manual review\n")
cat("  ghsl_final_with_initiatives.csv\n")
cat("  ghsl_audit_log.csv\n")
cat("  ghsl_drop_log.csv\n")

# ── Recall check: unmatched ClimActor cities with string similarity ───────────
# For ClimActor city entries with population > 30,000 and initiatives that did
# not match any GHSL city, compute Jaro-Winkler similarity against all GHSL
# cities in the same country. Flag top candidates for manual review.
# Requires: install.packages("stringdist") if not already installed.

if (!requireNamespace("stringdist", quietly = TRUE)) {
  install.packages("stringdist", quiet = TRUE)
}
library(stringdist)

# ClimActor cities with initiatives, population filter, not yet matched
ctx_with_initiatives <- contextuals_v100 %>%
  filter(
    !is.na(initiatives_committed),
    initiatives_committed != "",
    !is.na(population), population > 30000
  ) %>%
  mutate(
    .norm_key = normalise(name),
    .iso3_ctx = to_iso3(country),
    n_initiatives = count_initiatives(initiatives_committed)
  )

# Which ClimActor entries made it into all_matched?
# Recompute norm_key and iso3 since dot-columns are dropped by dedup_matches.
matched_ctx_keys <- all_matched %>%
  mutate(
    norm_key = normalise(name),
    iso3_ctx = to_iso3(country)
  ) %>%
  distinct(norm_key, iso3_ctx)

# Unmatched ClimActor entries
ctx_unmatched <- ctx_with_initiatives %>%
  anti_join(matched_ctx_keys, by = c(".norm_key" = "norm_key",
                                     ".iso3_ctx" = "iso3_ctx"))

cat(sprintf("\n========== Recall check ==========\n"))
cat(sprintf("ClimActor cities with initiatives + pop>30k : %d\n",
            nrow(ctx_with_initiatives)))
cat(sprintf("Matched                                     : %d\n",
            nrow(ctx_with_initiatives) - nrow(ctx_unmatched)))
cat(sprintf("Unmatched                                   : %d\n",
            nrow(ctx_unmatched)))

# GHSL city lookup table: normalised name + iso3 + original name
ghsl_lookup <- ghsl_clean %>%
  st_drop_geometry() %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, .norm_key, .iso3_ghsl) %>%
  filter(!is.na(.iso3_ghsl)) %>%
  filter(!ID_UC_G0 %in% all_matched$ID_UC_G0)   # exclude already-matched cities

# For each unmatched ClimActor city, find the best GHSL match in same country
similarity_results <- ctx_unmatched %>%
  select(
    ctx_name      = name,
    ctx_country   = country,
    ctx_iso3      = .iso3_ctx,
    ctx_norm      = .norm_key,
    ctx_lat       = lat,
    ctx_lng       = lng,
    n_initiatives,
    population,
    initiatives_committed
  )

sim_list <- lapply(seq_len(nrow(similarity_results)), function(i) {
  row        <- similarity_results[i, ]
  candidates <- ghsl_lookup %>% filter(.iso3_ghsl == row$ctx_iso3)
  
  if (nrow(candidates) == 0) return(
    tibble(best_ghsl_id = NA_real_, best_ghsl_name = NA_character_,
           best_ghsl_country = NA_character_, best_ghsl_norm = NA_character_,
           best_similarity = NA_real_, dist_km = NA_real_,
           n_ghsl_same_country = 0L)
  )
  
  sims     <- 1 - stringdist(row$ctx_norm, candidates$.norm_key,
                             method = "jw", p = 0.1)
  best_idx <- which.max(sims)
  
  # Distance between ClimActor point and best GHSL candidate centroid (WGS84)
  best_candidate <- candidates[best_idx, ]
  centroid_wgs   <- st_transform(
    st_centroid(
      st_as_sf(
        ghsl %>%
          filter(ID_UC_G0 == best_candidate$ID_UC_G0) %>%
          select(geom)
      )
    ), 4326
  )
  ghsl_lng <- st_coordinates(centroid_wgs)[, 1]
  ghsl_lat <- st_coordinates(centroid_wgs)[, 2]
  
  dist_km <- tryCatch({
    if (!is.na(row$ctx_lat) && !is.na(row$ctx_lng)) {
      R    <- 6371
      dlat <- (ghsl_lat - row$ctx_lat) * pi / 180
      dlon <- (ghsl_lng - row$ctx_lng) * pi / 180
      a    <- sin(dlat/2)^2 +
        cos(row$ctx_lat * pi/180) * cos(ghsl_lat * pi/180) * sin(dlon/2)^2
      round(R * 2 * atan2(sqrt(a), sqrt(1 - a)), 1)
    } else NA_real_
  }, error = function(e) NA_real_)
  
  tibble(
    best_ghsl_id        = best_candidate$ID_UC_G0,
    best_ghsl_name      = best_candidate$GC_UCN_MAI_2025,
    best_ghsl_country   = best_candidate$GC_CNT_GAD_2025,
    best_ghsl_norm      = best_candidate$.norm_key,
    best_similarity     = round(sims[best_idx], 3),
    dist_km             = dist_km,
    n_ghsl_same_country = nrow(candidates)
  )
})

similarity_results <- bind_cols(
  similarity_results,
  bind_rows(sim_list)
) %>%
  filter(!is.na(best_similarity)) %>%
  arrange(desc(best_similarity), desc(n_initiatives))

cat(sprintf("\nString similarity computed for %d unmatched cities\n",
            nrow(similarity_results)))

# Summary by similarity band
cat("\n-- Similarity distribution --\n")
similarity_results %>%
  mutate(sim_band = cut(best_similarity,
                        breaks = c(0, 0.7, 0.8, 0.85, 0.9, 0.95, 1.01),
                        labels = c("<0.70","0.70-0.80","0.80-0.85",
                                   "0.85-0.90","0.90-0.95",">=0.95"),
                        right = FALSE)) %>%
  count(sim_band) %>%
  print()

# Top candidates for review (similarity >= 0.85)
cat("\n-- Top unmatched candidates (similarity >= 0.85) --\n")
similarity_results %>%
  filter(best_similarity >= 0.85) %>%
  select(ctx_name, ctx_country, best_ghsl_name, best_ghsl_country,
         best_similarity, dist_km, n_initiatives, population) %>%
  print(n = 100)

# Save full similarity table
write_csv(
  similarity_results %>%
    select(ctx_name, ctx_country, ctx_iso3, ctx_lat, ctx_lng,
           n_initiatives, population,
           best_ghsl_id, best_ghsl_name, best_ghsl_country,
           best_similarity, dist_km, n_ghsl_same_country, initiatives_committed),
  "data/c2cNW/ghsl_recall_similarity.csv",
  na = ""
)
cat("  ghsl_recall_similarity.csv\n")

# ── 15. Post-run helpers (call interactively) ─────────────────────────────────
# inspect_city("brest", "FRA")
# inspect_city("concepcion")
# print_dropped(drop_log)
# print_dropped(drop_log, "country_mismatch_default_drop")
# review_ambiguous(ambiguity_log, contextuals_v100, ghsl, step = "name")