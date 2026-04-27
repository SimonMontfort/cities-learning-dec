library(ClimActor)
library(sf)
library(dplyr)
library(tidyverse)
library(sf)
library(stringr)


load("/Users/simon/Documents/repo/ClimActor/data/contextuals.rda")

contextuals_v100 <- contextuals_v100 %>% as_tibble()

load_key_dict()

unique(key_dict$ent_type_eng)


key_dict %>% 
  filter(ent_type_eng == "City")

unique(key_dict$entity_type)


ghsl <- st_read("/Users/simon/Documents/repo/cities-learning-dec/data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") %>% as_tibble()

ghsl <- ghsl %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, geom)
# ============================================================
# Merge ghsl + contextuals_v100
# Strategy:
#   1. Name match: GC_UCN_MAI_2025 == name
#   2. Spatial intersection fallback for unmatched rows
#   3. Report all non-matched ghsl entries
#
# Deduplication priority (when one ghsl city hits multiple ctx rows):
#   1. Country match (GC_CNT_GAD_2025 == country) — preferred
#   2. If still ambiguous: most initiatives
#   All ambiguous cases are logged to ghsl_ambiguous_matches.csv
# ============================================================


# ── 0. Helpers ───────────────────────────────────────────────

count_initiatives <- function(x) {
  map_int(x, function(s) {
    if (is.na(s) || s == "") return(0L)
    parts <- str_split(s, ";;|;")[[1]]
    length(parts[nzchar(trimws(parts))])
  })
}

# Deduplicate a join result for a given step label.
# Priority: (1) country match, (2) most initiatives.
# Logs:
#   "multiple_country_matches"         — country narrows but >1 row still tie
#   "no_country_match_used_initiatives" — no country match at all; fell back to initiatives
dedup_matches <- function(df, step_label, ambiguity_log) {
  
  df <- df %>%
    mutate(
      .country_ghsl  = str_to_lower(str_trim(GC_CNT_GAD_2025)),
      .country_ctx   = str_to_lower(str_trim(country)),
      .country_match = !is.na(.country_ctx) & (.country_ghsl == .country_ctx),
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
  
  # ── Log ambiguous cases (before resolving) ─────────────────
  
  ambiguous_b <- df %>%
    filter(.n_candidates > 1, .n_country_match > 1) %>%
    mutate(.ambiguity_reason = "multiple_country_matches")
  
  ambiguous_c <- df %>%
    filter(.n_candidates > 1, .n_country_match == 0) %>%
    mutate(.ambiguity_reason = "no_country_match_used_initiatives")
  
  new_ambiguous <- bind_rows(ambiguous_b, ambiguous_c) %>%
    transmute(
      match_step        = step_label,
      ambiguity_reason  = .ambiguity_reason,
      ID_UC_G0,
      ghsl_city         = GC_UCN_MAI_2025,
      ghsl_country      = GC_CNT_GAD_2025,
      ctx_name          = name,
      ctx_country       = country,
      initiatives_committed,
      n_candidates      = .n_candidates,
      n_country_matches = .n_country_match
    )
  
  ambiguity_log <<- bind_rows(ambiguity_log, new_ambiguous)
  
  # ── Resolve: country first, then initiatives ───────────────
  df %>%
    group_by(ID_UC_G0) %>%
    arrange(desc(.country_match), desc(.n_initiatives)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-starts_with("."))
}

# ============================================================
# STEP 1 — Name-based merge (case-insensitive, trimmed)
# ============================================================

ambiguity_log <- tibble()

ghsl_clean <- ghsl %>%
  mutate(.name_key = str_to_lower(str_trim(GC_UCN_MAI_2025)))

ctx_clean <- contextuals_v100 %>%
  mutate(.name_key = str_to_lower(str_trim(name)))

name_joined <- ghsl_clean %>%
  inner_join(
    ctx_clean %>% select(.name_key, name, country, initiatives_committed),
    by = ".name_key",
    relationship = "many-to-many"
  )

name_matched <- dedup_matches(name_joined, step_label = "name", ambiguity_log) %>%
  mutate(match_method = "name")

cat(sprintf("Name-matched rows  : %d / %d ghsl entries\n", nrow(name_matched), nrow(ghsl)))
cat(sprintf("Ambiguous (name)   : %d candidate rows across %d ghsl cities\n",
            nrow(ambiguity_log), n_distinct(ambiguity_log$ID_UC_G0)))

# ============================================================
# STEP 2 — Spatial intersection fallback
# ============================================================

ghsl_unmatched_name <- ghsl_clean %>%
  filter(!ID_UC_G0 %in% name_matched$ID_UC_G0)

cat(sprintf("ghsl rows to spatial: %d\n", nrow(ghsl_unmatched_name)))

ghsl_sf <- ghsl_unmatched_name %>%
  { if (inherits(., "sf")) . else st_as_sf(.) } %>%
  st_transform(4326)

ctx_sf <- ctx_clean %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

spatial_join_raw <- st_join(
  ghsl_sf  %>% st_make_valid() %>% select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025, .name_key),
  ctx_sf   %>% select(name, country, initiatives_committed),
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  filter(!is.na(initiatives_committed)) %>%
  # Restore any remaining ghsl columns needed for dedup (none beyond what's already here,
  # but left_join keeps the pattern consistent for future columns)
  left_join(
    ghsl_clean %>%
      { if (inherits(., "sf")) st_drop_geometry(.) else . } %>%
      select(ID_UC_G0) %>%   # add extra ghsl cols here if needed later
      distinct(),
    by = "ID_UC_G0"
  )

n_ambig_before <- nrow(ambiguity_log)

spatial_matched <- dedup_matches(spatial_join_raw, step_label = "spatial", ambiguity_log) %>%
  mutate(match_method = "spatial")

cat(sprintf("Spatially matched  : %d rows\n", nrow(spatial_matched)))
cat(sprintf("Ambiguous (spatial): %d candidate rows across %d ghsl cities\n",
            nrow(ambiguity_log) - n_ambig_before,
            n_distinct(filter(ambiguity_log, match_step == "spatial")$ID_UC_G0)))

# ============================================================
# STEP 3 — Combine + derive output columns
# ============================================================

all_matched <- bind_rows(name_matched, spatial_matched) %>%
  mutate(
    initiatives_committed_binary = if_else(
      !is.na(initiatives_committed) & initiatives_committed != "", 1L, 0L
    ),
    initiatives_committed_count = count_initiatives(initiatives_committed)
  ) 

# ============================================================
# STEP 4 — Full ghsl output (all rows, 0 for unmatched)
# ============================================================

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
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025,initiatives_committed, initiatives_committed_binary, initiatives_committed_count, match_method)

# ============================================================
# STEP 5 — Summary + non-matched report
# ============================================================

non_matched <- ghsl_final %>%
  filter(match_method == "unmatched") %>%
  select(ID_UC_G0, GC_UCN_MAI_2025, GC_CNT_GAD_2025)

cat(sprintf("\n========== SUMMARY ==========\n"))
cat(sprintf("Total ghsl rows      : %d\n", nrow(ghsl_final)))
cat(sprintf("Matched (name)       : %d\n", sum(ghsl_final$match_method == "name")))
cat(sprintf("Matched (spatial)    : %d\n", sum(ghsl_final$match_method == "spatial")))
cat(sprintf("Unmatched            : %d\n", nrow(non_matched)))
cat(sprintf("Ambiguous logged     : %d candidate rows across %d ghsl cities\n",
            nrow(ambiguity_log), n_distinct(ambiguity_log$ID_UC_G0)))
cat(sprintf("With initiatives     : %d ghsl cities\n",
            sum(ghsl_final$initiatives_committed_binary)))

cat("\n-- initiatives_committed_binary distribution --\n")
print(table(ghsl_final$initiatives_committed_binary, useNA = "always"))

cat("\n-- Ambiguity reasons by step --\n")
if (nrow(ambiguity_log) > 0) {
  print(count(ambiguity_log, match_step, ambiguity_reason))
} else {
  cat("  (none)\n")
}

cat("\n-- Top 10 non-matched ghsl cities --\n")
print(head(non_matched, 10))

# ============================================================
# STEP 6 — Save outputs
# ============================================================

write_csv(ghsl_final,    "ghsl_final_with_initiatives.csv")
write_csv(non_matched,   "ghsl_non_matched.csv")
write_csv(ambiguity_log, "ghsl_ambiguous_matches.csv")

cat("\nFiles written:\n")
cat("  ghsl_final_with_initiatives.csv  — full ghsl with initiative columns\n")
cat("  ghsl_non_matched.csv             — ghsl cities with no ctx match\n")
cat("  ghsl_ambiguous_matches.csv       — all candidate rows for ambiguous cities\n")