library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(countrycode)
library(ggplot2)
library(patchwork)
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)

# ── 0. Load data ──────────────────────────────────────────────────────────────
setwd("/Users/simon/Documents/repo/cities-learning-dec")

ucdb <- read_xlsx("data/case_selection/case_selection_and_literature.xlsx",
                  sheet = 1, na = c("", "#N/A"))

data("vdem")
vdem <- vdem |> as_tibble()

# ── 1. Most recent non-NA observation per country ─────────────────────────────
# v2x_feduni: federalism/unitarism index (0-1), subnational authority dispersion
# v2x_libdem: liberal democracy index (0-1), institutional quality + rule of law
# Note: v2x_feduni runs to 2023 per codebook; v2x_libdem to 2024/2025.
# Taking last non-NA per variable independently to maximise coverage.
vdem_gov <- vdem |>
  select(country_name, country_text_id, year,
         v2x_feduni, v2x_libdem) |>
  filter(year == 2025) |>
  select(iso3 = country_text_id, feduni = v2x_feduni, libdem = v2x_libdem, year)

# ── 2. ISO3 bridge for ucdb ───────────────────────────────────────────────────
ucdb <- ucdb |>
  mutate(
    iso3 = countrycode(country_name, origin = "country.name",
                       destination = "iso3c", warn = FALSE),
    iso3 = ifelse(country_name == "México", "MEX", iso3),
    iso3 = ifelse(country_name == "Kosovo", "XKX", iso3)
  )

# ── 3. Territory imputation via parent country ────────────────────────────────
territory_parent <- tribble(
  ~iso3,  ~parent_iso3,
  "PYF",  "FRA",
  "ABW",  "NLD",
  "CUW",  "NLD",
  "PRI",  "USA",
  "MTQ",  "FRA",
  "GUF",  "FRA",
  "ESH",  NA,
  "JEY",  "GBR",
  "MYT",  "FRA",
  "REU",  "FRA",
  "NCL",  "FRA"
)

parent_descriptors <- territory_parent |>
  filter(!is.na(parent_iso3)) |>
  left_join(
    vdem_gov |> select(iso3, feduni, year, libdem),
    by = c("parent_iso3" = "iso3")
  ) |>
  select(-parent_iso3)

# ── 4. Join to urban centres ──────────────────────────────────────────────────
ucdb_vdem <- ucdb |>
  left_join(vdem_gov, by = "iso3") |>
  rows_patch(parent_descriptors, by = "iso3", unmatched = "ignore")

# ── 5. Diagnostics ────────────────────────────────────────────────────────────
cat("=== Overall coverage ===\n")
ucdb_vdem |>
  summarise(
    n_cities          = n(),
    n_feduni          = sum(!is.na(feduni)),
    pct_feduni        = round(mean(!is.na(feduni)) * 100, 1),
    n_libdem          = sum(!is.na(libdem)),
    pct_libdem        = round(mean(!is.na(libdem)) * 100, 1)
  ) |>
  print()

cat("\n=== Missing by region ===\n")
ucdb_vdem |>
  group_by(region) |>
  summarise(
    pct_missing_feduni = round(mean(is.na(feduni)) * 100, 1),
    pct_missing_libdem = round(mean(is.na(libdem)) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_missing_feduni)) |>
  print()

cat("\n=== Unmatched countries ===\n")
ucdb_vdem |>
  filter(is.na(feduni)) |>
  distinct(country_name, iso3, region) |>
  print(n = 50)

cat("\n=== Variable distributions (matched cities) ===\n")
ucdb_vdem |>
  summarise(
    across(c(feduni, libdem),
           list(min = ~min(., na.rm = TRUE),
                median = ~median(., na.rm = TRUE),
                mean = ~mean(., na.rm = TRUE),
                max = ~max(., na.rm = TRUE),
                n_NA = ~sum(is.na(.))),
           .names = "{.col}_{.fn}")
  ) |>
  print()

# ── 6. Save ───────────────────────────────────────────────────────────────────
ucdb_vdem |>
  select(city_id, iso3,
         feduni, year,
         libdem) |>
  write_csv("data/vdem/ucdb_vdem_descriptors.csv", na = "", )

cat("\nSaved: data/vdem/ucdb_vdem_descriptors.csv\n")

# ── 7. Boxplots by cluster type ───────────────────────────────────────────────
pal_type <- c("Type 1" = "#E69F00", "Type 2" = "#009E97",
              "Type 3" = "#D55E00", "Type 4" = "#0072B2")

var_labels <- c(
  feduni = "Federalism index (v2x_feduni)\nSubnational authority dispersion",
  libdem = "Liberal democracy index (v2x_libdem)\nInstitutional quality & rule of law"
)

ucdb_case <- ucdb_vdem |>
  filter(!is.na(cluster_name)) |>
  select(cluster_name, feduni, libdem) |>
  pivot_longer(c(feduni, libdem),
               names_to = "variable", values_to = "value") |>
  mutate(variable = factor(recode(variable, !!!var_labels),
                           levels = unname(var_labels)))

p_vdem <- ggplot(ucdb_case,
                 aes(x = cluster_name, y = value,
                     fill = cluster_name, colour = cluster_name)) +
  geom_boxplot(
    alpha         = 0.3,
    linewidth     = 0.4,
    width         = 0.5,
    outlier.size  = 0.5,
    outlier.alpha = 0.3
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = pal_type) +
  scale_colour_manual(values = pal_type) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    x        = NULL,
    y        = "Index value (0–1)",
    title    = "Subnational governance structure and institutional quality by city typology",
    subtitle = "V-Dem; most recent non-NA observation per country (post-2000). Country-level descriptors assigned to all cities within country.",
    caption  = paste0(
      "v2x_feduni: equally weighted average of elected local and regional government indices ",
      "(existence × election × relative power).\n",
      "v2x_libdem: liberal component index capturing electoral democracy, ",
      "rule of law, and individual liberties. Source: V-Dem v14."
    )
  ) +
  theme_SM()

ggsave("plots/vdem_by_type.pdf", p_vdem,
       width = 10, height = 5)

cat("Saved: plots/vdem_by_type.pdf\n")