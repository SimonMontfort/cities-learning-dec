library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(countrycode)
library(ggplot2)
library(patchwork)

theme_SM <- function(){
  theme_light() +
    theme(
      panel.grid        = element_blank(),
      panel.border      = element_rect(colour = "grey50", fill = NA, linewidth = .5),
      strip.placement   = "outside",
      text              = element_text(size = 12),
      axis.text.x       = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
      axis.text.y       = element_text(colour = "grey30"),
      axis.ticks.length = unit(.2, "cm"),
      axis.ticks        = element_line(colour = "grey50", linewidth = .5),
      strip.background  = element_rect(fill = "white"),
      strip.text        = element_text(colour = "black"),
      strip.clip        = "off",
      legend.text       = element_text(size = 7),
      legend.key.size   = unit(.4, "cm"),
      legend.position   = c(0.9, .05),
      legend.margin     = margin(rep(2, 4)),
      legend.title      = element_blank(),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", size = .3,
                                       linetype = "solid", colour = "grey")
    )
}

# ── 0. Load data ──────────────────────────────────────────────────────────────
setwd("/Users/simon/Documents/repo/cities-learning-dec")

ucdb <- read_xlsx("data/case_selection/case_selection_and_literature.xlsx",
                  sheet = 1, na = c("", "#N/A"))
cclw <- read_csv("data/CCLW/cclw-2026-04-21.csv", show_col_types = FALSE)

# ── 1. Document-type classification ───────────────────────────────────────────
# Both flags are coded independently; compound types (Decision And Plan,
# Law And Plan) receive a 1 on each flag.
#
# STRATEGIC: directional, non-binding planning instruments.
strategic_types <- c(
  "Strategy", "Programme", "Action Plan", "Plan", "Roadmap", "Policy",
  "Agenda", "Vision", "Decision And Plan", "Law And Plan",
  "National Adaptation Plan", "Framework", "Strategic Assessment",
  "National Biodiversity Strategy And Action Plan (Nbsap)"
)

# LAWS & DECREES: legally binding / enforceable instruments.
laws_decrees_types <- c(
  "Act", "Law", "Decree", "Decree Law", "Royal Decree",
  "Regulation", "Ordinance", "Rules", "Directive", "Executive Order",
  "Order", "Resolution", "Constitution", "Bill", "Criteria",
  "Accord", "Protocol", "Eu Regulation", "Decision",
  "Decision And Plan", "Law And Plan"
)

# Types coded as neither (informational / process):
# Publication, Report, Progress Report, Press Release, Statement,
# Discussion Paper, Assessment, Annex, Publication/Report

# ── 2. Filter and classify CCLW ───────────────────────────────────────────────
cclw_clean <- cclw |>
  filter(
    `Document Role` == "Main",
    Category %in% c("Legislative", "Executive"),
    !Geographies %in% c("No Geography", "European Union", "International")
  ) |>
  mutate(
    is_strategic    = `Document Type` %in% strategic_types,
    is_laws_decrees = `Document Type` %in% laws_decrees_types
  )

# ── 3. Country-level counts ───────────────────────────────────────────────────
country_gov <- cclw_clean |>
  group_by(Geographies) |>
  summarise(
    n_total         = n(),
    n_legislative   = sum(Category == "Legislative"),
    n_executive     = sum(Category == "Executive"),
    n_strategic     = sum(is_strategic),
    n_laws_decrees  = sum(is_laws_decrees),
    .groups = "drop"
  )

# Sanity checks
stopifnot(
  all(country_gov$n_legislative + country_gov$n_executive == country_gov$n_total)
  # n_strategic + n_laws_decrees can exceed n_total due to double-coding of
  # compound types; no sum check here by design.
)

# ── 4. ISO3 bridge ────────────────────────────────────────────────────────────
add_iso3 <- function(df, name_col) {
  df |>
    mutate(iso3 = countrycode(.data[[name_col]],
                              origin      = "country.name",
                              destination = "iso3c",
                              warn        = FALSE))
}

country_gov <- add_iso3(country_gov, "Geographies")

ucdb <- add_iso3(ucdb, "country_name") |>
  mutate(
    iso3 = ifelse(country_name == "México", "MEX", iso3),
    iso3 = ifelse(country_name == "Kosovo", "XKX", iso3)
  )

# ── 5. Territory imputation via parent country ────────────────────────────────
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
    country_gov |> select(iso3, n_total, n_legislative, n_executive,
                          n_strategic, n_laws_decrees),
    by = c("parent_iso3" = "iso3")
  ) |>
  select(-parent_iso3)

# ── 6. Join to urban centres ──────────────────────────────────────────────────
ucdb_gov <- ucdb |>
  left_join(country_gov |> select(-Geographies), by = "iso3") |>
  rows_patch(parent_descriptors, by = "iso3", unmatched = "ignore")

# ── 7. Diagnostics ────────────────────────────────────────────────────────────
cat("=== Overall coverage ===\n")
ucdb_gov |>
  summarise(
    n_cities    = n(),
    n_matched   = sum(!is.na(n_total)),
    pct_matched = round(mean(!is.na(n_total)) * 100, 1)
  ) |>
  print()

cat("\n=== Missing by region ===\n")
ucdb_gov |>
  group_by(region) |>
  summarise(pct_missing = round(mean(is.na(n_total)) * 100, 1),
            .groups = "drop") |>
  arrange(desc(pct_missing)) |>
  print()

cat("\n=== Unmatched countries ===\n")
ucdb_gov |>
  filter(is.na(n_total)) |>
  distinct(country_name, iso3, region) |>
  print(n = 50)

cat("\n=== Country-level descriptor summary ===\n")
country_gov |>
  select(n_total, n_legislative, n_executive, n_strategic, n_laws_decrees) |>
  summary() |>
  print()

cat("\n=== Document types coded as NEITHER flag ===\n")
cclw_clean |>
  filter(!is_strategic, !is_laws_decrees) |>
  count(`Document Type`, sort = TRUE) |>
  print(n = 30)

# ── 8. Save outputs ───────────────────────────────────────────────────────────
ucdb_gov |>
  select(city_id, iso3,
         n_total, n_legislative, n_executive,
         n_strategic, n_laws_decrees) |>
  write_csv("data/CCLW/ucdb_governance_descriptors.csv", na = "")

cat("\nSaved: data/CCLW/ucdb_governance_descriptors.csv\n")

# ── 9. Boxplots by cluster type ───────────────────────────────────────────────
pal_type <- c("Type 1" = "#E69F00", "Type 2" = "#009E97",
              "Type 3" = "#D55E00", "Type 4" = "#0072B2")

ucdb_case <- ucdb_gov |>
  filter(!is.na(cluster_name), !is.na(n_total))

gov_vars <- c(
  "n_total"        = "Total",
  "n_legislative"  = "Legislative",
  "n_executive"    = "Executive",
  "n_strategic"    = "Strategic",
  "n_laws_decrees" = "Laws & decrees"
)

ucdb_long <- ucdb_case |>
  select(cluster_name, all_of(names(gov_vars))) |>
  pivot_longer(
    cols      = all_of(names(gov_vars)),
    names_to  = "variable",
    values_to = "n"
  ) |>
  mutate(
    variable = factor(recode(variable, !!!gov_vars),
                      levels = unname(gov_vars))
  )

p_box <- ggplot(ucdb_long,
                aes(x = cluster_name, y = n,
                    fill = cluster_name, colour = cluster_name)) +
  geom_boxplot(
    alpha         = 0.3,
    linewidth     = 0.4,
    width         = 0.5,
    outlier.size  = 0.5,
    outlier.alpha = 0.3
  ) +
  scale_fill_manual(values = pal_type) +
  scale_colour_manual(values = pal_type) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    x       = NULL,
    y       = "Count",
    title   = "National climate governance activity by city typology",
    caption = paste0(
      "CCLW 2026; UNFCCC process documents excluded. Country-level descriptors assigned to all cities within country.\n",
      "Legislative/Executive distinction reflects enacting branch following CCLW definition.\n",
      "Strategic and Laws & decrees are coded independently; compound types (Decision And Plan, Law And Plan) ",
      "contribute to both counts.\n",
      "Strategic = Strategy, Plan, Action Plan, Programme, Roadmap, Policy, Agenda, Vision, Framework, ",
      "Strategic Assessment, National Adaptation Plan, NBSAP, Decision And Plan, Law And Plan.\n",
      "Laws & decrees = Act, Law, Decree, Decree Law, Royal Decree, Regulation, Ordinance, Rules, Directive, ",
      "Executive Order, Order, Resolution, Constitution, Bill, Criteria, Accord, Protocol, EU Regulation, ",
      "Decision, Decision And Plan, Law And Plan."
    )
  ) +
  theme_SM() +
  theme(legend.position = c(0.6, .9))

