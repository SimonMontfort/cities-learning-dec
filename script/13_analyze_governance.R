library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(ggplot2)
library(patchwork)

# ── 0. Paths ──────────────────────────────────────────────────────────────────
setwd("/Users/simon/Documents/repo/cities-learning-dec")

# ── 1. Load ucdb ──────────────────────────────────────────────────────────────
ucdb <- read_xlsx(
  "data/case_selection/case_selection_and_literature.xlsx",
  sheet = 1, na = c("", "#N/A")
) %>%
  select(city_id, country_name, cluster_name, region)

# ── 2. Load descriptor files ──────────────────────────────────────────────────
cclw <- read_csv("data/CCLW/ucdb_governance_descriptors.csv",
                 show_col_types = FALSE) %>%
  select(city_id, n_total, n_legislative, n_executive, n_strategic, n_laws_decrees)

vdem <- read_csv("data/vdem/ucdb_vdem_descriptors.csv",
                 show_col_types = FALSE) %>%
  select(city_id, feduni, libdem)

climactor <- read_csv("data/c2cNW/ghsl_final_with_initiatives.csv",
                      show_col_types = FALSE) %>%
  select(city_id = ID_UC_G0, initiatives_committed_binary, initiatives_committed_count)

# ── 3. Merge ──────────────────────────────────────────────────────────────────
gov <- ucdb %>%
  left_join(cclw,      by = "city_id") %>%
  left_join(vdem,      by = "city_id") %>%
  left_join(climactor, by = "city_id")

# ── 4. Shared setup ───────────────────────────────────────────────────────────
theme_SM <- function(){
  theme_light() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill = NA, linewidth = .5),
          strip.placement = "outside",
          text = element_text(size = 12),
          axis.text.x = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(colour = "grey30"),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth = .5),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.9, .05),
          legend.margin = margin(rep(2, 4)),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", linewidth = .3,
                                           linetype = "solid", colour = "grey")
    )
}

pal_type <- c(
  "Type 1" = "#E41A1C",
  "Type 2" = "#377EB8",
  "Type 3" = "#4DAF4A",
  "Type 4" = "#984EA3"
)


gov_case <- gov %>% filter(!is.na(cluster_name))

var_meta <- tribble(
  ~variable,                     ~label,
  "n_total",                     "National:\nTotal policy output",
  "n_laws_decrees",              "National:\nLegislative & regulatory",
  "n_strategic",                 "National:\nStrategic & planning",
  "feduni",                      "National:\nDivision of power index",
  "libdem",                      "National:\nLiberal democracy index",
  "pct_network",                 "Urban:\n% urban centres in transnational networks"
)

# ── 5. Country-type aggregation ───────────────────────────────────────────────
# One row per country × type combination.
# National indicators take first value (identical within country).
# Urban indicator aggregated as % of network-member urban centres.
country_type <- gov_case %>%
  group_by(country_name, cluster_name) %>%
  summarise(
    n_urban_centres       = n(),
    n_total        = first(n_total),
    n_laws_decrees = first(n_laws_decrees),
    n_strategic    = first(n_strategic),
    feduni         = first(feduni),
    libdem         = first(libdem),
    pct_network    = 100 * mean(initiatives_committed_binary == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Long form
ct_long <- country_type %>%
  pivot_longer(cols = all_of(var_meta$variable),
               names_to = "variable", values_to = "value") %>%
  left_join(var_meta, by = "variable") %>%
  filter(!is.na(value)) %>%
  mutate(label = factor(label, levels = var_meta$label)) %>% 
  mutate(iso3 = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  mutate(iso3 = ifelse(country_name == "México", "MEX", iso3),
         iso3 = ifelse(country_name == "Kosovo", "XKX", iso3),)

# ── OPTION 1 — Type-level aggregated bar + IQR ────────────────────────────────
# Collapse to type medians with IQR bars. Loses country detail entirely.
type_summary <- ct_long %>%
  group_by(cluster_name, label) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    q25    = quantile(value, 0.25, na.rm = TRUE),
    q75    = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

pB <- ggplot(type_summary,
             aes(x = cluster_name, y = median,
                 fill = cluster_name)) +
  geom_col(alpha = 0.8, width = 0.6, linewidth = .2, col = "black" ) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.2, linewidth = 0.5, col = "black") +
  scale_fill_manual(values = pal_type) +
  scale_colour_manual(values = pal_type) +
  facet_wrap(~ label, scales = "free_y", nrow = 2) +
  labs(
    x       = NULL, y = NULL,
    # title   = "Option B: Type-level median + IQR bar",
    # subtitle = "Bar = median across country-type combinations; error bar = IQR",
    # caption = "Unit of observation: country × type. National: single value per country. Urban: % network-member urban centres."
  ) +
  theme_SM() +
  theme(legend.position = "none")

ggsave(pB, filename = "plots/fig3.pdf", width = 10, height = 7, dpi = 150)
# cat("Saved: plots/gov_option_B.png\n")

# ── OPTION 2 — Boxplot on country-type unit ───────────────────────────────────
# Boxplot but unit = country × type, not city. More honest than city-level.
pC <- ggplot(ct_long,
             aes(x = cluster_name, y = value,
                 fill = cluster_name)) +
  geom_boxplot(
    alpha = .8, linewidth = 0.4, width = 0.5,
    outlier.size = 0.6, outlier.alpha = 0.4, col = "black"
  ) +
  scale_fill_manual(values = pal_type) +
  scale_colour_manual(values = pal_type) +
  facet_wrap(~ label, scales = "free_y", nrow = 2) +
  labs(
    x       = NULL, y = NULL,
    # title   = "Option C: Boxplot on country-type unit",
    # subtitle = "Unit of observation: country × type combination (not urban centre)",
    # caption = "National indicators: single value per country. Urban: % network-member urban centres per country-type."
  ) +
  theme_SM() +
  theme(legend.position = "none")

ggsave("plots/fig3_option_2.pdf", pC, width = 10, height = 5)
