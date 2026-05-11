library(tidyverse)
library(sf)

# ── Data ──────────────────────────────────────────────────────────────────────
setwd("/Users/simon/Documents/repo/cities-learning-dec")

types <- readxl::read_xlsx("data/case_selection/data_base_clean.xlsx", sheet = 2)

ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg") |>
  as.data.frame() |>
  select(ID_UC_G0, GC_DEV_WIG_2025) |>
  as_tibble()

theme_SM <- function(){
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          text = element_text(size = 12, 
                              # family = "Myriad Pro"
          ),
          axis.text.x = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(colour = "grey30"),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth=.5),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.9,.05),
          legend.margin = margin(rep(2, 4)),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill="white", 
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
}

# ── Variable mapping ──────────────────────────────────────────────────────────
var_labels <- c(
  population                = "Population",
  population_growth         = "Population growth",
  population_density        = "Population density",
  population_density_growth = "Population density growth",
  GDP_PPP                   = "GDP PPP",
  GDP_PPP_growth            = "GDP PPP growth",
  critical_infrastructure   = "Critical infrastructure",
  heating_degree_days       = "Heating degree days",
  cooling_degree_days       = "Cooling degree days",
  human_development_index   = "HDI",
  female_gender_index       = "Gender index",
  old_pop_relative_to_young = "Old population relative to young"
)

vars <- names(var_labels)

# ── Group mappings ────────────────────────────────────────────────────────────
dev_group_map <- c(
  "Low income"   = "low",
  "Lower Middle" = "low",
  "Upper Middle" = "medium",
  "High income"  = "high"
)

type_dev_map <- c(
  "Type 1" = "low",
  "Type 2" = "medium",
  "Type 3" = "high",
  "Type 4" = "all"
)

type_dev_lookup <- tibble(
  cluster_name = names(type_dev_map),
  type_dev     = type_dev_map
)
# ── Merge & filter ────────────────────────────────────────────────────────────
df <- types |>
  # filter(main_or_mixed_type == "main type") |>
  left_join(ghsl |> select(ID_UC_G0, GC_DEV_WIG_2025), by = c("city_id" = "ID_UC_G0")) |>
  mutate(dev_collapsed = dev_group_map[GC_DEV_WIG_2025]) |>
  select(city_id, cluster_name, dev_collapsed, all_of(vars))

# Type 4 gets duplicated once per income group
df_type4_expanded <- df |>
  filter(cluster_name == "Type 4") |>
  select(-dev_collapsed) |>
  cross_join(tibble(type_dev = c("low", "medium", "high")))

df_other <- df |>
  filter(cluster_name != "Type 4") |>
  mutate(type_dev = type_dev_map[cluster_name])

df_long <- bind_rows(df_other, df_type4_expanded)

# ── Scale within dev-group ────────────────────────────────────────────────────
scale_percentile <- function(x) 1 + 99 * ecdf(x)(x)

df_scaled <- df_long |>
  group_by(type_dev) |>
  mutate(across(all_of(vars), scale_percentile)) |>
  ungroup()

# ── Type medians in percentile space ──────────────────────────────────────────
type_medians_scaled <- df_scaled |>
  group_by(cluster_name, type_dev) |>
  summarise(across(all_of(vars), \(x) median(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(all_of(vars), names_to = "variable", values_to = "type_med")

# ── Dev-group medians in percentile space ─────────────────────────────────────
dev_medians_scaled <- df_scaled |>
  group_by(type_dev) |>
  summarise(across(all_of(vars), \(x) median(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(all_of(vars), names_to = "variable", values_to = "dev_med")

# ── Compute differences ───────────────────────────────────────────────────────
diffs <- type_medians_scaled |>
  left_join(dev_medians_scaled, by = c("type_dev", "variable")) |>
  mutate(
    diff           = type_med - dev_med,
    variable_label = var_labels[variable],
    variable_label = factor(variable_label, levels = rev(var_labels)),
    # display label: Type 4 gets income-group suffix, others get ref label
    cluster_label  = case_when(
      cluster_name == "Type 4" & type_dev == "low"    ~ "Type 4 vs. low income",
      cluster_name == "Type 4" & type_dev == "medium" ~ "Type 4 vs. middle income",
      cluster_name == "Type 4" & type_dev == "high"   ~ "Type 4 vs. high income",
      cluster_name == "Type 1" ~ "Type 1\n(low income ref.)",
      cluster_name == "Type 2" ~ "Type 2\n(middle income ref.)",
      cluster_name == "Type 3" ~ "Type 3\n(high income ref.)"
    )
  )

# ── Palette ───────────────────────────────────────────────────────────────────
type_colours <- c(
  "Type 1\n(low income ref.)"  = "#E41A1C",
  "Type 2\n(middle income ref.)" = "#377EB8",
  "Type 3\n(high income ref.)"  = "#4DAF4A",
  "Type 4 vs. low income"      = "#D4537E",
  "Type 4 vs. middle income"   = "#984EA3",
  "Type 4 vs. high income"     = "#C0397A"
)

type_shapes <- c(
  "Type 1\n(low income ref.)"    = 16,
  "Type 2\n(middle income ref.)" = 16,
  "Type 3\n(high income ref.)"   = 16,
  "Type 4 vs. low income"        = 17,
  "Type 4 vs. middle income"     = 17,
  "Type 4 vs. high income"       = 17
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(
  diffs |> mutate(panel = if_else(cluster_name == "Type 4", "Type 4 (megacities)", "Types 1–3")),
  aes(x = diff, y = variable_label, colour = cluster_label, shape = cluster_label)
) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey60", linetype = "dashed") +
  geom_point(size = 2.5, alpha = 0.9,
             position = position_dodge(width = 0.7)) +
  geom_linerange(aes(xmin = 0, xmax = diff),
                 linewidth = 0.7, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  scale_colour_manual(values = type_colours, name = NULL) +
  scale_shape_manual(values = type_shapes, name = NULL) +
  scale_x_continuous(
    limits = c(-55, 55),
    breaks = seq(-50, 50, 5),
    labels = \(x) paste0(ifelse(x > 0, "+", ""), x)
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    x       = "Percentile points above/below matched income-group median",
    y       = NULL,
    caption = "Percentiles computed within matched income group. Type 4 (megacities) shown separately for each income group comparison."
  ) +
  theme_SM()

p

library(patchwork)

# ── Shared colour scale for income groups ─────────────────────────────────────
inc_colours <- c(
  "low"    = "#E41A1C",
  "medium" = "#377EB8",
  "high"   = "#4DAF4A"
)
inc_labels <- c(
  "low"    = "Low income",
  "medium" = "Middle income",
  "high"   = "High income"
)

type_colours_base <- c(
  "Type 1" = "#E41A1C",
  "Type 2" = "#377EB8",
  "Type 3" = "#4DAF4A",
  "Type 4" = "#984EA3"
)

# ── Plot 1: Flipped reference ─────────────────────────────────────────────────
# For each income group × variable: where does the income-group median sit
# within each type's distribution?

df_long_typed <- df_long |>
  mutate(type_dev = if_else(cluster_name == "Type 4", type_dev, type_dev_map[cluster_name]))

# Scale within type (not income group)
df_scaled_bytype <- df_long |>
  group_by(cluster_name) |>
  mutate(across(all_of(vars), scale_percentile)) |>
  ungroup()

# Income-group medians in type-scaled space
inc_med_in_type <- df_scaled_bytype |>
  group_by(cluster_name, type_dev) |>
  summarise(across(all_of(vars), \(x) median(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(all_of(vars), names_to = "variable", values_to = "inc_med") |>
  mutate(
    diff           = inc_med - 50,
    variable_label = var_labels[variable],
    variable_label = factor(variable_label, levels = rev(var_labels)),
    inc_label      = inc_labels[type_dev],
    inc_label      = factor(inc_label, levels = inc_labels)
  ) |>
  filter(!is.na(type_dev))

p1 <- ggplot(
  inc_med_in_type,
  aes(x = diff, y = variable_label, colour = type_dev, shape = type_dev)
) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey60", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.9, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(xmin = 0, xmax = diff),
                 linewidth = 0.6, alpha = 0.7,
                 position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = inc_colours, labels = inc_labels, name = NULL) +
  scale_shape_manual(values = c(low = 16, medium = 17, high = 15), labels = inc_labels, name = NULL) +
  scale_x_continuous(
    # limits = c(-55, 55), breaks = seq(-50, 50, 25),
    labels = \(x) paste0(ifelse(x > 0, "+", ""), x)
  ) +
  facet_wrap(~ cluster_name, ncol = 2) +
  labs(
    x = "Percentile points above/below type median",
    y = NULL,
    title = "1) Where does each income group sit within each type?"
  ) +
  theme_SM() +
  theme(legend.position = "top")

# ── Plot 2: Side-by-side boxplots, type vs. matched income group ──────────────

var_labels_lb <- c(
  population                = "Population",
  population_growth         = "Population\ngrowth",
  population_density        = "Population\ndensity",
  population_density_growth = "Population\ndensity growth",
  GDP_PPP                   = "GDP PPP",
  GDP_PPP_growth            = "GDP PPP\ngrowth",
  critical_infrastructure   = "Critical\ninfrastructure",
  heating_degree_days       = "Heating degree\ndays",
  cooling_degree_days       = "Cooling\ndegree days",
  human_development_index   = "HDI",
  female_gender_index       = "Gender\nindex",
  old_pop_relative_to_young = "Old/young\npopulation"
)

# For Types 1-3: pull type cities and their matched income group cities
# Label them as "Type X" or "Income group ref."

make_p2_panel <- function(type_name, ref_group, ref_label, type_colour) {
  
  type_cities <- df |>
    filter(cluster_name == type_name) |>
    mutate(group = type_name)
  
  ref_cities <- df |>
    filter(dev_collapsed == ref_group, cluster_name != type_name) |>
    mutate(group = ref_label)
  
  bind_rows(type_cities, ref_cities) |>
    mutate(group = factor(group, levels = c(type_name, ref_label))) |>
    pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
    mutate(
      variable_label = var_labels_lb[variable],
      variable_label = factor(variable_label, levels = var_labels_lb)
    ) |>
    ggplot(aes(x = group, y = value, fill = group)) +
    geom_boxplot(
      width = 0.5, linewidth = 0.3, outlier.size = 0.4,
      outlier.alpha = 0.3, alpha = 0.7, outliers = FALSE,
    ) +
    scale_fill_manual(values = c(type_colour, "grey70"), name = NULL) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    facet_wrap(~ variable_label, scales = "free_y", ncol = 12) +
    labs(x = NULL, y = NULL, 
         # title = type_name
         ) +
    theme_SM() +
    theme(
      legend.position = "none",
      # axis.text.x     = element_blank(),
      # axis.ticks.x    = element_blank(),
      # strip.text      = element_text(size = 7)
    )
}

# Type 4: three panels, one per income group
make_p2_panel_t4 <- function(ref_group, ref_label, panel_colour) {
  
  type_cities <- df |>
    filter(cluster_name == "Type 4") |>
    mutate(group = "Type 4")
  
  ref_cities <- df |>
    filter(dev_collapsed == ref_group, cluster_name != "Type 4") |>
    mutate(group = ref_label)
  
  bind_rows(type_cities, ref_cities) |>
    mutate(group = factor(group, levels = c("Type 4", ref_label))) |>
    pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
    mutate(
      variable_label = var_labels_lb[variable],
      variable_label = factor(variable_label, levels = var_labels_lb)
    ) |>
    ggplot(aes(x = group, y = value, fill = group)) +
    geom_boxplot(
      width = 0.5, linewidth = 0.3, outlier.size = 0.4,
      outlier.alpha = 0.3, alpha = 0.7, outliers = FALSE,
    ) +
    scale_fill_manual(values = c(panel_colour, "grey70"), name = NULL) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    facet_wrap(~ variable_label, scales = "free_y", ncol = 12) +
    labs(x = NULL, y = NULL, 
         # title = paste0("Type 4 vs. ", ref_label)
         ) +
    theme_SM() +
    theme(
      # plot.title = element_text(hjust = -.1, vjust=-.5),
      legend.position = "none"
    )
}

p2_t1 <- make_p2_panel("Type 1", "low",    "Low inc.",    "#E41A1C")
p2_t2 <- make_p2_panel("Type 2", "medium", "Middle inc.", "#377EB8")
p2_t3 <- make_p2_panel("Type 3", "high",   "High inc.",   "#4DAF4A")
p2_t4a <- make_p2_panel_t4("low",    "Low inc.",    "#D4537E")
p2_t4b <- make_p2_panel_t4("medium", "Middle inc.", "#984EA3")
p2_t4c <- make_p2_panel_t4("high",   "High inc.",   "#C0397A")

p_dev_group_comparison <- (p2_t1 / p2_t2 / p2_t3 / p2_t4a / p2_t4b / p2_t4c) +
  plot_annotation(
    theme   = theme(plot.title = element_text(size = 12, face = "plain"))
  )

p_dev_group_comparison
ggsave(p_dev_group_comparison, file = "plots/p_dev_group_comparison.pdf", height = 13, width = 10)


# ── Summary stats: median and IQR for each type and its reference group ───────
summary_stats <- df |>
  mutate(type_dev = type_dev_map[cluster_name]) |>
  # add Type 4 expanded rows
  {\(x) bind_rows(
    x |> filter(cluster_name != "Type 4"),
    x |> filter(cluster_name == "Type 4") |>
      select(-type_dev) |>
      cross_join(tibble(type_dev = c("low", "medium", "high")))
  )}() |>
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
  group_by(cluster_name, type_dev, variable) |>
  summarise(
    n      = n(),
    median = median(value, na.rm = TRUE),
    q25    = quantile(value, 0.25, na.rm = TRUE),
    q75    = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ── Type stats ────────────────────────────────────────────────────────────────
type_stats <- bind_rows(
  df |> filter(cluster_name != "Type 4") |>
    mutate(type_dev = type_dev_map[cluster_name]),
  df |> filter(cluster_name == "Type 4") |>
    select(-dev_collapsed) |>
    cross_join(tibble(type_dev = c("low", "medium", "high")))
) |>
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
  group_by(cluster_name, type_dev, variable) |>
  summarise(
    n          = n(),
    median     = median(value, na.rm = TRUE),
    q25        = quantile(value, 0.25, na.rm = TRUE),
    q75        = quantile(value, 0.75, na.rm = TRUE),
    .groups    = "drop"
  )

# ── Reference group stats (income group cities, type own cities excluded) ─────
ref_stats <- bind_rows(
  tibble(
    cluster_name = c("Type 1", "Type 2", "Type 3"),
    type_dev     = c("low", "medium", "high")
  ),
  tibble(
    cluster_name = rep("Type 4", 3),
    type_dev     = c("low", "medium", "high")
  )
) |>
  pmap_dfr(\(cluster_name, type_dev) {
    df |>
      filter(dev_collapsed == type_dev, cluster_name != !!cluster_name) |>
      pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
      group_by(variable) |>
      summarise(
        ref_n      = n(),
        ref_median = median(value, na.rm = TRUE),
        ref_q25    = quantile(value, 0.25, na.rm = TRUE),
        ref_q75    = quantile(value, 0.75, na.rm = TRUE),
        .groups    = "drop"
      ) |>
      mutate(cluster_name = cluster_name, type_dev = type_dev)
  })

# ── Join and compute differences ──────────────────────────────────────────────
comparison <- type_stats |>
  left_join(ref_stats, by = c("cluster_name", "type_dev", "variable")) |>
  mutate(
    median_diff_pct = (median - ref_median) / ref_median * 100,
    variable_label  = var_labels[variable]
  ) |>
  arrange(cluster_name, type_dev, desc(abs(median_diff_pct)))

comparison |>
  select(cluster_name, type_dev, variable_label,
         median, ref_median, median_diff_pct,
         q25, q75, ref_q25, ref_q75) |>
  print(n = Inf)

# ── Heatmap: percentage overlap between types and income groups ───────────────
overlap <- df |>
  filter(!is.na(dev_collapsed)) |>
  count(cluster_name, dev_collapsed) |>
  group_by(cluster_name) |>
  mutate(pct_within_type = n / sum(n) * 100) |>
  ungroup() |>
  group_by(dev_collapsed) |>
  mutate(pct_within_inc = n / sum(n) * 100) |>
  ungroup() |>
  mutate(pct_overall = n / sum(n) * 100) |>
  mutate(
    cluster_name  = factor(cluster_name, levels = c("Type 1", "Type 2", "Type 3", "Type 4")),
    dev_collapsed = factor(dev_collapsed, levels = c("low", "medium", "high"),
                           labels = c("Low income", "Middle income", "High income"))
  )

# ── Shared tile base ──────────────────────────────────────────────────────────
base_theme <- list(
  theme_SM(),
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )
)

# ── Panel 1: % within type ────────────────────────────────────────────────────
p_ov1 <- ggplot(overlap, aes(x = dev_collapsed, y = cluster_name, fill = pct_within_type)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(
    label  = paste0(round(pct_within_type, 1), "%"),
    colour = pct_within_type > 50
  ), size = 3) +
  scale_fill_gradient2(
    low = "white", mid = "#FFFBCC", high = "#377EB8",
    midpoint = 1, limits = c(0, 90), name = "% of type"
  ) +
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    title    = "% within type",
    subtitle = "Rows sum to 100%"
  ) +
  base_theme

# ── Panel 2: % within income group ───────────────────────────────────────────
p_ov2 <- ggplot(overlap, aes(x = dev_collapsed, y = cluster_name, fill = pct_within_inc)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(
    label  = paste0(round(pct_within_inc, 1), "%"),
    colour = pct_within_inc > 50
  ), size = 3) +
  scale_fill_gradient2(
    low = "white", mid = "#FFFBCC", high = "#377EB8",
    midpoint = 1, limits = c(0, 92), name = "% of income group"
  ) +
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    title    = "% within income group",
    subtitle = "Columns sum to 100%"
  ) +
  base_theme

# ── Panel 3: % of overall total ───────────────────────────────────────────────
p_ov3 <- ggplot(overlap, aes(x = dev_collapsed, y = cluster_name, fill = pct_overall)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(
    label  = paste0(round(pct_overall, 1), "%"),
    colour = pct_overall > 15
  ), size = 3) +
  scale_fill_gradient2(
    low = "white", mid = "#FFFBCC", high = "#377EB8",
    midpoint = 1, limits = c(0, 41), name = "% of total"
  ) +
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    title    = "% of all cities",
    subtitle = "All cells sum to 100%"
  ) +
  base_theme

# ── Panel 4: raw counts ───────────────────────────────────────────────────────
p_ov4 <- ggplot(overlap, aes(x = dev_collapsed, y = cluster_name, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(
    label  = n,
    colour = n > median(overlap$n)
  ), size = 3) +
  scale_fill_gradient(
    low = "white", high = "#984EA3", name = "n cities"
  ) +
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "white"), guide = "none") +
  labs(
    x = NULL, y = NULL,
    title    = "Raw counts",
    subtitle = ""
  ) +
  base_theme

# ── Patchwork ─────────────────────────────────────────────────────────────────
p_overlap <- (p_ov3 | p_ov1) / (p_ov2 | p_ov4) +
  plot_annotation(
    theme   = theme(plot.title = element_text(size = 12, face = "plain"))
  ) 

p_overlap

# ggsave("p_overlap.pdf", p_overlap, width = 16, height = 5)
ggsave(p_overlap, file = "plots/p_overlap.pdf", width = 10, height = 10)
# 
# # ggsave("p_overlap.pdf", p_overlap, width = 12, height = 5)