setwd("/Users/simon/Documents/repo/cities-learning-dec")

library(tidyverse)
library(patchwork)

# ── Data ──────────────────────────────────────────────────────────────────────
orig <- readxl::read_xlsx("data/case_selection/data_base_clean.xlsx", sheet = 2) %>%
  select(city_id, cluster_name_old = cluster_name, main_or_mixed_type,
         `assignment_probability: Type 1`,
         `assignment_probability: Type 2`,
         `assignment_probability: Type 3`,
         `assignment_probability: Type 4`)

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c("Type 3", "Type 4", "Type 2", "Type 1")
) %>%
  mutate(cluster_name = factor(cluster_name,
                               levels = c("Type 1", "Type 2", "Type 3", "Type 4")))

main_or_mixed_type_robustness <- read.csv(
  "data/clustering_results/type_main_mixed.csv"
)

robustness <- read.csv(
  "robustness/appraisal/data/clustering_results/dec_clusters_k4.csv"
) %>%
  left_join(cluster_names, by = "consensus_label_majority") %>%
  select(GHS_urban_area_id, cluster_name_new = cluster_name) %>%
  left_join(
    main_or_mixed_type_robustness %>%
      select(ID_UC_G0, main_or_mixed_type_robustness = main_mixed),
    by = c("GHS_urban_area_id" = "ID_UC_G0")
  )

# ── Merge & flag changes — matched sample only ────────────────────────────────
df_full <- inner_join(
  robustness, orig,
  by = c("GHS_urban_area_id" = "city_id")
) %>%
  mutate(
    changed  = cluster_name_new != cluster_name_old,
    max_prob = pmax(`assignment_probability: Type 1`,
                    `assignment_probability: Type 2`,
                    `assignment_probability: Type 3`,
                    `assignment_probability: Type 4`),
    prob_bin = cut(max_prob,
                   breaks = c(0, 0.5, 0.7, 0.9, 1.0),
                   labels = c("<0.5", "0.5–0.7", "0.7–0.9", ">0.9"),
                   include.lowest = TRUE),
    cluster_label = if_else(main_or_mixed_type == "mixed",
                            "mixed", as.character(cluster_name_old)),
    cluster_label = factor(cluster_label,
                           levels = c("Type 1", "Type 2",
                                      "Type 3", "Type 4", "mixed"))
  )

# ── Sample size report ────────────────────────────────────────────────────────
n_orig       <- nrow(orig)
n_robustness <- nrow(robustness)
n_matched    <- nrow(df_full)

cat(sprintf(
  "Original: %d | Robustness: %d | Matched: %d | Dropped: %d (%.1f%%)\n",
  n_orig, n_robustness, n_matched,
  n_orig - n_matched, (n_orig - n_matched) / n_orig * 100
))

# ── Summary statistics ────────────────────────────────────────────────────────
pct_overall <- mean(df_full$changed, na.rm = TRUE) * 100
pct_main    <- mean(df_full$changed[df_full$main_or_mixed_type == "main type"],
                    na.rm = TRUE) * 100
pct_mixed   <- mean(df_full$changed[df_full$main_or_mixed_type == "mixed"],
                    na.rm = TRUE) * 100
n_high_conf_stable <- sum(
  df_full$main_or_mixed_type == "main type" & df_full$max_prob > 0.9,
  na.rm = TRUE
)

cat(sprintf("Overall reassigned: %.1f%%\n", pct_overall))
cat(sprintf("Main type reassigned: %.1f%%\n", pct_main))
cat(sprintf("Mixed reassigned: %.1f%%\n", pct_mixed))

# mixed only has <0.5 and 0.5-0.7 bins by definition — note for panel d
mixed_prob_bins <- df_full %>%
  filter(main_or_mixed_type == "mixed") %>%
  count(prob_bin) %>%
  filter(n > 0) %>%
  pull(prob_bin)
cat("Mixed prob bins present:", paste(mixed_prob_bins, collapse = ", "), "\n")

# ── Theme ─────────────────────────────────────────────────────────────────────
theme_SM <- function() {
  theme_light() +
    theme(
      panel.grid        = element_blank(),
      panel.border      = element_rect(colour = "grey50", fill = NA,
                                       linewidth = .5),
      strip.placement   = "outside",
      text              = element_text(size = 11),
      axis.text.x       = element_text(colour = "grey30", angle = 45,
                                       hjust = 1, vjust = 1),
      axis.text.y       = element_text(colour = "grey30"),
      axis.ticks.length = unit(.2, "cm"),
      axis.ticks        = element_line(colour = "grey50", linewidth = .5),
      strip.background  = element_rect(fill = "white"),
      strip.text        = element_text(colour = "black"),
      strip.clip        = "off",
      legend.text       = element_text(size = 7),
      legend.key.size   = unit(.4, "cm"),
      legend.title      = element_blank(),
      legend.background = element_rect(fill = "white", linewidth = .3,
                                       linetype = "solid", colour = "grey")
    )
}

type_colours_ext <- c(
  "Type 1"  = "#E41A1C",
  "Type 2"  = "#377EB8",
  "Type 3"  = "#4DAF4A",
  "Type 4"  = "#984EA3",
  "mixed"   = "grey50",
  "Overall" = "grey20"
)

# ── Confusion matrix data ─────────────────────────────────────────────────────
conf_data <- df_full %>%
  group_by(cluster_name_old, cluster_name_new, main_or_mixed_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster_name_old, main_or_mixed_type) %>%
  mutate(
    share    = n / sum(n),
    diagonal = if_else(cluster_name_new == cluster_name_old, NA_real_, share)
  ) %>%
  ungroup()

# drop Type 4 mixed — only 1 city, meaningless
conf_data_mixed <- conf_data %>%
  filter(main_or_mixed_type == "mixed")

# shared fill scale limit across both matrices
fill_max <- max(conf_data$diagonal, na.rm = TRUE)

# ── Panel A1: main type confusion matrix ──────────────────────────────────────
pA1 <- conf_data %>%
  filter(main_or_mixed_type == "main type") %>%
  ggplot(aes(x = cluster_name_old, y = cluster_name_new)) +
  geom_tile(data = ~ filter(., is.na(diagonal)),
            fill = "grey88", colour = "white", linewidth = 0.5,
            aes(x = cluster_name_old, y = cluster_name_new)) +
  geom_tile(data = ~ filter(., !is.na(diagonal)),
            aes(fill = diagonal), colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(n, "\n(", round(share * 100), "%)")),
            colour = "grey20", size = 2.8, lineheight = 0.9) +
  scale_fill_gradient(low = "#FFF5CC", high = "#E41A1C",
                      name = "Off-diagonal\nshare",
                      limits = c(0, fill_max)) +
  labs(x = "Original main type", y = "Robustness check assignment",
       title = "a) Main type assignments") +
  theme_SM() +
  theme(legend.position = "none")

# ── Panel A2: mixed type confusion matrix (Type 4 excluded) ───────────────────
pA2 <- conf_data_mixed %>%
  ggplot(aes(x = cluster_name_old, y = cluster_name_new)) +
  geom_tile(data = ~ filter(., is.na(diagonal)),
            fill = "grey88", colour = "white", linewidth = 0.5,
            aes(x = cluster_name_old, y = cluster_name_new)) +
  geom_tile(data = ~ filter(., !is.na(diagonal)),
            aes(fill = diagonal), colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(n, "\n(", round(share * 100), "%)")),
            colour = "grey20", size = 2.8, lineheight = 0.9) +
  scale_fill_gradient(low = "#FFF5CC", high = "#E41A1C",
                      name = "Off-diagonal\nshare",
                      limits = c(0, fill_max)) +
  labs(x = "Original mixed type", y = "Robustness check assignment",
       title = "b) Mixed type assignments (Type 4 excluded: n=1)") +
  theme_SM() +
  theme(legend.position = "right")

# ── Panel C: reassignment rate by type ────────────────────────────────────────
bar_data <- df_full %>%
  group_by(cluster_label) %>%
  summarise(
    n_total   = n(),
    n_changed = sum(changed, na.rm = TRUE),
    pct       = n_changed / n_total * 100,
    .groups   = "drop"
  ) %>%
  bind_rows(
    tibble(
      cluster_label = factor("Overall"),
      n_total       = nrow(df_full),
      n_changed     = sum(df_full$changed, na.rm = TRUE),
      pct           = pct_overall
    )
  ) %>%
  mutate(cluster_label = factor(cluster_label,
                                levels = c("Type 1", "Type 2", "Type 3",
                                           "Type 4", "mixed", "Overall")))

pB <- ggplot(bar_data,
             aes(x = cluster_label, y = pct, fill = cluster_label)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(",
                               n_changed, "/", n_total, ")")),
            vjust = -0.3, size = 2.8, lineheight = 0.9, colour = "grey20") +
  scale_fill_manual(values = type_colours_ext) +
  scale_y_continuous(limits = c(0, max(bar_data$pct) * 1.3),
                     labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "% reassigned",
       title = "c) Reassignment rate by original type") +
  theme_SM() +
  theme(legend.position = "none")

# ── Panel D: reassignment by probability bin ──────────────────────────────────
# mixed only has two bins — truncate line at last observed bin
prob_data <- df_full %>%
  group_by(cluster_label, prob_bin) %>%
  summarise(
    n_total   = n(),
    n_changed = sum(changed, na.rm = TRUE),
    pct       = n_changed / n_total * 100,
    .groups   = "drop"
  ) %>%
  filter(n_total >= 5)   # drop cells with <5 cities to avoid noisy points

pD <- ggplot(prob_data,
             aes(x = prob_bin, y = pct,
                 colour = cluster_label,
                 group  = cluster_label,
                 shape  = cluster_label)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 2.5) +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            vjust = -0.8, size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = type_colours_ext, name = NULL) +
  scale_shape_manual(values = c(16, 17, 15, 18, 4), name = NULL) +
  scale_y_continuous(limits = c(0, 65),
                     labels = \(x) paste0(x, "%")) +
  labs(
    x       = "Max assignment probability",
    y       = "% reassigned",
    title   = "d) Reassignment by assignment confidence",
  ) +
  theme_SM() +
  theme(
    legend.position = c(.9,.8),
    axis.text.x     = element_text(angle = 0, hjust = 0.5),
    plot.caption    = element_text(size = 7, colour = "grey50")
  )

# ── Combine ───────────────────────────────────────────────────────────────────
p_robust <- (pA1 | pA2) / (pB | pD) +
  plot_annotation(
    title   = "Robustness check: cluster stability when excluding rural settlements",
    # caption = paste0(
    #   sprintf("%.1f%%", pct_overall), " of matched cities reassigned overall (",
    #   sprintf("%.1f%%", pct_main), "% main type, ",
    #   sprintf("%.1f%%", pct_mixed), "% mixed). ",
    #   "Diagonal cells (grey) = same dominant type retained. ",
    #   n_high_conf_stable,
    #   " cities with assignment probability >0.9 show 0% reassignment. ",
    #   sprintf("%d", n_orig - n_matched),
    #   " cities (", sprintf("%.1f%%", (n_orig - n_matched) / n_orig * 100),
    #   ") absent from robustness sample (rural settlements excluded)."
    # ),
    theme = theme(
      plot.title   = element_text(size = 12, face = "plain"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

p_robust
ggsave("plots/robustness_check_diffs.pdf", p_robust, height = 10, width = 13)

# ── Cities dropped from robustness sample by type ────────────────────────────
dropped <- anti_join(
  orig %>% select(city_id, cluster_name_old, main_or_mixed_type),
  robustness %>% select(GHS_urban_area_id),
  by = c("city_id" = "GHS_urban_area_id")
)

dropped_summary <- dropped %>%
  group_by(cluster_name_old, main_or_mixed_type) %>%
  summarise(n_dropped = n(), .groups = "drop") %>%
  left_join(
    orig %>%
      group_by(cluster_name_old, main_or_mixed_type) %>%
      summarise(n_total = n(), .groups = "drop"),
    by = c("cluster_name_old", "main_or_mixed_type")
  ) %>%
  mutate(pct_dropped = n_dropped / n_total * 100)

print(dropped_summary)
