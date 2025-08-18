
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

library(tidyverse)
library(clue) 
library(viridis)
library(sf)

################################################################################
# load data
################################################################################

dat <- read.csv("/Users/simon/Documents/repo/cities-learning-dec/data/clustering_results/dec_clusters_k4.csv")

cluster_names <- data.frame(
  consensus_label_majority = 0:3,
  cluster_name = c(
    "Mitigation first",
    "Mega all in",
    "Development first", 
    "Urban planning first"
    
  )) %>% 
  mutate(cluster_name = factor(cluster_name, levels = c("Development first",
                                                        "Mitigation first",
                                                        "Urban planning first",
                                                        "Mega all in")))


################################################################################
# custom plotting theme 
################################################################################

# Load and register a modern font (e.g., Helvetica Neue)
showtext_auto()  # Automatically use showtext for fonts

# Check available fonts
remotes::install_github("kjhealy/myriad")
myriad::import_myriad(font_family = "Myriad Pro", silent = F)
theme_SM <- function(){
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          text = element_text(size = 12, family = "Myriad Pro"),
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


################################################################################
# mean probabilities by cluster
################################################################################

# Pivot to long format
long_dat <- dat %>%
  pivot_longer(
    cols = starts_with("mean_prob_cluster_"),
    names_to = "cluster_prob",
    names_prefix = "mean_prob_cluster_",
    values_to = "mean_prob"
  ) %>%
  mutate(cluster_prob = as.integer(cluster_prob))

# Keep only rows where the mean prob column matches the assigned cluster
matched <- long_dat %>%
  filter(consensus_label_majority == cluster_prob) %>% 
  left_join(cluster_names, by = "consensus_label_majority")

p_mean_prob <- ggplot(matched, aes(x = cluster_name, y = mean_prob)) +
  geom_violin(aes(fill = cluster_name), alpha = 0.5, width = 1, color = NA) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    x = "Assigned Cluster",
    y = "Mean Assignment Probability",
    title = "Mean Assignment Probability per Cluster"
  ) +
  theme_SM() +
  theme(legend.position = "none")

ggsave(p_mean_prob, file = "plots/p_mean_prob.pdf", width = 5, height = 4)




# 
# 
# # ---------------- PARAMETERS ----------------
# num_runs <- 30
# num_clusters <- 4
# file_prefix <- "data/clustering_results/dec_soft_assignments_run"
# file_suffix <- "_clusters4.csv"
# silhouette_file <- "data/clustering_results/silhouette_scores.csv"  # store silhouettes here
# 
# # ---------------- 1. Load reference run ----------------
# ref_file <- paste0(file_prefix, 0, file_suffix)
# ref_df <- read_csv(ref_file, show_col_types = FALSE)
# ref_probs <- select(ref_df, starts_with("cluster_")) %>% as.matrix()
# ref_labels <- apply(ref_probs, 1, which.max)
# 
# aligned_soft <- list()
# aligned_soft[[1]] <- ref_probs
# 
# aligned_labels_list <- list()
# aligned_labels_list[[1]] <- ref_labels
# 
# # ---------------- 2. Align remaining runs ----------------
# for (i in 1:num_runs) {
#   file <- paste0(file_prefix, i, file_suffix)
#   df <- read_csv(file, show_col_types = FALSE)
#   
#   probs <- select(df, starts_with("cluster_")) %>% as.matrix()
#   pred_labels <- apply(probs, 1, which.max)
#   
#   contingency <- table(factor(ref_labels, levels = 1:num_clusters),
#                        factor(pred_labels, levels = 1:num_clusters))
#   cost_matrix <- max(contingency) - contingency
#   mapping <- solve_LSAP(as.matrix(cost_matrix))
#   
#   aligned_labels <- sapply(pred_labels, function(x) mapping[x])
#   aligned_labels_list[[i]] <- aligned_labels
#   aligned_soft[[i]] <- probs[, mapping]
# }
# 
# aligned_labels_df <- data.frame(GHS_urban_area_id = ref_df$GHS_urban_area_id)
# for (i in 1:(num_runs)) {
#   aligned_labels_df[[paste0("run_", i - 1)]] <- aligned_labels_list[[i]]
# }
# 
# # ---------------- 3. Load & transform silhouette scores ----------------
# silhouette_file <- "data/clustering_results/raw_clustering_scores.csv"
# 
# silhouettes_df <- read_csv(silhouette_file, show_col_types = FALSE)
# 
# # Filter for the correct method and number of clusters (matching your runs)
# target_method <- "dec"  # adjust if needed
# silhouettes <- silhouettes_df %>%
#   filter(method == target_method, n_clusters == num_clusters) %>%
#   arrange(run_id) %>%
#   pull(silhouette)
# 
# # Check alignment
# if (length(silhouettes) != (num_runs)) {
#   stop("Mismatch: silhouette scores do not match number of runs!")
# }
# 
# # Normalize and transform silhouettes into weights
# silhouettes <- (silhouettes - min(silhouettes)) / (max(silhouettes) - min(silhouettes) + 1e-8)
# alpha <- 15; beta <- 0.5
# weights <- 1 / (1 + exp(-alpha * (silhouettes - beta)))
# weights <- weights / sum(weights)
# 
# # ---------------- 4. Compute weighted consensus probabilities ----------------
# # Note: There are num_runs+1 runs (including reference)
# if (length(weights) != (num_runs)) {
#   stop("Mismatch between weights and number of runs!")
# }
# 
# weighted_soft_probs <- Reduce(`+`, Map(function(p, w) p * w, aligned_soft, weights))
# colnames(weighted_soft_probs) <- paste0("cluster_", 1:num_clusters, "_weighted_prob")
# 
# # Assign final cluster based on weighted soft probabilities
# final_labels <- apply(weighted_soft_probs, 1, which.max)
# certainty_weighted <- apply(weighted_soft_probs, 1, max)
# 
# obs_summary <- data.frame(
#   GHS_urban_area_id = ref_df$GHS_urban_area_id,
#   final_cluster = final_labels,
#   certainty = certainty_weighted
# )
# 
# obs_summary %>% 
#   group_by(final_cluster) %>% 
#   summarise(sd = sd(certainty), 
#             mean = mean(certainty))
# 
# obs_summary %>% 
#   group_by(final_cluster) %>% 
#   arrange(-certainty, .by_group = T) %>% 
#   slice(1:5)
# 
# obs_summary %>% 
#   count(final_cluster)
# 
# # ---------------- 5. Add city names ----------------
# ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")
# obs_summary <- obs_summary %>%
#   left_join(ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_UCN_MAI_2025),
#             by = c("GHS_urban_area_id" = "ID_UC_G0"))
# 
# # ---------------- 6. Select top/bottom 10 by certainty per cluster ----------------
# top_n <- 10
# obs_selected <- obs_summary %>%
#   group_by(final_cluster) %>%
#   arrange(certainty) %>%
#   slice(c(1:top_n, (n() - top_n + 1):n())) %>%
#   ungroup()
# selected_ids <- obs_selected$GHS_urban_area_id
# 
# # ---------------- 7. Extract labels for selected cities ----------------
# labels_long <- aligned_labels_df %>%
#   pivot_longer(cols = starts_with("run_"), names_to = "run", values_to = "cluster") %>%
#   mutate(run = as.integer(gsub("run_", "", run)))
# 
# labels_selected <- labels_long %>%
#   filter(GHS_urban_area_id %in% selected_ids) %>%
#   left_join(obs_selected %>% select(GHS_urban_area_id, GC_UCN_MAI_2025, final_cluster),
#             by = "GHS_urban_area_id")
# 
# # Compute counts and proportions
# assignment_summary <- labels_selected %>%
#   group_by(GC_UCN_MAI_2025, final_cluster, cluster) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   mutate(proportion = count / (num_runs + 1))
# 
# # Order cities by final_cluster then certainty
# city_order <- obs_selected %>% arrange(final_cluster, certainty) %>% pull(GC_UCN_MAI_2025)
# assignment_summary$GC_UCN_MAI_2025 <- factor(assignment_summary$GC_UCN_MAI_2025, levels = city_order)
# 
# # ---------------- 8. Visualization ----------------
# p_bar <- assignment_summary %>%
#   mutate(cluster = factor(cluster)) %>%
#   ggplot(aes(x = GC_UCN_MAI_2025, y = proportion, fill = cluster)) +
#   geom_bar(stat = "identity", position = "stack", color = "white") +
#   geom_text(aes(label = count),
#             position = position_stack(vjust = 0.5), size = 3, color = "white") +
#   facet_wrap(~ final_cluster, scales = "free_y", ncol = 1) +
#   scale_fill_viridis_d(name = "Cluster") +
#   coord_flip() +
#   theme_minimal(base_size = 12) +
#   theme(strip.text = element_text(face = "bold"),
#         axis.text.x = element_text(angle = 0)) +
#   labs(title = "Weighted Cluster Assignment Proportions Across Runs",
#        x = "City", y = "Proportion of Runs")
# 
# print(p_bar)
# 
# # Save weighted consensus results
# # write_csv(obs_summary, "weighted_observation_certainty.csv")
# # write_csv(as.data.frame(weighted_soft_probs), "weighted_soft_probabilities.csv")
