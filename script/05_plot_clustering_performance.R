# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggsci)

setwd("/Users/simon/Documents/repo/cities-learning")

theme_set(
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          # text = element_text(size = 12, family = "Myriad Pro"),
          # axis.text.x = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
          # axis.text.y = element_text(colour = "grey30"),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth=.5  ),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.95,.27),
          legend.margin = margin(rep(2, 4)),
          # legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill="white", 
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
)



all_data <- read.csv("data/clustering_results/raw_clustering_scores.csv")


means_and_se <- all_data %>% 
  group_by(method, n_clusters) %>% 
  summarise(se_silhouette = sd(silhouette)/sqrt(n()), 
            mean_silhouette = mean(silhouette),
            se_calinski = sd(calinski)/sqrt(n()), 
            mean_calinski = mean(calinski),
            se_davies = sd(davies)/sqrt(n()), 
            mean_davies = mean(davies)) %>% 
  pivot_longer(
    cols = -c(method, n_clusters),
    names_to = c(".value", "metric"),
    names_sep = "_"
  )
  

# Create the plot with facet_wrap
p_clustering_performance_agg <- all_data %>% 
  pivot_longer(c("silhouette",  "calinski", "davies"), names_to = "metric") %>% 
  ggplot(aes(x = n_clusters, y = value, group = run_id)) +
  geom_line(alpha = .08, col = "black") +
  geom_ribbon(data = means_and_se,
            aes(x = n_clusters, ymin = mean - se*1.96, ymax = mean + se*1.96),
            inherit.aes = F, fill = "coral1", alpha = .4 , lty = 2) +
  geom_line(data = means_and_se, 
              aes(x = n_clusters, y = mean),
              inherit.aes = F, col = "coral1", alpha = .6) + 
  facet_grid(metric~method, scales = "free_y") +
  labs(x = "Number of Clusters", y = "Score") 
p_clustering_performance_agg

p_clustering_performance_agg
ggsave(p_clustering_performance_agg, file = "plots/p_clustering_performance_agg.pdf", height = 6, width = 10)


####################
# uncertainty
####################

all_data <- read_csv("data/clustering_results/raw_clustering_scores.csv")

# List files matching the pattern
files <- list.files(path = "data/clustering_results", 
                    pattern = "cluster_assignments_(DEC|KMeans)_\\d+.*\\.csv$", 
                    full.names = TRUE)

# Read each file and enrich with method and cluster number
data_list <- lapply(files, function(f) {
  df <- read.csv(f)
  
  # Extract method (DEC or KMeans)
  method <- sub(".*cluster_assignments_(DEC|KMeans)_.*", "\\1", basename(f))
  
  # Extract cluster number
  cluster_num <- sub(".*_(\\d+).*\\.csv$", "\\1", basename(f))
  
  # Add as new columns
  df$method <- method
  df$cluster_num <- as.integer(cluster_num)
  
  return(df)
})

# Combine all into one data frame
combined_df <- do.call(rbind, data_list)

# Optional: view result
head(combined_df)

chosen_n = 6

p_avg_prob_entroopy_per_cluster <- combined_df %>% 
  mutate(most_probable_cluster = most_probable_cluster + 1) %>%
  filter(cluster_num == chosen_n) %>%
  group_by(most_probable_cluster, method, cluster_num) %>% 
  summarise(mean_prob = mean(assignment_probability),
            mean_entropy = mean(entropy)) %>% 
  pivot_longer(c("mean_prob", "mean_entropy")) %>% 
  mutate(name = ifelse(name == "mean_prob", "Mean probability", "Mean entropy")) %>% 
  ggplot(aes(x = most_probable_cluster, y = value)) + 
  geom_col(col = "black", fill = "coral1", alpha = .3) +
  facet_grid(name~method) + 
  labs(x = "Cluster", y = "Score") +
  scale_x_continuous(breaks = 1:chosen_n)
p_avg_prob_entroopy_per_cluster

library(ggpubr)
p_perf_and_uncertainty <- ggarrange(p_clustering_performance_agg, p_avg_prob_entroopy_per_cluster, labels = c("a", "b"),  
align = "v", ncol = 1, heights = c(3,2.3))
p_perf_and_uncertainty
ggsave(p_perf_and_uncertainty, file = "plots/p_perf_and_uncertainty.pdf", height = 8, width = 10)


# file_path <- "clustering_models/latent_representation"
# # Load and reshape all files
# all_data <- lapply(file_paths, function(file_path) {
#   run_number <- str_extract(file_path, "run[1-4]") %>% str_extract("[1-4]") %>% as.integer()
#   df <- read_csv(file_path)
#   df_long <- df %>%
#     pivot_longer(c("DEC", "KMeans", "Hierarchical"), names_to = "Method", values_to = "Silhouette") %>%
#     mutate(Method = ifelse(Method == "DEC", paste(Method, "run", run_number), Method))
#   return(df_long)
# }) %>% bind_rows()
# 
# 
# # Create the plot with facet_wrap
# p_clustering_performance <- all_data %>% 
#   ggplot(aes(x = n_clusters, y = silhouette, group = run_id, col = run_id)) +
#   geom_line(alpha = .5) +
#   geom_point(size = 1, alpha = .5) +
#   # scale_color_manual(values = method_colors) +
#   # scale_shape_manual(values = method_shapes) +
#   facet_wrap(~method) +
#   labs(x = "Number of Clusters", y = "Silhouette Score") 
# p_clustering_performance
#   
# # Find the point for DEC run 3 at 10 clusters
# highlight_point <- all_data %>%
#   ungroup() %>% 
#   mutate(keep = method == "DEC" & n_clusters == 10) %>% 
#   mutate(n_clusters = n_clusters - abs(max(n_clusters)-min(n_clusters))/80,
#          silhouette = silhouette + abs(max(silhouette)-min(silhouette))/80) %>% 
#   filter(keep) %>% 
#   slice(1)
# 
# 
# # Add annotation and arrow to the plot
# p_clustering_performance <- p_clustering_performance +
#   geom_curve(data = highlight_point,
#              aes(x = n_clusters - 4, y = silhouette + 0.05,
#                  xend = n_clusters, yend = silhouette),
#              arrow = arrow(length = unit(0.02, "npc")), 
#              curvature = 0.3, 
#              color = "black", linewidth = 0.5) +
#   annotate("text", x = highlight_point$n_clusters - 4,
#            y = highlight_point$silhouette + 0.055,
#            label = "10 clusters chosen for run 3",
#            hjust = 0, size = 3)
# 
# p_clustering_performance
# ggsave(p_clustering_performance, file = "plots/p_clustering_performance.pdf", height = 6, width = 10)
# 

