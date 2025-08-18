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

########################################
# packages, plot theme and directory
########################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggsci)
library(scales)

setwd("/Users/simon/Documents/repo/cities-learning-dec")

theme_set(
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          axis.title = element_text(size = 10),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth=.5  ),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.95,.27),
          legend.margin = margin(rep(2, 4)),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill="white", 
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
)


####################
# performance
####################
all_data <- read.csv("data/clustering_results/raw_clustering_scores.csv") %>% as_tibble()
all_data

all_data <- all_data %>% 
  mutate(method = ifelse(method == "dec", "Deep Embedded Clustering", method),
         method = ifelse(method == "hierarchical simple", "Hierarchical Clustering", method),
         method = ifelse(method == "kmeans embedded", "K-Means Clustering\non the latent space", method),
         method = ifelse(method == "kmeans simple", "K-Means Clustering\nsimple", method),
         method = factor(method, levels = c("Deep Embedded Clustering",
                                            "K-Means Clustering\non the latent space",
                                            "K-Means Clustering\nsimple",
                                            "Hierarchical Clustering"))) %>% 
  rename(Silhouette = silhouette,
         Calinski = calinski, 
         Davies = davies)

sil_means_se <- all_data %>% 
  group_by(method, n_clusters) %>% 
  summarise(se_Silhouette = sd(Silhouette)/sqrt(n()), 
            mean_Silhouette = mean(Silhouette),
            se_Calinski = sd(Calinski)/sqrt(n()), 
            mean_Calinski = mean(Calinski),
            se_Davies = sd(Davies)/sqrt(n()), 
            mean_Davies = mean(Davies)) %>% 
  pivot_longer(
    cols = -c(method, n_clusters),
    names_to = c(".value", "metric"),
    names_sep = "_"
  ) %>% 
  mutate(metric = factor(metric, levels = c("Silhouette",  "Calinski", "Davies"))) 


# Data frame for vline in only for DEC
vline_df <- data.frame(
  method = factor("Deep Embedded Clustering", levels = c("Deep Embedded Clustering",
                                                         "K-Means Clustering\non the latent space",
                                                         "K-Means Clustering\nsimple",
                                                         "Hierarchical Clustering")),
  xintercept = 4
)

# Add geom_vline using this data frame
# Create grouping labels for the legend
p_clustering_performance_agg <- all_data %>%
  pivot_longer(c("Silhouette", "Calinski", "Davies"), names_to = "metric") %>%
  mutate(metric = factor(metric, levels = c("Silhouette", "Calinski", "Davies"))) %>%
  ggplot(aes(x = n_clusters, y = value, group = run_id)) +
  
  # Grey lines for individual runs
  geom_line(aes(color = "Individual Runs"), alpha = .08) +
  
  # Vertical reference line
  geom_vline(data = vline_df,
             aes(xintercept = xintercept, color = "# clusters chosen"), lty = 3) +
  
  # Confidence interval ribbon
  geom_ribbon(data = sil_means_se,
              aes(x = n_clusters,
                  ymin = mean - se * 1.96,
                  ymax = mean + se * 1.96,
                  fill = "95% confidence interval"),
              inherit.aes = FALSE, alpha = .4) +
  
  # Mean line
  geom_line(data = sil_means_se,
            aes(x = n_clusters, y = mean, color = "Mean"),
            inherit.aes = FALSE, alpha = .8) +
  
  # Scales for colors and fills
  scale_color_manual(
    values = c(
      "Individual Runs" = "grey40",
      "Mean" = "coral2",
      "# clusters chosen" = "black"
    )
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("95% confidence interval" = "coral1")
  ) +
  
  # Adjust guides to combine legends
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  
  # Axis and theme
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_grid(metric ~ method, scales = "free_y") +
  labs(x = "Number of clusters", y = "Score") +
  theme(legend.position = c(.9,.5), 
        legend.title = element_blank())


####################
# stability
####################

stab <- read.csv("data/clustering_results/ensemble_stability_analysis.csv")
stab <- stab %>% filter(ensemble_size >2)

ari_mean_se <- stab %>% 
  group_by(ensemble_size) %>% 
  summarise(se = sd(ari_vs_prev)/sqrt(n()),
            mean = mean(ari_vs_prev)
            )

# Add geom_vline using this data frame
# Create grouping labels for the legend
p_stability <- stab %>%
  mutate(method = "Deep Embeedded Clustering") %>% 
  ggplot(aes(x = ensemble_size, y = ari_vs_prev, group = perm_id)) +
  
  # Grey lines for individual runs
  geom_line(aes(color = "Permutation"), alpha = .08) +
  # Confidence interval ribbon
  geom_ribbon(data = ari_mean_se,
              aes(x = ensemble_size,
                  ymin = mean - se * 1.96,
                  ymax = mean + se * 1.96,
                  fill = "95% CI"),
              inherit.aes = FALSE, alpha = .4) +
  
  # Mean line
  geom_line(data = ari_mean_se,
            aes(x = ensemble_size, y = mean, color = "Mean"),
            inherit.aes = FALSE, alpha = .8) +
  
  # Scales for colors and fills
  scale_color_manual(
    values = c(
      "Permutation\n(of chosen models)" = "grey40",
      "Mean" = "coral2"
    )
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("95% CI" = "coral1")
  ) +
  
  # Adjust guides to combine legends
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  
  # Axis and theme
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_grid(. ~ method) +
  labs(x = "Ensemble size\n(number of models)", y = "Adjusted Rand Index") +
  theme(legend.position = c(.9,.4), 
        legend.title = element_blank(), 
        plot.margin = unit(c(.5,.73,0,.73), "cm"))
p_stability

####################
# export results
####################

p_clust_perf_and_stabil <- ggarrange(p_clustering_performance_agg, p_stability, heights = c(3, 2), ncol = 1, align = "v", labels = c("a", "b"))

ggsave(p_clust_perf_and_stabil, file = "plots/figA1.pdf", height = 10, width = 10)

