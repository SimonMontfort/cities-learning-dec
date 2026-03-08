old <-readxl::  read_xlsx("/Users/simon/Documents/repo/cities-learning-dec/data/case_selection/case_selection_and_literature_formatted_v3.xlsx", sheet = 3)

new <- clust %>% 
  left_join(cluster_names) %>% 
  ungroup() %>% 
  select(GHS_urban_area_id, cluster_name_new = cluster_name, Region)


old <- old %>% 
  mutate(cluster_name = case_when(cluster_name == "Development first" ~ "Type 1",
                                  cluster_name == "Urban planning first" ~ "Type 2",
                                  cluster_name == "Mitigation first" ~ "Type 3",
                                  cluster_name == "Mega all in" ~ "Type 4")) %>% 
  select(city_id, cluster_name_old = cluster_name)


changes <- left_join(new, old, by = c("GHS_urban_area_id" = "city_id")) %>% 
  group_by(Region, cluster_name_new, cluster_name_old) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  mutate(share = n/sum(n)) %>% 
  mutate(diagonal = ifelse(cluster_name_new != cluster_name_old, share, NA)) %>% 
  ggplot(aes(cluster_name_old, cluster_name_new)) +
  geom_tile(aes(fill = diagonal), color = "white") +
  # scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "steelblue")) +
  geom_text(aes(label = paste0(n, "\n(", round(share*100), "%)")), color = "white") +
  facet_wrap(~Region) +
  theme_SM()

ggsave("plots/changes.pdf", changes, height = 10, width = 10)
