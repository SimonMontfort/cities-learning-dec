R.version
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

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(sf)
library(showtext)
library(rnaturalearth)
library(cowplot)
library(purrr)
library(arrow)
library(ggsci)
library(stringi)
library(stringr)
library(ggpubr)
library(ggtext)
library(ggpp)

################################################################################
# load data
################################################################################

clust <- read.csv("data/clustering_results/dec_clusters_k4.csv")
ghsl <- read_sf("data/GHS_UCDB_GLOBE_R2024A_V1_0/GHS_UCDB_GLOBE_R2024A_small.gpkg")

city_validation <- read_xlsx("data/case_study_indentification_validation/sample_classifier.xlsx") %>% as_tibble()

city_validation <- city_validation %>% 
  mutate(city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "New York", "New York City"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Delhi", "New Delhi"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Dar es Salaam", "Dar es-Salaam"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Rotterdam", "Rotterdam [The Hague]"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Heroica Caborca", "Heroica Caborca"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Zhuhai", "Zhuhai [Macau]"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Malmö", "Malmo"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Chuncheon-si", "Chuncheon"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Xushui", "Xushui District"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Incheon", "Seoul"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Urumqi", "Ürümqi"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Chuncheon-si", "Chuncheon"),
         city_manual_annotation = stri_replace_all_fixed(city_manual_annotation, "Chuncheon-si", "Chuncheon"),
  )


validation_cities <- city_validation %>% 
  pull(city_manual_annotation) %>% 
  c() %>% 
  stri_split_fixed(., ", ") %>% 
  unlist() 


validation_cities <- validation_cities[!is.na(validation_cities)]
validation_cities[!validation_cities %in% ghsl$GC_UCN_MAI_2025]


# studies per city
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv")

clean_places <- clean_places %>% 
  filter((city_word_match_yes | city_intersects_yes) %in% TRUE) %>%
  filter(id %in% oa$id) %>% # only deduplicated count
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id) %>% 
  distinct() 

any(gsub("https://openalex.org/", "", clean_places$id) %in% city_validation$id)

##############################################################################
# to test if cities are no longer in the GHSL UCDB 2024A
##############################################################################
ghsl$GC_UCN_MAI_2025[grepl("Rotterdam", ghsl$GC_UCN_MAI_2025)]
# 
# library(tmap)
# library(dplyr)
# 
# # --- 1. Filter all cities in country ---
# ghsl_country <- ghsl %>%
#   filter(GC_DEV_USR_2025 == "Spain")
# 
# # --- 2. Plot with tmap ---
# tm_shape(ghsl_country) +
#   tm_polygons(
#     size = 0.2,
#     border.col = "black",
#     title = "Cities in selected country"
#   ) +
#   tm_layout(
#     title = "All GHSL Cities in selected country",
#     title.size = 1.2,
#     legend.outside = TRUE,
#     frame = FALSE
#   ) +  tmap_options(check.and.fix = TRUE)


##############################################################################
# to test if cities are no longer in the GHSL UCDB 2024A
##############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggsci)
library(ggplot2)
library(sf)
library(stringi)
library(cowplot)

ghsl <- ghsl %>% 
  mutate(GC_UCN_MAI_2025 = ifelse(GC_UCN_MAI_2025 == "N/A", "", GC_UCN_MAI_2025),
         GC_UCN_MAI_2025 = stri_replace_all_regex(GC_UCN_MAI_2025, " \\[.*?\\]", ""))
# # replace special characters
# latin <- "AAAAAACEEEEIIIIDNOOOOOOUUUUYÞaaaaaaæceeeeiiiinoooooouuuuyyAaAaCcCcCcCcDdDdEeEeEeEeEeGgGgGgGgHh"
# speci <- "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞàáâãäåæçèéêëìíîïñòóôõöøùúûüýÿĀāĂăĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥ"
# ghsl$GC_UCN_MAI_2025 <- chartr(speci, latin, ghsl$GC_UCN_MAI_2025)

# oa data
file_names <- list.files(
  path = "/Users/simon/Documents/repo/cities-learning/data/OpenAlex/05_deduplicated",
  pattern = "^city_works_df_NA_abstr_added_dedup_\\d+\\.csv$",
  full.names = TRUE
)
df_list <- lapply(file_names, read.csv)
oa <- do.call(rbind, df_list)

manual <- city_validation %>% select(id, city_manual_annotation) %>% separate_rows(city_manual_annotation, sep=", ") 
# Goal: merge information on continent to the city name. Cities do not uniquely identify. 
# Hence, for that non-unique names, assign country name manually. For others, merge it form the city db.
dup_city_names <- ghsl$GC_UCN_MAI_2025[duplicated(ghsl$GC_UCN_MAI_2025)]
non_unique_ids <- manual$id[manual$city_manual_annotation %in% dup_city_names]

manual_unique <- manual[!manual$id %in% non_unique_ids,]
manual_unique <- left_join(manual_unique, ghsl %>% select(GC_UCN_MAI_2025, GC_DEV_USR_2025), by = c("city_manual_annotation" = "GC_UCN_MAI_2025"))
unique(manual_unique$GC_DEV_USR_2025)
manual_unique$GC_DEV_USR_2025[manual_unique$id == "W2136847927"] <- "Europe"
manual_unique$GC_DEV_USR_2025[manual_unique$id == "W2332573361"] <- "Asia"
manual_unique[!is.na(manual_unique$city_manual_annotation) & is.na(manual_unique$GC_DEV_USR_2025),]

manual_non_unique <- manual[manual$id %in% non_unique_ids,]
manual_non_unique$GC_DEV_USR_2025 <- NA
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W1967649154"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W1991595249"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2076608158"] <- rep("Europe", 5)
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W1991595249"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W1991595249"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2087040141"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2370479439"] <- rep("Europe", 3)
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2336869639"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2158003246"] <- rep("Northern America", 6)
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2296757888"] <- "Northern America"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2416662970"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2534406093"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2604882981"] <- rep("Europe", 5)
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2803030899"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2924337606"] <- c("Africa", "Northern America", "Europe")
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2966737849"] <- rep("Europe", 2)
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2967094539"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2886111353"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W3001060985"] <- "Europe"
manual_non_unique$GC_DEV_USR_2025[manual_non_unique$id == "W2342328412"] <- "Latin America and the Caribbean"
manual_non_unique[is.na(manual_non_unique$GC_DEV_USR_2025) & !is.na(manual_non_unique$city_manual_annotation),]

manual <- rbind(manual_unique %>% select(-geom), manual_non_unique) %>% rename(GC_DEV_USR_2025_manual_annotation = GC_DEV_USR_2025)

###########
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv")

manual %>% 
  left_join(clean_places %>% 
              mutate(id = gsub("https://openalex.org/", "", id)) %>% 
              select(GC_UCN_MAI_2025_word_match, city_word_match_id, city_word_match_yes), by = c("id", "city_manual_annotation" = ))

oa_old$id[!oa_old$id %in% gsub("https://openalex.org/", "", oa$id)]
###########



ghsl_sub <- ghsl %>% as.data.frame() %>% select(ID_UC_G0, GC_DEV_USR_2025) %>% mutate(ID_UC_G0 = as.character(ID_UC_G0))

intersection <- city_validation %>% select(id, GC_UCN_MAI_2025_intersection, city_intersection_id) %>% separate_rows(GC_UCN_MAI_2025_intersection, city_intersection_id, sep=", ")
intersection <- left_join(intersection, ghsl_sub, by = c("city_intersection_id" = "ID_UC_G0")) %>% rename(GC_DEV_USR_2025_intersection = GC_DEV_USR_2025)

word_match <- city_validation %>% select(id, GC_UCN_MAI_2025_word_match, city_word_match_id) %>% separate_rows(GC_UCN_MAI_2025_word_match, city_word_match_id, sep=", ") 
word_match <- left_join(word_match, ghsl_sub, by = c("city_word_match_id" = "ID_UC_G0")) %>% rename(GC_DEV_USR_2025_word_match = GC_DEV_USR_2025)

###################################
# prepare to compare approaches
###################################

combine_cities_by_doc <- function(method){
  l <- list()
  l_cont <- list()
  ids <- unique(c(validation_sep_row_match$id, validation_sep_row_inter$id))
  for (i in ids) {
    match <- validation_sep_row_match[validation_sep_row_match$id == i, c("GC_UCN_MAI_2025_word_match", "GC_DEV_USR_2025_word_match", "city_word_match_id")]
    int <- validation_sep_row_inter[validation_sep_row_inter$id == i, c("GC_UCN_MAI_2025_intersection", "GC_DEV_USR_2025_intersection", "city_intersection_id")]
    if (method  == "intersection"){
      res <- intersect(match$city_word_match_id, int$city_intersection_id) 
    } else if (method == "union"){
      res <- union(match$city_word_match_id, int$city_intersection_id)
    }
    res_cont <- ghsl$GC_DEV_USR_2025[ghsl$ID_UC_G0 %in% res]
    res <- ghsl$GC_UCN_MAI_2025[ghsl$ID_UC_G0 %in% res]
    l[[i]] <- res[!is.na(res)]
    l_cont[[i]] <- res_cont[!is.na(res_cont)]
  }
  out <- as.data.frame(cbind(unlist(l), unlist(l_cont), rep(names(l), times = lengths(l))))
  colnames(out) <- c("GC_UCN_MAI_2025", "GC_DEV_USR_2025", "id")
  rownames(out) <- NULL
  out
}

clean_join <- function(df){
  df %>% 
    mutate(id.x = ifelse(is.na(id.x), id.y, id.x)) %>%
    rename(id = id.x) %>% 
    select(-id.y) %>% 
    as.data.frame()
}

validation_performance <- function(df, truth, prediction, score){
  res <- df %>%
    mutate(
      prediction = ifelse(is.na({{prediction}}), "", {{prediction}}),
      truth = ifelse(is.na({{truth}}), "", {{truth}}),
      conf = case_when(truth == "" & prediction == "" ~ "True negative",
                       truth != "" & prediction == "" ~ "False negative",
                       truth == "" & prediction != "" ~ "False positive",
                       truth == prediction ~ "True positive"
      )) %>%
    group_by(conf) %>%
    reframe(n = n())
  
  TP <- if (length(res$n[res$conf == "True positive"]) != 0) {res$n[res$conf == "True positive"]} else {0}
  TN <- if (length(res$n[res$conf == "True negative"]) != 0) {res$n[res$conf == "True negative"]} else {0}
  FN <- if (length(res$n[res$conf == "False negative"]) != 0) {res$n[res$conf == "False negative"]} else {0}
  FP <- if (length(res$n[res$conf == "False positive"]) != 0) {res$n[res$conf == "False positive"]} else {0}
  
  if (score == "classification"){
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
    f1 = 2 * (precision*recall)/(precision + recall)
    
    scores <- rbind(
      cbind("Precision", precision),
      cbind("Recall", recall),
      cbind("F1", f1)
    )
  } else if (score == "confusion"){
    scores <- rbind(
      cbind("TP", TP),
      cbind("TN", TN),
      cbind("FN", FN),
      cbind("FP", FP)
    )
  }
  
  colnames(scores) <- c("name", "score")
  rownames(scores) <- 1:nrow(scores)
  scores <- as.data.frame(scores)
  
  scores$score <- round(as.numeric(scores$score), 3)
  
  return(scores)
}

validation_sep_row_inter <- full_join(manual, intersection, by = c("id", "city_manual_annotation" = "GC_UCN_MAI_2025_intersection"), keep = T) %>% clean_join()
validation_sep_row_match <- full_join(manual, word_match, by = c("id", "city_manual_annotation" = "GC_UCN_MAI_2025_word_match"), keep = T) %>% clean_join()
city_union_ap1_2 <- combine_ghsl_by_doc(method = "union")
city_intersection_ap1_2 <- combine_ghsl_by_doc(method = "intersection")
validation_sep_union_ap1_2 <- full_join(manual, city_union_ap1_2, by = c("id", "city_manual_annotation" = "GC_UCN_MAI_2025"), keep = T) %>% clean_join()
validation_sep_intersection_ap1_2 <- full_join(manual, city_intersection_ap1_2, by = c("id", "city_manual_annotation" = "GC_UCN_MAI_2025"), keep = T) %>% clean_join()
nrow(validation_sep_row_inter)
nrow(validation_sep_row_match)
nrow(validation_sep_union_ap1_2)
nrow(validation_sep_intersection_ap1_2)

ap_1 <- validation_performance(validation_sep_row_inter, city_manual_annotation, GC_UCN_MAI_2025_intersection, "classification")
ap_2 <- validation_performance(validation_sep_row_match, city_manual_annotation, GC_UCN_MAI_2025_word_match, "classification")
ap_3 <- validation_performance(validation_sep_union_ap1_2, city_manual_annotation, GC_UCN_MAI_2025, "classification")
ap_4 <- validation_performance(validation_sep_intersection_ap1_2, city_manual_annotation, GC_UCN_MAI_2025, "classification")

labs_approaches <- c("Spatial Intersection", 
                     "Word Match", 
                     "Word Match OR\nIntersection",
                     "Word Match AND\nIntersection")
res_world <- rbind(ap_1, ap_2, ap_3, ap_4)
res_world$approach <- rep(labs_approaches, each = 3)

res_world_confusion <- rbind(validation_performance(validation_sep_row_inter, city_manual_annotation, GC_UCN_MAI_2025_intersection, "confusion"),
                             validation_performance(validation_sep_row_match, city_manual_annotation, GC_UCN_MAI_2025_word_match, "confusion"),
                             validation_performance(validation_sep_union_ap1_2, city_manual_annotation, GC_UCN_MAI_2025, "confusion"),
                             validation_performance(validation_sep_intersection_ap1_2, city_manual_annotation, GC_UCN_MAI_2025, "confusion"))
res_world_confusion$approach <- rep(labs_approaches, each = 4)

perf <- ggplot(res_world, aes(name, score, col = approach, shape = approach)) + 
  geom_point(size = 2) + 
  theme_light() + 
  labs(y = "Score", x = "") +
  scale_color_npg()+
  theme(legend.position = "bottom", 
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length=unit(.25, "cm"),
        axis.line.x = element_line(linewidth = unit(.22, "mm")),
        axis.line.y = element_line(linewidth = unit(.22, "mm")),
        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.border = element_blank()) + 
  guides(col=guide_legend(nrow=2, byrow=TRUE))
# perf
# ggsave(perf, file = "plots2/city_validataion.pdf", width = 6.5, height = 5)


subset_df <- function(df, continent, GC_DEV_USR_2025_manual_annotation, GC_DEV_USR_2025_intersection){
  out <- matrix(nrow = 0, ncol = 2)
  for (i in unique(continent)){
    res <- df %>% 
      filter((is.na({{GC_DEV_USR_2025_manual_annotation}}) & {{GC_DEV_USR_2025_intersection}} == i)
             | (is.na({{GC_DEV_USR_2025_intersection}}) & {{GC_DEV_USR_2025_manual_annotation}} == i) 
             | (is.na({{GC_DEV_USR_2025_intersection}}) & is.na({{GC_DEV_USR_2025_manual_annotation}})) 
             | ({{GC_DEV_USR_2025_intersection}} == i & {{GC_DEV_USR_2025_manual_annotation}} == i) 
      )
    res$continent <- i
    out <- rbind(out, res)
  }
  out %>% as_tibble()
}


continent_performance <- function(df_continent, truth, prediction, approach_lab, method){
  out <- matrix(nrow = 0, ncol = 3)
  for (i in unique(df_continent$continent)){
    res <- validation_performance(df_continent[df_continent$continent == i,], {{truth}}, {{prediction}}, method)
    res$continent <- i
    out <- as.data.frame(rbind(out, res))
  }
  out[, "approach"] <- approach_lab
  out %>% as_tibble()
}

s1 <- subset_df(validation_sep_row_inter, ghsl$GC_DEV_USR_2025, GC_DEV_USR_2025_manual_annotation, GC_DEV_USR_2025_intersection)
s2 <- subset_df(validation_sep_row_match, ghsl$GC_DEV_USR_2025, GC_DEV_USR_2025_manual_annotation, GC_DEV_USR_2025_word_match)
s3 <- subset_df(validation_sep_union_ap1_2, ghsl$GC_DEV_USR_2025, GC_DEV_USR_2025_manual_annotation, GC_DEV_USR_2025)
s4 <- subset_df(validation_sep_intersection_ap1_2, ghsl$GC_DEV_USR_2025, GC_DEV_USR_2025_manual_annotation, GC_DEV_USR_2025)
res_cont <- rbind(continent_performance(s1, city_manual_annotation, GC_UCN_MAI_2025_intersection,  "Spatial Intersection", "classification"), 
                  continent_performance(s2, city_manual_annotation, GC_UCN_MAI_2025_word_match, "Word Match", "classification"),
                  continent_performance(s3, city_manual_annotation, GC_UCN_MAI_2025, "Word Match OR\nIntersection", "classification"),
                  continent_performance(s4, city_manual_annotation, GC_UCN_MAI_2025, "Word Match AND\nIntersection", "classification")
)
res_cont_confusion <- rbind(continent_performance(s1, city_manual_annotation, GC_UCN_MAI_2025_intersection,  "Spatial Intersection", "confusion"), 
                            continent_performance(s2, city_manual_annotation, GC_UCN_MAI_2025_word_match, "Word Match", "confusion"),
                            continent_performance(s3, city_manual_annotation, GC_UCN_MAI_2025, "Word Match OR\nIntersection", "confusion"),
                            continent_performance(s4, city_manual_annotation, GC_UCN_MAI_2025, "Word Match AND\nIntersection", "confusion")
)


res_world$continent <- "World"
res <- bind_rows(res_cont, res_world %>% select(name, score, continent, approach)) %>% 
  mutate(continent = ifelse(continent == "Northern America", "Northern\nAmerica", continent),
         continent = ifelse(continent == "Latin America and the Caribbean", "Latin America\nand the\nCaribbean", continent),
         continent = factor(continent, levels = c("World", "Northern\nAmerica", "Latin America\nand the\nCaribbean","Europe", "Africa", "Asia", "Oceania")))
library(ggpubr)
perf_cont <- ggplot(res, aes(name, score, col = approach, shape = approach)) + 
  geom_point(size = 2) + 
  theme_light() + 
  labs(y = "Score", x = "") +
  scale_color_npg() +
  # ylim(0,1) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  facet_wrap(~continent, nrow = 1) +
  coord_cartesian(clip = "off") + 
  theme(legend.position = "right", 
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length=unit(.25, "cm"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        plot.margin = (unit(c(.1,.1,0,.1), "cm") ) )
# perf_cont

res_world_confusion <- res_world_confusion %>% mutate(continent = "World") %>% select(name, score, continent, approach) %>% as_tibble()
res_conf <- bind_rows(res_cont_confusion, res_world_confusion) %>% 
  mutate(continent = ifelse(continent == "Northern America", "Northern\nAmerica", continent),
         continent = ifelse(continent == "Latin America and the Caribbean", "Latin America\nand the\nCaribbean", continent),
         continent = factor(continent, levels = c("World", "Northern\nAmerica", "Latin America\nand the\nCaribbean","Europe", "Africa", "Asia", "Oceania")),
         approach = factor(approach, labs_approaches))

perf_cont_conf <- res_conf %>%
  mutate(Actual = ifelse(name %in% c("TP", "FN"), "positive", "negative")) %>% 
  mutate(Predicted = ifelse(name %in% c("TP", "FP"), "positive", "negative")) %>%
  mutate(Actual = factor(Actual, levels = rev(c("negative", "positive"))),
         Predicted = factor(Predicted, levels = c("negative", "positive"))) %>% 
  ggplot(aes(x = Actual, y = Predicted, fill = ifelse(Actual == "negative" & Predicted == "negative", NA, score))) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = score), color = "white", size = 4) +
  theme_light() + 
  scale_fill_gradient(trans = "sqrt") +
  theme(legend.position = "none", 
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length=unit(.25, "cm"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_text(angle = 0, size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        strip.text.x = element_blank()
  ) + 
  facet_grid(approach~continent)

comb <- plot_grid(perf_cont, perf_cont_conf, ncol = 1, align = "v", axis = "lr", rel_widths = c(1, 1.3), labels = "auto")
comb
ggsave("plots2/figA7.pdf", plot = comb, width = 10, height = 5.5)


