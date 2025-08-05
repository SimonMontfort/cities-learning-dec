
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

library(dplyr)
library(arrow)

################################################################################
# load data
################################################################################

# oa data
file_names <- list.files(
  path = "/Users/simon/Documents/repo/cities-learning/data/OpenAlex/05_deduplicated",
  pattern = "^city_works_df_NA_abstr_added_dedup_\\d+\\.csv$",
  full.names = TRUE
)
df_list <- lapply(file_names, read.csv)
oa <- do.call(rbind, df_list)

# studies per city
clean_places <- read.csv("data/geoparser/clean_places_augmented.csv")
clust_stud_pop <- read.csv("data/clustering_results/cities_by_regional_type_clean.csv")

################################################################################
# prep and transform data
################################################################################

clean_places <- clean_places %>% 
  filter(!is.na(city_intersection_id) & !is.na(city_word_match_id)) %>%
  mutate(city_id = ifelse(is.na(city_intersection_id), city_word_match_id, city_intersection_id)) %>% 
  select(id, city_id)

clean_places <- clean_places %>% 
  left_join(clust_stud_pop) %>% 
  select(id, city_id, cluster_name, city_name, country, Region)

clean_places_type_region <- clean_places %>% 
  group_by(id) %>% 
  mutate(value = "present") %>%
  pivot_wider(
    names_from = cluster_name,
    values_from = value,
    values_fill = list(value = "absent"),
    names_prefix = "Type: "
  ) %>%
  mutate(value = "present") %>%
  pivot_wider(
    names_from = Region,
    values_from = value,
    values_fill = list(value = "absent"),
    names_prefix = "Region: "
  ) %>%
  summarise(across(starts_with("Type:") | starts_with("Region:"), ~ ifelse(any(. == "present"), "present", "absent")))

clean_places_cities_countries <- clean_places %>%
  group_by(id) %>% 
  summarise_at(vars(city_name, city_id, country), .funs = function(x){paste(x, collapse = ", ")}) %>% 
  rename(city_names = city_name, city_ids = city_id, countries = country)

clean_places <- left_join(clean_places_cities_countries, clean_places_type_region)

case_studies <- oa %>%
  left_join(clean_places, by = "id") %>%
  filter(!is.na(city_ids)) %>%
  as_tibble()


################################################################################
# download specs (e.g. citations)
################################################################################

# library(OpenAlexR)
# article_specs <- oa_fetch(
#   entity = "works",
#   id = case_studies$id,
#   to_publication_date = "2025-03-24",
#   count_only = FALSE,
#   verbose = FALSE,
#   options = list(select = c(
#     "id", "publication_date", "type", "locations",
#     "authorships", "open_access", 
#     "concepts", "cited_by_count", "referenced_works", "language"
#   ))
# )
# saveRDS(article_specs, "data/OpenAlex/meta.Rds")
article_specs <- readRDS("data/OpenAlex/meta.Rds")

# extract authros
authors <- article_specs %>% 
  rename(article_id = id) %>% 
  group_by(article_id) %>% 
  select(authorships) %>% 
  unnest(authorships) %>% 
  summarise(authors = paste0(display_name, collapse = ", "))


################################################################################
# clean and store db
################################################################################

case_studies_clean <- case_studies %>% 
  left_join(authors, by = c("id" = "article_id")) %>% 
  left_join(article_specs %>% select(-authorships, -any_repository_has_fulltext, 
                                     -is_oa_anywhere, -referenced_works, -concepts), 
            by = "id") %>% 
  select(OpenAlex_article_id = id, publication_year, publication_date, abstract = text, cited_by_count,
         type, oa_status, oa_url, language,
         city_names, countries, starts_with("Region"), starts_with("Type")
         )
  
case_studies_clean


# Second sheet: Case study literature
cleaned_case_studies <- case_studies_clean %>%
  mutate(
    abstract = as.character(abstract),
    abstract = iconv(abstract, from = "", to = "UTF-8", sub = "byte"),
    abstract = gsub("[\r\n]", " ", abstract),
    abstract = substr(abstract, 1, 32000),
    abstract = ifelse(is.na(abstract), "", abstract)
  )

# Load the workbook
wb <- wb_load("data/case_selection/case_selection_and_literature.xlsx")

wb$add_worksheet("Case study literature")
wb$add_data(sheet = "Case study literature", x = cleaned_case_studies)

# Save the workbook with both sheets
wb$save("data/case_selection/case_selection_and_literature.xlsx")


