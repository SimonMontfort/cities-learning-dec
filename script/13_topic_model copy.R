rm(list = ls())

# Load libraries
library(dplyr)
library(tibble)
library(stringi)
library(stringr)
library(furrr)
library(data.table)
library(textclean)
library(tidytext)
library(purrr)
library(stm)
library(future)
library(cld3)
library(SnowballC)
library(arrow)

# Set working directory
setwd("/Users/simon/Documents/repo/cities-learning-dec")

cat(">>> Starting...\n")
flush.console()

# Load data
clim_sol <- read_parquet("data/climate_solutions_typology/oa_sentence_solutions.parquet")
sentences <- read_parquet("data/climate_solutions_typology/oa_sentences.parquet")

clean_places <- read.csv("data/geoparser/clean_places_augmented.csv") %>% as_tibble()

case_studies <- clean_places %>% 
  group_by(id) %>% 
  summarise(is_case_study = any(city_intersects_yes) | any(city_intersects_yes))

cat("class(oa):", class(clim_sol), "\n")
cat("class(case_studies):", class(case_studies), "\n")
cat("Rows in oa:", ifelse(is.null(clim_sol), 0, nrow(clim_sol)), "\n")
cat("Rows in case_studies (yes and no):", ifelse(is.null(case_studies), 0, nrow(case_studies)), "\n")

clim_sol <- clim_sol %>% 
  left_join(case_studies, by = "id") %>% 
  filter(is_case_study) %>% 
  select(-is_case_study)

clim_sol <- clim_sol %>% 
  left_join(sentences, by =  c("id", "sentence_id")) 
  
sol_ind <- colnames(clim_sol)[grepl("_match", colnames(clim_sol))]
clim_sol$any_solution <- rowSums(clim_sol[, sol_ind])

clim_sol <- clim_sol %>% 
  filter(any_solution >=1)

clim_sol
write_parquet(clim_sol, "data/climate_solutions_typology/oa_sentence_solutions_relevant.parquet")

# 
# cat(">>> Files loaded and pre-processed...\n")
# 
# filter_english_docs <- function(oa, abstract) {
#   # Split text into paragraphs
#   abs_split <- stri_split_regex(oa[, abstract], "\n+\\s+")
#   abs_split <- cbind(unlist(abs_split), rep(oa$id, lengths(abs_split)))
#   colnames(abs_split) <- c("abstract_paragraph", "doc_id")
#   abs_split <- as.data.frame(abs_split, stringsAsFactors = FALSE)
#   
#   # Detect language of each paragraph
#   abs_split$lang_p <- detect_language(abs_split$abstract_paragraph)
#   
#   # Keep only English paragraphs for each doc
#   abs_split_en <- abs_split %>% filter(lang_p == "en")
#   
#   # Reassemble English paragraphs by doc_id
#   abs_en_agg <- as.data.table(abs_split_en)[
#     , .(abstract_en = paste(abstract_paragraph, collapse = "\n ")), by = doc_id
#   ]
#   
#   # Detect language of titles
#   abs_title <- data.frame(title = oa$title, doc_id = oa$id, stringsAsFactors = FALSE)
#   abs_title$lang_t <- detect_language(abs_title$title)
#   
#   # Join English abstracts back to oa
#   oa2 <- left_join(oa, abs_en_agg, by = c("id" = "doc_id"))
#   
#   # Join title language info
#   oa2 <- left_join(oa2, abs_title %>% select(doc_id, lang_t), by = c("id" = "doc_id"))
#   
#   oa_filtered <- oa2 %>%
#     filter(
#       (lang_t == "en" & !is.na(abstract_en)) |   # English title and English abstract
#         (is.na(title) & !is.na(abstract_en))       # No title but English abstract
#     )
#   
#   # Optional: Replace title with Title Case for English titles
#   oa_filtered <- oa_filtered %>%
#     mutate(title_en = stri_trans_totitle(title))
#   
#   return(oa_filtered)
# }
# 
# # --- Example usage ---
# clim_sol_cleaned <- filter_english_docs(clim_sol, sentences)
# # head(oa_cleaned %>% select(id, title, abstract, title_en, abstract_en))

cat(">>> Files loaded and pre-processed...\n")
flush.console()

######################################################
# Cleaning
######################################################

# cat(">>> subset OpenAlex data for testing...\n")
# oa <- oa[1:5000,]
# 
# oa_cleaned <- oa_cleaned %>% 
#   mutate(
#     text = paste(title_en, abstract_en),
#     text = str_replace_all(text, "<a(.*?)>", " "),  # links
#     text = stri_replace_all_regex(text, "\\([^()\\d]*\\d[^()]*\\)", ""),  # citations
#     text = stri_replace_all_regex(text, "ACKNOWLEDMENTS.*", ""), 
#     text = stri_replace_all_regex(text, "Acknowledgements.*", ""),
#     text = stri_replace_all_regex(text, "REFERENCES.*", ""), 
#     text = stri_replace_all_regex(text, "References.*", ""), 
#     text = stri_replace_all_regex(text, "Correspondence.*", ""), 
#     text = replace_non_ascii(text, "")
#   )

cat(">>> Starting text processing...\n")
flush.console()



######################################################
# Preprocess for STM
######################################################

corpus_stm_t <- textProcessor(documents = clim_sol$sentence, metadata = clim_sol, ucp = TRUE)
out_t <- prepDocuments(corpus_stm_t$documents, corpus_stm_t$vocab, corpus_stm_t$meta, lower.thresh = 50)

cat(">>> Preprocessed text ....\n")
flush.console()

######################################################
# STM Topic Modeling
######################################################

# Setup parallel plan
options(future.debug = TRUE)  # Enable future debug logging
options(future.globals.maxSize = 20000 * 1024^2)  # 20GB
plan(multisession, gc = TRUE, workers = 2)

cat(">>> Starting future_map...\n")
flush.console()

many_models <- tryCatch({
  tibble(K = seq(50, 60, 10)) %>%
    mutate(model = future_map(
      K,
      ~ {
        cat(paste0(">>> Starting STM for K = ", ., " at ", Sys.time(), "\n"))
        flush.console()
        
        result <- stm(
          documents = out_t$documents,
          vocab = out_t$vocab,
          K = .,
          data = out_t$meta,
          seed = 1,
          verbose = TRUE,
          ngroups = 1
        )
        
        cat(paste0(">>> Finished STM for K = ", ., " at ", Sys.time(), "\n"))
        flush.console()
        result
      },
      .options = furrr_options(seed = 1),
      .progress = TRUE
    ))
}, error = function(e) {
  cat(">>> ERROR in future_map: ", conditionMessage(e), "\n")
  flush.console()
  NULL
})

if (is.null(many_models)) {
  cat(">>> many_models is NULL. Skipping result saving.\n")
  flush.console()
} else {
  cat(">>> Saving results...\n")
  flush.console()
  
  td_betas <- many_models %>%
    mutate(model = map(model, tidy))
  
  td_gammas <- many_models %>%
    mutate(model = lapply(model, function(x) {
      tidy(x, matrix = "gamma", document_names = out_t$meta$id)
    }))
  
  saveRDS(many_models, "data/topic_model/many_models_220.rds")
  saveRDS(td_betas, "data/topic_model/td_betas_220.rds")
  saveRDS(td_gammas, "data/topic_model/td_gammas_220.rds")
  saveRDS(out_t, "data/topic_model/out_t_220.rds")
  
  cat(">>> Saved all data correctly....\n")
  flush.console()
}

cat(">>> Script completed successfully.\n")
flush.console()
