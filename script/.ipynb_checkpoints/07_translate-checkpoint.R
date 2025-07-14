rm(list = ls())
library(dplyr)
library(arrow)
library(tibble)
library(stringi)

setwd("/Users/simon/Documents/repo/cities-learning")

# > google_get_supported_languages()
# # A tibble: 194 × 2
# Language  `ISO-639 code`
# <chr>     <chr>         
#   1 Abkhaz    ab            
# 2 Acehnese  ace           
# 3 Acholi    ach           
# 4 Afrikaans af            
# 5 Albanian  sq            
# 6 Alur      alz           
# 7 Amharic   am            
# 8 Arabic    ar            
# 9 Armenian  hy            
# 10 Assamese  as            
# # ℹ 184 more rows
# # ℹ Use `print(n = ...)` to see more rows

# function to combine individual files (split to save space and enable saving on GitHub)
combine_files <- function(directory = "data", file_prefix = "clim_deb_fr_[1-9]") {
  # List all files in the directory that match the file prefix
  file_list <- list.files(path = directory, pattern = file_prefix, full.names = TRUE)
  
  print(file_list)
  # Check if any files were found
  if (length(file_list) == 0) {
    message("No files found with the specified prefix.")
    return(NULL)
  }

  # Read each file into a list of data frames
  data_list <- lapply(file_list, read_feather)

  # Combine all data frames into one
  combined_data <- do.call(rbind, data_list)

  # Return the combined data frame
  return(as_tibble(combined_data))
}



# Translate using Google Translate
translate_by_sentence <- function(clim_deb_it, variable, target_language){
  clim_deb_it <- as.data.frame(clim_deb_it)
  var_translated <- paste0(variable, "_", target_language)
  var_lang <- paste0(variable, "_languages")
  var_lang_filt <- paste0(variable, "_language_filtering")
  clim_deb_it[, var_translated] <- NA
  clim_deb_it[, var_lang] <- NA
  clim_deb_it[, var_lang_filt] <- NA
  
  require(polyglotr)
  require(stringi)
  
  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = nrow(clim_deb_it), style = 3)
  
  for (i in 1:nrow(clim_deb_it)) {
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # skip NA abstracts
    if (!is.na(clim_deb_it[i, variable])){
      text_sentences <- t(stri_split_regex(clim_deb_it[i, variable], "(?<=\\.|!|\\?)\\s+", simplify = TRUE))
      text_languages <- sapply(text_sentences, language_detect)
      if (any(text_languages != "en")){
        for (j in 1:length(text_sentences)) {
          if (text_languages[j] != "en"){
            text_sentences[j] <- google_translate(text_sentences[j], target_language = target_language)
          }
        }
      }
      share <- plyr::count(text_languages) %>% mutate(share = freq/sum(freq))
      share_condition <- ifelse(!any(is.na(share$x)) && any(share$x == "en"), share$share[share$x == "en"] > .8, FALSE)
      tryCatch({
        if (share_condition) {
          clim_deb_it[i, var_translated] <- paste0(text_sentences[text_languages == "en"], collapse = " ")
          clim_deb_it[i, var_lang_filt] <- "only kept english text; 80% of the sentences are in english"
        } else if (!"en" %in% text_languages) {
          clim_deb_it[i, var_translated] <- paste0(text_sentences, collapse = " ")
          clim_deb_it[i, var_lang_filt] <- "translated"
        } else if (!share_condition) {
          clim_deb_it[i, var_translated] <- paste0(text_sentences, collapse = " ")
          clim_deb_it[i, var_lang_filt] <- "kept english and non-english text parts"
        } else if (unique(text_languages) == "en") {
          clim_deb_it[i, var_translated] <- paste0(text_sentences, collapse = " ")
          clim_deb_it[i, var_lang_filt] <- "text was already in english"
        }
      }, error = function(e) {
        message(paste("Error at row", i, ":", e$message))
      })
      clim_deb_it[, var_lang][i] <- paste0(unique(text_languages), collapse = ", ")
    }
  }
  
  # Close progress bar
  close(pb)
  
  print(paste0("translations checks done for file"))
  clim_deb_it %>% as_tibble()
}

translate_by_sentence <- function(clim_deb_it, variable, target_language) {
  require(polyglotr)
  require(stringi)
  require(plyr)
  require(dplyr)
  require(future.apply)
  
  plan(multisession, workers = 6)  # number of cores
  
  clim_deb_it <- as.data.frame(clim_deb_it)
  var_translated <- paste0(variable, "_", target_language)
  var_lang <- paste0(variable, "_languages")
  var_lang_filt <- paste0(variable, "_language_filtering")
  
  clim_deb_it[, var_translated] <- NA
  clim_deb_it[, var_lang] <- NA
  clim_deb_it[, var_lang_filt] <- NA
  
  pb <- txtProgressBar(min = 0, max = nrow(clim_deb_it), style = 3)
  
  process_row <- function(i) {
    result <- list(translated = NA, lang = NA, filt = NA)
    
    tryCatch({
      text <- clim_deb_it[i, variable]
      if (!is.na(text)) {
        text_sentences <- t(stri_split_regex(text, "(?<=\\.|!|\\?)\\s+", simplify = TRUE))
        text_languages <- sapply(text_sentences, language_detect)
        
        share <- plyr::count(text_languages) %>% mutate(share = freq / sum(freq))
        share_condition <- ifelse(!any(is.na(share$x)) && any(share$x == "en"),
                                  share$share[share$x == "en"] > 0.8, FALSE)
        
        if (length(unique(text_languages)) == 1 && unique(text_languages) == "en") {
          result$translated <- paste0(text_sentences, collapse = " ")
          result$filt <- "text was already in english"
        } else if (share_condition) {
          result$translated <- paste0(text_sentences[text_languages == "en"], collapse = " ")
          result$filt <- "only kept english text; 80% of the sentences are in english"
        } else {
          # Translate non-English only (batch)
          translated_sentences <- text_sentences
          non_en_idx <- which(text_languages != "en")
          if (length(non_en_idx) > 0) {
            translated_sentences[non_en_idx] <- google_translate(text_sentences[non_en_idx], target_language)
          }
          result$translated <- paste0(translated_sentences, collapse = " ")
          result$filt <- ifelse(!"en" %in% text_languages, "translated", "kept english and non-english text parts")
        }
        
        result$lang <- paste0(unique(text_languages), collapse = ", ")
      }
    }, error = function(e) {
      message(paste("Error at row", i, ":", e$message))
    })
    
    setTxtProgressBar(pb, i)
    return(result)
  }
  
  # Parallel loop
  results <- future_lapply(1:nrow(clim_deb_it), process_row, future.seed=TRUE)
  
  # Populate results back into the data frame
  for (i in seq_along(results)) {
    clim_deb_it[i, var_translated] <- results[[i]]$translated
    clim_deb_it[i, var_lang] <- results[[i]]$lang
    clim_deb_it[i, var_lang_filt] <- results[[i]]$filt
  }
  
  close(pb)
  print("translation checks done for file")
  clim_deb_it %>% as_tibble()
}



split_data <- function(data, rows_per_chunk){
  # Calculate the total number of chunks needed
  n <- nrow(data)
  num_chunks <- ceiling(n / rows_per_chunk)
  chunk_data <- list()
  for (i in 1:num_chunks) {
    # Define the row indices for the current chunk
    start_row <- ((i - 1) * rows_per_chunk) + 1
    end_row <- min(i * rows_per_chunk, n)
    
    # Subset the data for the current chunk
    chunk_data[[i]] <- data[start_row:end_row, ]
  }
  chunk_data
}


write_files <- function(data_list, dir = "data", base_filename = "clim_deb_de") {
  # Ensure the directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  for (i in 1:length(data_list)) {
    # Construct filename
    filename <- file.path(dir, paste0(base_filename, "_", i, ".feather"))
    
    # Write and check file size
    write_feather(data_list[[i]], filename)
  }
}

city_works <- combine_files("data/OpenAlex/02_NA_added", "city_works_df_NA_abstr_added_[0-9]+\\.feather$")

# before exact duplicate removal by id
nrow(city_works)
# [1] 281553

city_works <- city_works %>% distinct(id, .keep_all = T)

# after exact duplicate removal by id
nrow(city_works)
# [1] 254161

city_works <- city_works %>% 
  # filter(!stri_detect_fixed(title, "Decision: ")) %>% 
  filter(!stri_detect_fixed(title, ".xlsx")) %>% 
  filter(!type %in% c("dataset", "erratum", "retraction", "peer-review", "reference-entry")) 

nrow(city_works)
# [1] 253778

# city_works_s <- city_works %>% sample_n(100)
# city_works_s <- city_works %>% filter(id %in% c('https://openalex.org/W4403377257', 'https://openalex.org/W4403377257'))
# city_works_s$abstract
# city_works_split_translated <- list()
# city_works_split_translated[[i]] <- translate_by_sentence(city_works_split[[i]][1:226,], "abstract", "en")


city_works_split <- split_data(city_works, rows_per_chunk = 20000)
for (i in 1:length(city_works_split)){
  print(paste("Translating file", i, "/", length(city_works_split)))
  city_works_split_translated[[i]] <- translate_by_sentence(city_works_split[[i]], "abstract", "en")
}
write_files(city_works_split_translated, dir = "data/OpenAlex/03_translated", base_filename = "city_works_df_translated")
