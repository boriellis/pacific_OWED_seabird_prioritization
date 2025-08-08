#this function takes the raw data from qualtrics and cleans it into a longform dataframe that can be used in the calculate_exposure function

clean_exweights <- function(csv_file_path) {
  # Define rare species codes lookup
  rare_codes <- tibble(common_name = c("Short-tailed Albatross", "Townsend's Storm-Petrel", "Hawaiian Petrel"), 
                       alpha_code = c("STAL", "TOSP", "HAPE"))
  
  # Define model names to replace with
  model_names <- c("SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", "MAMU", "PIGU", 
                   "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", "BOGU", "HEEG", "WEGU-WGWH-GWGU", 
                   "CAGU", "HERG-ICGU", "CATE", "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", 
                   "LOON", "LAAL", "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", 
                   "BULS", "STTS-SOSH-FFSH", "BVSH", "BRAC", "PECO", "DCCO", "BRPE")
  
  # Read header from row 2 (skip 1, read 1 row)
  header <- read_csv(csv_file_path, skip = 1, n_max = 1, show_col_types = FALSE)
  
  # Read the actual data starting from row 4, using proper column names
  raw_dataframe <- read_csv(csv_file_path,
                            skip = 3,
                            col_names = colnames(header),
                            show_col_types = FALSE)
  
  # Clean the data
  cleaned_weights <- raw_dataframe %>% 
    mutate(expert = row_number()) %>% 
    slice(-9, -17, -18) %>%  # Remove incomplete submissions
    select(expert,
           starts_with("Short-tailed Albatross"),
           starts_with("Townsend's Storm-Petrel"),
           starts_with("Hawaiian Petrel")) %>% 
    pivot_longer(-expert, 
                 names_to = c("species", "model"),
                 names_sep = " - ",
                 values_to = "weight") %>% 
    mutate(weight = weight / 100) %>%  # Convert to percentages
    left_join(rare_codes, by = c(species = "common_name"))
  
  # Add model names
  cleaned_weights$model_name <- rep(model_names, nrow(cleaned_weights) / length(model_names))
  
  return(cleaned_weights)
}
