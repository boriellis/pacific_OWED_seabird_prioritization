#these are the functions in the status branch of the work flow




# CLEAN AND COMBINE CV & DV VALUES ----------------------------------------

# delete this chunk later
packages<- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)
sp<- read_csv(here::here("data/raw_data/total_sp_list.csv"))
cv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_CV.csv"))
dv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_DV.csv"))


#' Clean and combine collision and displacement sensitivity values 
#'
#' @param sp loaded in total_sp_list.csv df
#' @param cv loaded in POCS_VulnIndex_CV.csv df
#' @param dv loaded in POCS_VulnIndex_DV.csv df
#'
#' @returns a cleaned df of species alpha code, common name, scientific name, following our local taxonomy, and the up to date CV and DV values from the Kelsey et al 2025 report 
#' @export
#'
#' @examples
clean_sens <- function(sp, cv, dv) {
  #fix typos in dv 
  dv <- dv %>% 
    mutate(AlphaCode = case_when(
      AlphaCode == "ROTE" ~ "ROYT",
      AlphaCode == "WIPE" ~ "AWPE",
      TRUE ~ AlphaCode
    ))
  #use the main list as the taxonomy
  clean_sens <- sp %>% 
    select(alpha_code,
           common_name,
           scientific_name) %>%
    left_join(dv, by = c("alpha_code" = "AlphaCode")) %>% 
    select(alpha_code, common_name, scientific_name, DV_new) %>% 
    left_join(cv, by = c("alpha_code" = "AlphaCode")) %>% 
    select(alpha_code, common_name, scientific_name, DV = DV_new, CV = CV_new) %>% 
    filter(!is.na(alpha_code))
  return(clean_sens)
  
}



# RESCALE SENSITIVITY VALUES ----------------------------------------------

#for whatever reason I think I'm going to make it so that the options are weight of 1, 1.5, and 2 to start? so that would be a range of 0.618 to 1.618, 0.5-2.00, and 0.414-2.414. Max and I should discuss this!

#I think the structure of this function is close but it's currently returning an error! need to figure out why.  


#' Rescale Sensitivity Values
#'
#' @param list is the loaded in total_sp_list.csv df (used to select only regional species)
#' @param sens cleaned sensitivity value df (output from clean_sens)
#' @param sel user selected choice of CV, DV, the two summed, or whichever is highest
#' @param weight user selected sensitivity weight (we'll need to decide what the bounds are here)
#'
#' @returns a df of alpha codes, species names and rescaled selected sensitivity value 

rescale_sens <- function(list, sens, sel, weight){
  
  # Input validation
  if (!sel %in% c("CV", "DV", "sum", "highest")) {
    stop("sel must be one of: 'CV', 'DV', 'sum', 'highest'")
  }
  
  if (!weight %in% c(1, 1.5, 2)) {
    stop("weight must be one of: 1, 1.5, 2")
  }
  
  # Filter for regional species only
  regional_species <- list %>% 
    filter(regional == "Y") %>% 
    pull(alpha_code)  
  
  sens <- sens %>% 
    filter(alpha_code %in% regional_species)  # adjust column name as needed
  
  # Step 1: Define the range bounds based on weight
  get_range_bounds <- function(w) {
    if (w == 1) {
      low <- (sqrt(5) - 1) / 2  # ≈ 0.618
      high <- 1 / low           # ≈ 1.618
    } else if (w == 1.5) {
      low <- 0.5
      high <- 2.0
    } else if (w == 2) {
      low <- sqrt(2) - 1        # ≈ 0.414  
      high <- 1 / low           # ≈ 2.414
    }
    return(list(low = low, high = high))
  }
  
  bounds <- get_range_bounds(weight)
  
  # Step 2: Selection logic and data preparation
  if (sel == "CV") {
    selected_values <- sens$CV
  } else if (sel == "DV") {
    selected_values <- sens$DV
  } else if (sel == "sum") {
    # Rescale both to 0-1 range, add 0.01 to avoid zeros, then add
    cv_scaled <- (sens$CV - min(sens$CV)) / (max(sens$CV) - min(sens$CV)) + 0.01
    dv_scaled <- (sens$DV - min(sens$DV)) / (max(sens$DV) - min(sens$DV)) + 0.01
    selected_values <- cv_scaled + dv_scaled
  } else if (sel == "highest") {
    # Rescale both to 0-1 range, add 0.01 to avoid zeros, then take max
    cv_scaled <- (sens$CV - min(sens$CV)) / (max(sens$CV) - min(sens$CV)) + 0.01
    dv_scaled <- (sens$DV - min(sens$DV)) / (max(sens$DV) - min(sens$DV)) + 0.01
    selected_values <- pmax(cv_scaled, dv_scaled)
  }
  
  # Step 3: Direct rescaling to target range
  final_values <- bounds$low + (selected_values - min(selected_values)) / (max(selected_values) - min(selected_values)) * (bounds$high - bounds$low)
  
  # Create output dataframe
  result <- data.frame(
    species = sens$species,
    sensitivity = final_values
  )
  return(result)
}

test1 <- rescale_sens(list, sens, sum, 1) #this isn't working yet, haven't figured out why yet


