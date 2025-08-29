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
    select(alpha_code, common_name, scientific_name, DV_new, CV_new) %>% 
    filter(!is.na(alpha_code))
  return(clean_sens)
  
}




