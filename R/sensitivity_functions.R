#these are the functions in the status branch of the work flow




# CLEAN AND COMBINE CV & DV VALUES ----------------------------------------

#load in the three csvs before using - species list, cv, and dv sheets
clean_sens <- function(sp, cv, dv) {
  
}




# Load packages (think I'll delete this later when I clean it up?)
packages<- c("tidyverse", "here")

pacman::p_load(packages, character.only = TRUE); rm(packages)

#will need updated versions of:

masterlist <- read_csv(here::here("data/raw_data/allspp_iucnstatus.csv"))
CV <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_update2023_CV.csv"))
DV <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_update2023_DV.csv"))

#filter the main list to be the species we want



#then clean sensitivities and combine
cleanCV <- CV %>% #collision vulnerability
  select(alpha_code = AlphaCode,
         CV = CV_new) 
cleanDV <- DV %>% # displacement vulnerability
  select(alpha_code = AlphaCode,
         DV = DV_new)
cleansensitivity <- cleanCV %>% #combine into one 
  left_join(cleanDV, by = "alpha_code")

#combine desired data into single dataframe
cleanmasterlist <- masterlist %>% 
  left_join(cleandensities, by = "exposure_model") %>% 
  left_join(cleansensitivity, by = "alpha_code")

#return the dataframe