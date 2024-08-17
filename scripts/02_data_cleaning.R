###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################

# Part 1: Load Packages -------------------------------------------------------

# Load packages
packages<- c("tidyverse", "here")

pacman::p_load(packages, character.only = TRUE); rm(packages)


# Part 2: Import and Clean Data -----------------------------------------------

#load in csvs
densities <- read_csv(here::here("data/processed_data/raw_density_outputs.csv"))
masterlist <- read_csv(here::here("data/raw_data/allspp_iucnstatus.csv"))
CV <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_update2023_CV.csv"))
DV <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_update2023_DV.csv"))

#clean masterlist 
masterlist <- masterlist %>% 
  #filter out the species we don't consider "in the region"
  filter(regional == "Y") %>% 
  #select relevant columns
  select(alpha_code, taxonomy, common_name, scientific_name, exposure_model, iucn_status)

#clean densities
cleandensities <- densities %>% 
  #rename first col
  rename(exposure_model=...1) %>% 
  #add new columns for proportions in given regions
  mutate(propOR = ((totdens_OCSP0566 + totdens_OCSP0567)/totdens_region), #proportion that overlaps with OR call areas
         propCA = ((totdens_OCSP0561 + totdens_OCSP0562 + totdens_OCSP0563 + totdens_OCSP0564 + totdens_OCSP0565 )/totdens_region), #proportion that overlaps with CA leases
         propALL = ((totdens_OCSP0561 + totdens_OCSP0562 + totdens_OCSP0563 + totdens_OCSP0564 + totdens_OCSP0565 + totdens_OCSP0566 + totdens_OCSP0567)/totdens_region)
           ) %>% 
  #select only proportion columns
  select(exposure_model, propOR, propCA, propALL)

#clean sensitivities and combine
cleanCV <- CV %>% 
  select(alpha_code = AlphaCode,
         CV = CV_new) 

cleanDV <- DV %>% 
  select(alpha_code = AlphaCode,
         DV = DV_new)

cleansensitivity <- cleanCV %>% 
  left_join(cleanDV, by = "alpha_code")


#combine desired data into single dataframe
cleanmasterlist <- masterlist %>% 
  left_join(cleandensities, by = "exposure_model") %>% 
  left_join(cleansensitivity, by = "alpha_code")


