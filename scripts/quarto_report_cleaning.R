###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################
#------------------------------------------------------------------------------

## this script is just to create versions of the data that can be used to print out tables for the quarto report. Isn't necessary for the overall analysis. 

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


#get the proportions per region from the density df
cleandensities <- densities %>% 
  rename(exposure_model=...1) %>%  #rename first col
  #add new columns for proportions in given regions
  mutate(propOR = ((totdens_OCSP0566 + totdens_OCSP0567)/totdens_region), #proportion that overlaps with OR call areas
         propCA = ((totdens_OCSP0561 + totdens_OCSP0562 + totdens_OCSP0563 + totdens_OCSP0564 + totdens_OCSP0565 )/totdens_region), #proportion that overlaps with CA leases
         propALL = ((totdens_OCSP0561 + totdens_OCSP0562 + totdens_OCSP0563 + totdens_OCSP0564 + totdens_OCSP0565 + totdens_OCSP0566 + totdens_OCSP0567)/totdens_region)
  ) %>% 
  select(exposure_model, propOR, propCA, propALL) #select only proportion columns


#clean sensitivities and combine
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


min(cleanmasterlist$DV)
# Rescale values ----------------------------------------------------------

rescaled_masterlist <- cleanmasterlist %>% 
  #rescaling CV, DV, and exposures from 0 to 1
  mutate(rescaled_propALL = (propALL - min(propALL, na.rm = TRUE)) / (max(propALL, na.rm = TRUE) - min(propALL, na.rm = TRUE)),
         rescaled_propCA = (propCA - min(propCA, na.rm = TRUE)) / (max(propCA, na.rm = TRUE) - min(propCA, na.rm = TRUE)),
         rescaled_propOR = (propOR - min(propOR, na.rm = TRUE)) / (max(propOR, na.rm = TRUE) - min(propOR, na.rm = TRUE)),
         rescaled_DV = (DV - min(DV, na.rm = TRUE)) / (max(DV, na.rm = TRUE) - min(DV, na.rm = TRUE)),
         rescaled_CV = (CV - min(CV, na.rm = TRUE)) / (max(CV, na.rm = TRUE) - min(CV, na.rm = TRUE))
  ) %>% 
  # add a single column to contain whichever of the rescaled CV or DV #s is higher, and state which number is used in a second column 
  mutate(
    highest_sens = case_when( #listing which is the higher value
      rescaled_DV > rescaled_CV ~ "DV",
      rescaled_CV > rescaled_DV ~ "CV",
      TRUE ~ "tie" # Handle the case when both values are equal 
    )
  )

#print into csv

write.csv(rescaled_masterlist, file = "data/processed_data/data_for_quarto.csv")
