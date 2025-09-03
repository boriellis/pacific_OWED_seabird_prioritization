#this is the new holding area that will become the script that does the whole workflow analysis using the functions in the R folder. right now it's just notes on things I'll need to do to make all those play right together. 

# Part 1: Load Packages -------------------------------------------------------
packages<- c("tidyverse", "sf", "terra", "dplyr", "tidyterra")

pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/exposure.R"))

expert_weights <- clean_exweights(here::here("data/raw_data/expert_weights_may12_2025.csv"))

density_paths <- dir(here::here("data/raw_data/leirness_model_outputs/"),
    pattern = "density.tif$",
    full.names = TRUE)
cv_paths <- dir(here::here("data/raw_data/leirness_model_outputs/"),
                     pattern = "CV.tif$",
                     full.names = TRUE)

densities <- map(density_paths, rast) %>% 
  rast()

cvs <- map(cv_paths, rast) %>% 
  rast()

distribution_rasts <- distribution_mc(n_sims = 20, 
                                      densities,
                                      cvs,
                                      expert_weights)

writeRaster(distribution_rasts, here::here("output/distribution_rasts.tif"))



#load in WEAs
calls <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp") #this contains the two oregon areas
leases <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp") #this contains the five CA leases

weas <- clean_weas(l = leases, c = calls)

sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))

exposure_vals <- calculate_exposure(distribution_rasts, weas, sp)


