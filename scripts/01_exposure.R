

# Part 1: Load Packages -------------------------------------------------------
packages<- c("tidyverse", "sf", "terra", "dplyr", "tidyterra")

pacman::p_load(packages, character.only = TRUE); rm(packages)


# Part 2: Run MC simulations to make distribution rasters -----------------

#source the functions to calculate exposure
source(here::here("R/exposure.R"))

#clean qualtrics survey output to get the expert provided similarity weights
expert_weights <- clean_exweights(here::here("data/raw_data/expert_weights_may12_2025.csv"))

#load in the original Leirness average distribution maps and associated CV maps
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

#simulate possible distribution rasters based on the uncertainty in the CV rasters, including for the three elicited species (1 raster per expert per simulation)
distribution_rasts <- distribution_mc(n_sims = 100, 
                                      densities,
                                      cvs,
                                      expert_weights)

writeRaster(distribution_rasts, ("/Volumes/seagate/distribution_rasts.tif"))


# Make a df of exposure proportion per simulation -------------------------


#load in WEAs
calls <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp") #this contains the two oregon areas
leases <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp") #this contains the five CA leases

weas <- clean_weas(l = leases, c = calls)

sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))

exposure_vals <- calculate_exposure(distribution_rasts, weas, sp)

saveRDS(exposure_vals, "/Volumes/seagate/exposure_10000sims.rds")

