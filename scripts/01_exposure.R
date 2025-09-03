#this is the new holding area that will become the script that does the whole workflow analysis using the functions in the R folder. right now it's just notes on things I'll need to do to make all those play right together. 

library(tidyverse)
library(terra)

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





