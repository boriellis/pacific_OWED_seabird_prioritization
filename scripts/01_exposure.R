##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 01: Exposure Estimation ########################################
#-------------------------------------------------------------------------
#
# Estimates seabird exposure to offshore wind energy development (OWED) in
# the California Current, quantified as the proportion of each species'
# predicted regional distribution that overlaps proposed and leased wind
# energy areas (WEAs).
#
# Species distributions are derived from the original bootstrapped seasonal
# density predictions of Leirness et al. (2021): for each species, 200
# bootstrap realizations of seasonal density are summed into annual
# distributions, preserving the spatial covariance of each model fit. 
#
# This script sources the functions in R/exposure.R and runs the pipeline:
#
#   1. Clean expert similarity weights (clean_exweights)
#   2. Combine seasonal bootstraps into annual distributions per model
#      (combine_seasons)
#   3. Build elicited-species distributions from weighted surrogate models
#      (combine_models)
#   4. Assemble and aggregate WEA polygons (clean_weas)
#   5. Calculate raw proportional overlap per species and scale
#      (calculate_exposure)
#   6. Winsorize and rescale to final exposure values (clean_exposure)
#
# Inputs:  data/raw_data/ (Leirness bootstraps, Qualtrics weights, BOEM
#          shapefiles, species list)
# Outputs: output/ (raw and cleaned exposure .rds)
#
# NOTE: the bootstrap rasters (~36 GB) are stored locally and gitignored;
# they are not redistributed with this repository.
#-------------------------------------------------------------------------

# Part 1: Load Packages --------------------------------------------------------

packages <- c("tidyverse", "sf", "terra", "dplyr", "tidyterra", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

# source the exposure functions
source(here::here("R/exposure.R"))


# Part 2: Clean expert similarity weights --------------------------------------

# clean Qualtrics survey output into long-format expert similarity weights
# (used to build elicited-species distributions in Part 4)
expert_weights <- clean_exweights(
  here::here("data/raw_data/expert_weights_may12_2025.csv")
)


# Part 3: Combine seasonal bootstraps into annual distributions -----------------

# Leirness et al. (2021) bootstrap rasters: one file per model x season, each
# with 200 layers (bootstrap_001 ... bootstrap_200). Stored locally, gitignored.
boot_dir  <- here::here("data/raw_data/leirness_bootstrapped_models")
model_dir <- "/Volumes/seagate/bootstrap_annual_models"   # annual bootstrap output (large; off-repo)

# every model name, parsed off the filenames (everything before the season token)
models <- dir(boot_dir, pattern = "\\.tif$") %>% 
  str_extract("^.+(?=_(spring|summer|fall|winter)_)") %>% 
  unique()

# For each model: load its seasonal bootstraps, sum seasons within each
# bootstrap iteration into annual distributions, and write to disk.
# Written out one model at a time to keep memory in check.
walk(models, \(m) {
  message("Combining seasons: ", m)
  files  <- dir(boot_dir, pattern = str_glue("^{m}_"), full.names = TRUE)
  annual <- combine_seasons(rast(files), model = m)
  writeRaster(annual, str_glue("{model_dir}/{m}_annual_boot.tif"), overwrite = TRUE)
})


# Part 4: Build elicited-species distributions ---------------------------------

# For each elicited species x expert, weight and combine the annual bootstrap
# distributions of the surrogate models that expert selected. One output raster
# per species x expert (200 layers each); exposure later pools across experts.
elicited_dir <- "/Volumes/seagate/bootstrap_elicited_models"   # off-repo, as above

elicited_sp <- unique(expert_weights$alpha_code)
experts     <- unique(expert_weights$expert[expert_weights$weight > 0])

for (s in elicited_sp) {
  for (e in experts) {
    message("Combining models: ", s, " x expert ", e)
    elicited_rasts <- combine_models(s, e, model_dir, expert_weights)
    writeRaster(elicited_rasts,
                str_glue("{elicited_dir}/{s}_expert{e}_annual_boot.tif"),
                overwrite = TRUE)
  }
}



# Part 5: Assemble WEA polygons ------------------------------------------------

# BOEM planning areas (two OR areas) and lease outlines (five CA leases);
# clean_weas subsets to the WEAs of interest and builds lease-, state-, and
# region-level polygons.
calls  <- vect(here::here("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp"))
leases <- vect(here::here("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp"))
weas   <- clean_weas(l = leases, c = calls)


# Part 6: Calculate raw exposure -----------------------------------------------

# species information table (alpha codes, exposure_model, regional flag)
sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))

# proportional overlap of each species' annual bootstrap distribution with the
# WEAs, at every spatial scale — one distribution of values per species x scale
exposure_vals <- calculate_exposure(
  modeled_path  = model_dir,
  elicited_path = elicited_dir,
  v = weas,
  sp = sp
)

saveRDS(exposure_vals, here::here("output/raw_exposure_200boot.rds"))


# Part 7: Winsorize and rescale ------------------------------------------------
# NOTE: paused pending inspection of raw exposure. With bootstrap-based
# distributions the winsorization step may no longer be needed (it was largely
# there to tame outliers from the old per-pixel MC). Check how much winsorize()
# actually clips before finalizing clean_exposure(), e.g.:
#
#   raw <- exposure_vals$raw_overlap[[1]]
#   sum(raw != winsorize(raw)) / length(raw)   # fraction clipped
#
# cleaned_exposure <- clean_exposure(exposure_vals)
# saveRDS(cleaned_exposure, here::here("output/cleaned_exposure_200boot.rds"))