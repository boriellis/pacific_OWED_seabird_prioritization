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
# Species distributions are derived from the bootstrapped seasonal density
# predictions of Leirness et al. (2021): the seasonal bootstraps are summed
# into annual distributions (preserving each fit's spatial covariance), with
# outlier iterations removed via a median*k rule (k = 1000). The pooled
# overlaps are then rescaled onto the framework's [0.5, 2.0] scale, anchored
# on the central 99% of the distribution.
#
# The two processing parameters (k and anchor) are set to their main-analysis
# values here; scripts/06_sensitivity_analysis.R re-runs the pipeline across
# alternative values.
#
# This script sources R/exposure.R and runs the pipeline:
#   1. Clean expert similarity weights          (clean_exweights)
#   2. Flag/drop outlier bootstrap iterations    (build_keep_index)
#   3. Combine seasonal bootstraps -> annual     (combine_seasons)
#   4. Build elicited-species distributions      (combine_models)
#   5. Assemble and aggregate WEA polygons       (clean_weas)
#   6. Calculate raw proportional overlap        (calculate_exposure)
#   7. Rescale to final exposure values          (clean_exposure)
#
# Inputs:  data/raw_data/ (Leirness bootstraps, Qualtrics weights, BOEM
#          shapefiles, species list)
# Outputs: output/exposure_values/raw_exposure.rds
#          output/exposure_values/cleaned_exposure.rds
#          + annual bootstrap rasters on the external drive (see paths below)
#
# NOTE: the bootstrap rasters (~36 GB) are stored locally and gitignored;
# they are not redistributed with this repository.
#
# NOTE: modeled_dir/elicited_dir below point to an external drive path
# specific to the author's machine. Update these to a local path before
# rerunning this script.
#-------------------------------------------------------------------------


# Part 1: Load packages & set parameters --------------------------------------

packages <- c("tidyverse", "sf", "terra", "tidyterra", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/exposure.R"))

# main-analysis processing parameters
k      <- 1000     # outlier cutoff multiplier (median * k)
anchor <- 0.99     # central quantile span for exposure rescaling

# paths for the (large, off-repo) annual bootstrap rasters
boot_dir     <- here::here("data/raw_data/leirness_bootstrapped_models")
modeled_dir  <- str_glue("/Volumes/seagate/bootstrap_annual_outliers_rm/k_{k}/modeled")
elicited_dir <- str_glue("/Volumes/seagate/bootstrap_annual_outliers_rm/k_{k}/elicited")


# Part 2: Clean expert similarity weights -------------------------------------

# long-format expert similarity weights (used to build elicited species in Part 4)
expert_weights <- clean_exweights(
  here::here("data/raw_data/expert_weights_may12_2025.csv")
)


# Part 3: Combine seasonal bootstraps into annual distributions ---------------
# Drop outlier iterations (bootstraps with max cell value > median(max cell
# value) * 1000) and sum seasons within each surviving iteration, producing
# one annual stack per model.

models <- dir(boot_dir, pattern = "\\.tif$") %>%
  str_extract("^.+(?=_(spring|summer|fall|winter)_)") %>%
  unique()

keep_index <- build_keep_index(boot_dir, k = k)

dir.create(modeled_dir, recursive = TRUE, showWarnings = FALSE)

walk(models, \(m) {
  message("combining seasons: ", m)
  files  <- dir(boot_dir, pattern = str_glue("^{m}_"), full.names = TRUE)
  annual <- combine_seasons(rast(files), model = m, keep = keep_index[[m]])
  writeRaster(annual, str_glue("{modeled_dir}/{m}_annual_boot.tif"), overwrite = TRUE)
})


# Part 4: Build elicited-species distributions --------------------------------
# For each elicited species x expert, weight and combine surrogate models'
# annual bootstraps (reconciled by intersection across surrogates).

elicited_sp <- unique(expert_weights$alpha_code)
experts     <- unique(expert_weights$expert[expert_weights$weight > 0])

dir.create(elicited_dir, recursive = TRUE, showWarnings = FALSE)

for (s in elicited_sp) {
  for (e in experts) {
    message("combining models: ", s, " x expert ", e)
    elicited_rasts <- combine_models(s, e, modeled_dir, expert_weights)
    writeRaster(elicited_rasts,
                str_glue("{elicited_dir}/{s}_expert{e}_annual_boot.tif"),
                overwrite = TRUE)
  }
}


# Part 5: Assemble WEA polygons -----------------------------------------------

calls  <- vect(here::here("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp"))
leases <- vect(here::here("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp"))
weas   <- clean_weas(l = leases, c = calls)


# Part 6: Calculate raw exposure ----------------------------------------------

sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))

raw_exposure <- calculate_exposure(
  modeled_path  = modeled_dir,
  elicited_path = elicited_dir,
  v  = weas,
  sp = sp
)

saveRDS(raw_exposure, here::here("output/exposure_values/raw_exposure.rds"))


# Part 7: Rescale exposure ----------------------------------------------------
# Rescale pooled overlaps onto [0.5, 2.0], anchored on the central 99%.

cleaned_exposure <- clean_exposure(raw_exposure, anchor = anchor)

saveRDS(cleaned_exposure, here::here("output/exposure_values/cleaned_exposure.rds"))