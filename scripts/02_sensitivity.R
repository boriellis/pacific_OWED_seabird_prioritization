##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 02: Sensitivity Estimation #####################################
#-------------------------------------------------------------------------
#
# Estimates seabird sensitivity to offshore wind energy development (OWED)
# in the California Current from the collision (CV) and displacement (DV)
# vulnerability scores of Kelsey et al. (2025). Scores are joined onto the
# project's master species list, restricted to regional species, and CV and DV
# are then each rescaled to a 0-1 range. User can then select if they want CV, 
# DV, the highest metric, or the two metrcis summed together as the sensitivity 
# value. The selevtion is then rescaled to the framework's common [0.5, 2.0] 
# geometric range.
#
# This script sources the functions in R/sensitivity.R and runs the pipeline:
#
#   1. Clean and combine CV & DV values onto the master taxonomy
#      (clean_sens)
#   2. Restrict to regional species and rescale the selected metric to
#      [0.5, 2.0] (rescale_sens)
#
# The rescaled metric is selectable (CV, DV, their sum, or the higher of the
# two); "sum" is used here.
#
# Inputs:  data/raw_data/ (master species list, Kelsey et al. 2025 CV & DV)
# Outputs: output/ (rescaled sensitivity .rds)
#-------------------------------------------------------------------------


# Part 1: Load Packages --------------------------------------------------------

packages <- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

# source the sensitivity functions
source(here::here("R/sensitivity.R"))


# Part 2: Load data ------------------------------------------------------------

# master species list (taxonomy + regional flag) and the Kelsey et al. (2025)
# collision (CV) and displacement (DV) vulnerability tables
sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
cv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_CV.csv"))
dv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_DV.csv"))


# Part 3: Clean and combine sensitivity values ---------------------------------

# join CV and DV onto the master taxonomy (master list drives taxonomy)
cleaned_sens <- clean_sens(sp = sp, cv = cv, dv = dv)


# Part 4: Rescale sensitivity --------------------------------------------------

# restrict to regional species and rescale the chosen metric to [0.5, 2.0].
# sel options: "CV", "DV", "sum", "highest"
sensitivity <- rescale_sens(sp, cleaned_sens, sel = "sum")

saveRDS(sensitivity, here::here("output/sensitivity_values/sensitivity_sum.rds"))
