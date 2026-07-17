##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 03: Status Estimation ##########################################
#-------------------------------------------------------------------------
#
# Assigns each species a conservation status weight derived from its IUCN
# Red List category, for use as the status axis of the OWED vulnerability
# framework. Categories are mapped to the framework's [0.5, 2.0] scale with
# a constant 4^(1/4) ~ 1.414 ratio between successive categories (LC = 0.5,
# NT ~ 0.71, VU = 1.0, EN ~ 1.41, CR = 2.0), so status combines on equal
# footing with exposure and sensitivity into vulnerability.
#
# This script sources the functions in R/status.R and runs the pipeline:
#
#   1. Reconcile local scientific names to BirdLife taxonomy
#   2. Join the IUCN Red List categories
#   3. Map them to status weights (clean_statuses)
#
# Inputs:  data/raw_data/ (master species list, raw IUCN Red List export)
# Outputs: output/ (status weights .rds)
#-------------------------------------------------------------------------


# Part 1: Load Packages --------------------------------------------------------

packages <- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

# source the status functions
source(here::here("R/status.R"))


# Part 2: Load data ------------------------------------------------------------

# master species list (taxonomy) and the raw IUCN Red List export
# (BirdLife DataZone, seabird/waterbird filter)
sp   <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
iucn <- read_csv(here::here("data/raw_data/raw_iucn_list.csv"))


# Part 3: Clean and assign status weights --------------------------------------

# reconcile taxonomy, join IUCN categories, and map to [0.5, 2.0] weights
status <- clean_statuses(sp, iucn)

saveRDS(status, here::here("output/status_values/status.rds"))
