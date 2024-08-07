###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------
packages<- c("sf", "terra", "dplyr")

pacman::p_load(packages, character.only = TRUE); rm(packages)

# Part 2: Import data -------------------------------------------------------
calls <- vect("data/BOEM_Wind_Planning_Areas_04_29_2024.shp") #this is throwing an error
