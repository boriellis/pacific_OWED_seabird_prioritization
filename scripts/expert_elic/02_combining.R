
# Part 1: Load Packages -------------------------------------------------------
packages<- c("sf", "terra", "dplyr", "tidyverse")

pacman::p_load(packages, character.only = TRUE); rm(packages)

ex_sums <- read_csv(here::here("data/processed_data/unk_HML.csv"))
orig_data <- read_csv(here::here("data/processed_data/cleaned_data.csv"))
