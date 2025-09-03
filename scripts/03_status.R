packages<- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/status.R"))

sp <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
iucn <- read_csv(here::here("data/raw_data/raw_iucn_list.csv"))

status <- clean_statuses(sp, iucn)

saveRDS(status, here::here("output/status.rds"))
