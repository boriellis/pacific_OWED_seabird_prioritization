


packages<- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/sensitivity.R"))

sp<- read_csv(here::here("data/raw_data/total_sp_list.csv"))
cv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_CV.csv"))
dv <- read_csv(here::here("data/raw_data/sensitivity/POCS_VulnIndex_DV.csv"))



cleaned_sens <- clean_sens(dv = dv, cv = cv, sp)

sensitivity <- rescale_sens(sp, cleaned_sens, sum)

saveRDS(sensitivity, here::here("output/sensitivity_sum.rds"))
