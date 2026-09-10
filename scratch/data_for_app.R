#this script makes the data file that gets copied into the app folder for use in the shiny app. 

packages<- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

#load up all the relevant dataframes to combine


tax <- read_csv("data/raw_data/Clements-v2024-October-2024-rev.csv") #where to get family and order columns from
e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds")) #where to get raw and rescaled overlap proportions
se <- read_rds(here::here("output/sensitivity_sum.rds")) #where to get raw CV and DV values
st <- read_rds(here::here("output/status.rds")) #where to get IUCN statuses and rescaled values


tax <- tax %>% 
  rename(scientific_name = `scientific name`,
         index = `sort v2024`) 

app_data <- se %>%
  select(alpha_code, common_name, scientific_name, raw_CV = CV, raw_DV = DV) %>%
  left_join(tax %>% select(index, scientific_name, order, family), 
            by = "scientific_name") %>%
  left_join(e %>% select(alpha_code, region, raw_overlap, outliers_rm, scaled_overlap), 
            by = "alpha_code") %>%
  left_join(st %>% select(alpha_code, rl_category, status), 
            by = "alpha_code") %>%
  select(index, order, family, common_name, scientific_name, alpha_code, 
         region, raw_overlap, outliers_rm, scaled_overlap, 
         raw_CV, raw_DV, rl_category, status)


saveRDS(app_data, here::here("output/app_data.rds"))
