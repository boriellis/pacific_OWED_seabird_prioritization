library(tidyverse)
source(here::here("R/priority.R"))

exposure1000 <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_sum.rds"))
status <- read_rds(here::here("output/status.rds"))

# Table
priority_table1000<- calc_priority(exposure1000, 
                                sensitivity, 
                                status, 
                                w = c(3, 2, 1)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)#%>% 
  #filter(region == "CA")
  
saveRDS(priority_table1000, "output/priority_scores_1000.rds")






