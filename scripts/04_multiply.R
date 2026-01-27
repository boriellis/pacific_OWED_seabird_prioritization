library(tidyverse)
source(here::here("R/priority.R"))

exposure1000 <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_sum.rds"))
status <- read_rds(here::here("output/status.rds"))

#  321 Table
priority_table1000_321<- calc_priority(exposure1000, 
                                sensitivity, 
                                status, 
                                w = c(3, 2, 1)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)#%>% 
  #filter(region == "CA")
  
saveRDS(priority_table1000_321, "output/priority_scores_1000_321.rds")


#  111 Table
priority_table1000_111<- calc_priority(exposure1000, 
                                       sensitivity, 
                                       status, 
                                       w = c(1, 1, 1)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)#%>% 
#filter(region == "CA")

saveRDS(priority_table1000_111, "output/priority_scores_1000_111.rds")



#  211 Table
priority_table1000_211<- calc_priority(exposure1000, 
                                       sensitivity, 
                                       status, 
                                       w = c(2, 1, 1)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)#%>% 
#filter(region == "CA")

saveRDS(priority_table1000_211, "output/priority_scores_1000_211.rds")


#  121 Table
priority_table1000_121<- calc_priority(exposure1000, 
                                       sensitivity, 
                                       status, 
                                       w = c(1, 2, 1)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)

saveRDS(priority_table1000_121, "output/priority_scores_1000_121.rds")


#  112 Table
priority_table1000_112<- calc_priority(exposure1000, 
                                       sensitivity, 
                                       status, 
                                       w = c(1, 1, 2)) %>% 
  left_join(exposure1000, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  select(-raw_overlap, -scaled_overlap)#%>% 
#filter(region == "CA")

saveRDS(priority_table1000_112, "output/priority_scores_1000_112.rds")


