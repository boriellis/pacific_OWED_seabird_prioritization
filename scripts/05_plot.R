library(tidyverse)
library(ggridges)
library(scico)

#how to generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all


# e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
# se <- read_rds(here::here("output/sensitivity_sum.rds"))
# st <- read_rds(here::here("output/status.rds"))
# 
# w <- c(1, 1, 2)
# 
# priority_once <- function(x) {
#   e %>%
#     mutate(scaled_overlap = map_dbl(scaled_overlap, \(x) sample(x, 1))) %>%
#     left_join(se, by = "alpha_code") %>%
#     left_join(st, by = "alpha_code") %>%
#     group_by(region) %>%
#     mutate(e = scaled_overlap^w[1],
#            es = scaled_overlap^w[1] * sensitivity^w[2],
#            ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3],
#            pri_rank = min_rank(desc(ess))) %>%
#     ungroup() %>%
#     select(region, alpha_code, common_name, pri_rank)
# }
# result <- map(1:1000, priority_once) %>%
#   list_rbind()
# 
# 
# saveRDS(result, "paper/112priority_ranks_1000_for_plots.rds")


#results table for CA

raw_scores <- read_rds(here::here("output/priority_scores_1000.rds"))

CA_table <- raw_scores %>% 
  filter(region == "CA") %>% 
  mutate(mean_raw_overlap = map_dbl(outliers_rm, mean),
         lwr_raw_overlap = map_dbl(outliers_rm, min), #this is technically the 2.5% quantile since I already capped values below that to that value at the winzorization step
         upr_raw_overlap = map_dbl(outliers_rm, max) #97.5% quantile of the raw. 
         ) %>%
  select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap, upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr) %>% 
  arrange(desc(ess)) %>%         
  mutate(pri_rank = row_number())


#add in data deficient species

se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

dd_species <- se %>% 
  left_join(st, by = "alpha_code") %>% 
  anti_join(CA_table, by = "alpha_code") %>% 
  mutate(mean_raw_overlap = NA_real_,
         lwr_raw_overlap = NA_real_,
         upr_raw_overlap = NA_real_,
         ess = NA_real_,
         ess_lwr = NA_real_,
         ess_upr = NA_real_
         ) %>% 
  select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap, upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr)
  


total_CA_table <- bind_rows(CA_table, dd_species)

formatted_results_table <- total_CA_table %>% 
  mutate(mean_exp_percent = mean_raw_overlap*100,
         lwr_exp_percent = lwr_raw_overlap*100,
         upr_exp_percent = upr_raw_overlap *100,
         exp_ci = sprintf("%.3f (%.3f, %.3f)", mean_exp_percent, lwr_exp_percent, upr_exp_percent),
         ess_ci = sprintf("%.3f (%.3f, %.3f)", ess, ess_lwr, ess_upr)
         ) %>% 
  select(common_name, exp_ci, CV, DV, rl_category, ess_ci, pri_rank)
  


#save to paper folder
write_csv(formatted_results_table, "paper/CA_results_table.csv")









