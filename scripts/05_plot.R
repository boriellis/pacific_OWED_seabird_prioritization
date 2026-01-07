library(tidyverse)
library(ggridges)
library(scico)

source(here::here("R/visualizations.R"))



###########################################################################
#####################                           ###########################
#####################       RESULTS TABLES      ###########################
#####################                           ###########################
###########################################################################


#load data
raw_scores <- read_rds(here::here("output/priority_scores_1000.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

#CA
formatted_results_table_CA <- clean_priority_vals(raw_scores, se, st, "CA")
write_csv(formatted_results_table_CA, "paper/CA_results_table.csv")

#all lease areas
formatted_results_table_all <- clean_priority_vals(raw_scores, se, st, "all")
write_csv(formatted_results_table_all, "paper/POCS_results_table.csv")



###########################################################################
#####################                           ###########################
#####################      UNCERTAINTY PLOTS    ###########################
#####################                           ###########################
###########################################################################

# Generate rank output dataframes -----------------------------------------
#generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all
e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

#3, 2, 1
priority_ranks321 <- priority_mc(e, se, st, w = c(3, 2, 1))
saveRDS(priority_ranks321, "paper/321priority_ranks_1000_for_plots.rds")


#1, 1, 1
priority_ranks111 <- priority_mc(e, se, st, w = c(1, 1, 1))
saveRDS(priority_ranks111, "paper/111priority_ranks_1000_for_plots.rds")

#2, 1, 1
priority_ranks211 <- priority_mc(e, se, st, w = c(2, 1, 1))
saveRDS(priority_ranks211, "paper/211priority_ranks_1000_for_plots.rds")


#1, 2, 1
priority_ranks121 <- priority_mc(e, se, st, w = c(1, 2, 1))
saveRDS(priority_ranks121, "paper/121priority_ranks_1000_for_plots.rds")

#1, 1, 2
priority_ranks112 <- priority_mc(e, se, st, w = c(1, 1, 2))
saveRDS(priority_ranks112, "paper/112priority_ranks_1000_for_plots.rds")






