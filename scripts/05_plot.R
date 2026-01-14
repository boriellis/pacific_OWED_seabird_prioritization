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
#####################       RESULTS BOXPLOTS    ###########################
#####################                           ###########################
###########################################################################

#load in data
sp_list <- read_csv("data/raw_data/total_sp_list.csv")
tax <- read_csv("data/raw_data/Clements-v2024-October-2024-rev.csv")
priority_dists <- read_rds("output/priority_scores_1000.rds")


#CA
boxplot_CA <- make_boxplot(sp_list, tax, priority_dists, "CA")
ggsave(here::here("paper/boxplot_CA.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

#all lease areas
boxplot_all <- make_boxplot(sp_list, tax, priority_dists, "all")
ggsave(here::here("paper/boxplot_all.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)




###########################################################################
#####################                           ###########################
#####################      UNCERTAINTY PLOTS    ###########################
#####################                           ###########################
###########################################################################


# Generate rank output dataframes DON'T RERUN  -----------------------------------------
#generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all

#load data
e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

# #3, 2, 1
# priority_ranks321 <- priority_mc(e, se, st, w = c(3, 2, 1))
# saveRDS(priority_ranks321, "paper/321priority_ranks_1000_for_plots.rds")
# 
# 
# #1, 1, 1
# priority_ranks111 <- priority_mc(e, se, st, w = c(1, 1, 1))
# saveRDS(priority_ranks111, "paper/111priority_ranks_1000_for_plots.rds")
# 
# #2, 1, 1
# priority_ranks211 <- priority_mc(e, se, st, w = c(2, 1, 1))
# saveRDS(priority_ranks211, "paper/211priority_ranks_1000_for_plots.rds")
# 
# 
# #1, 2, 1
# priority_ranks121 <- priority_mc(e, se, st, w = c(1, 2, 1))
# saveRDS(priority_ranks121, "paper/121priority_ranks_1000_for_plots.rds")
# 
# #1, 1, 2
# priority_ranks112 <- priority_mc(e, se, st, w = c(1, 1, 2))
# saveRDS(priority_ranks112, "paper/112priority_ranks_1000_for_plots.rds")
# 





# set up ------------------------------------------------------------------

# set colors
topsps_CA <- c("Pink-footed Shearwater", "Cassin's Auklet", "Red Phalarope", "Red-necked Phalarope", "Ashy Storm-Petrel", "Guadalupe Murrelet", "Pomarine Jaeger",  "Rhinoceros Auklet", "Craveri's Murrelet", "Sooty Shearwater", "Scripps's Murrelet", "Townsend's Storm-Petrel", "Buller's Shearwater", "Sabine's Gull", "South Polar Skua", "Marbled Murrelet", "Bonaparte's Gull", "Black Scoter", "Short-tailed Albatross","Northern Fulmar", "Hawaiian Petrel", "California Gull")

# if you want colors to be ordered by priority
spcolors_pri <- c("#FBE9B1", "#FDE1AB", "#F1CC9B", "#F3BB84", "#E5A67C", "#E9946F", "#E88164", "#DB705F", "#C5655F", "#AE6363", "#996169", "#86606E", "#755F72", "#655E76", "#565B7A", "#405578", "#25486D", "#103657", "#07243E", "#021326", "#020F1C",  "#01080F")


# load differently weighted simulations

ranks321 <- readRDS(here::here("paper/321priority_ranks_1000_for_plots.rds"))
ranks111 <- readRDS(here::here("paper/111priority_ranks_1000_for_plots.rds"))
ranks211 <- readRDS(here::here("paper/211priority_ranks_1000_for_plots.rds"))
ranks121 <- readRDS(here::here("paper/121priority_ranks_1000_for_plots.rds"))
ranks112 <- readRDS(here::here("paper/112priority_ranks_1000_for_plots.rds"))


# bind all into one dataframe
ranks_all <- bind_rows(
  mutate(ranks111, scenario = "111"),
  mutate(ranks211, scenario = "211"),
  mutate(ranks121, scenario = "121"),
  mutate(ranks112, scenario = "112")
)

max_ranks <- tibble(
  scenario = c("111", "211", "121", "112"),
  max_rank = c(37, 43, 25, 20)
)

ranks_all <- ranks_all %>% 
  left_join(max_ranks, by = "scenario")



# ridge plots -------------------------------------------------------------

#main plot
p1 <- ridgeplot(ranks321, topsps_CA, spcolors_pri, "CA", 47)
ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = p1, width = 10, height = 8, units = "in", dpi = 300)

#sensitivity plots to combine in illustrator 
p2 <- ridgeplot2(ranks111, topsps_CA, spcolors_pri, "CA", 37)
ggsave(here::here("paper/1_1_1_ridgeplot.png"), plot = p2, width = 8, height = 6, units = "in", dpi = 300)

p3 <- ridgeplot2(ranks211, topsps_CA, spcolors_pri, "CA", 43)
ggsave(here::here("paper/2_1_1_ridgeplot.png"), plot = p3, width = 8, height = 6, units = "in", dpi = 300)

p4 <- ridgeplot2(ranks121, topsps_CA, spcolors_pri, "CA", 25)
ggsave(here::here("paper/1_2_1_ridgeplot.png"), plot = p4, width = 8, height = 6, units = "in", dpi = 300)

p5 <- ridgeplot2(ranks112, topsps_CA, spcolors_pri, "CA", 20)
ggsave(here::here("paper/1_1_2_ridgeplot.png"), plot = p5, width = 8, height = 6, units = "in", dpi = 300)


