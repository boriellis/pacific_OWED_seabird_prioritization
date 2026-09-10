##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 05: Results Tables and Figures ##################################
#-------------------------------------------------------------------------
#
# Produces the main-manuscript outputs from the priority scores (main
# weighting, 3-2-1):
#   - results tables (Table 1, and the region-wide supplement version)
#   - priority-score boxplots (Figure 4, panel 1)
#   - vulnerability scatter plot (Figure 4, panel 2)
#   - priority rank ridge plot (Figure 5)
#
# CA-region outputs go in the main manuscript; region-wide ("all") outputs
# go in Appendix K. Weighting-scheme sensitivity outputs (Appendix J) are
# produced in scripts/06_sensitivity_analysis.R.
#
# Rank uncertainty is derived from a Monte Carlo resampling of the exposure
# distribution (priority_mc): each iteration draws one exposure value per
# species, recomputes vulnerability, and re-ranks, giving a distribution of
# ranks per species. This propagates exposure uncertainty into rank space,
# which cannot be obtained from each species' marginal score distribution
# because ranks are relational.
#
# Inputs:  output/exposure_values/cleaned_exposure.rds
#          output/sensitivity_values/sensitivity_sum.rds
#          output/status_values/status.rds
#          output/priority_values/priority_scores_321.rds
#          data/raw_data/ (species list, Clements taxonomy)
# Outputs: output/rank_mc/priority_ranks_{weights}.rds
#          paper/CA_results_table.csv, paper/POCS_results_table.csv
#          paper/boxplot_CA.png, paper/boxplot_all.png
#          paper/vulnerability_scatter.png
#          paper/3_2_1_ridgeplot.png
#-------------------------------------------------------------------------


# Part 1: Setup ---------------------------------------------------------------

packages <- c("tidyverse", "sf", "terra", "tidyterra", "ggridges", "scico", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/results.R"))
source(here::here("R/visualizations.R"))

# the three rescaled factors and the main-analysis (321) priority scores
exposure    <- read_rds(here::here("output/exposure_values/cleaned_exposure.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_values/sensitivity_sum.rds"))
status      <- read_rds(here::here("output/status_values/status.rds"))
priority321 <- read_rds(here::here("output/priority_values/priority_scores_321.rds"))


# Part 2: Rank Monte Carlo ----------------------------------------------------
# SLOW. Resamples the exposure distribution to build rank distributions, for
# each weight set (main + the four weighting-sensitivity alternatives, since
# 06_sensitivity_analysis.R reads all five from this same folder). Only needs
# rerunning if the upstream exposure, sensitivity, or status values change;
# otherwise the saved outputs are reused below.

regenerate_mc <- FALSE   # set TRUE to rebuild the rank MC outputs

mc_dir <- here::here("output/rank_mc")

if (regenerate_mc) {
  dir.create(mc_dir, recursive = TRUE, showWarnings = FALSE)
  
  weight_sets <- list("321" = c(3, 2, 1), "111" = c(1, 1, 1), "211" = c(2, 1, 1),
                      "121" = c(1, 2, 1), "112" = c(1, 1, 2))
  
  iwalk(weight_sets, \(w, label) {
    message("rank MC: weights ", label)
    ranks <- priority_mc(exposure, sensitivity, status, w = w)
    saveRDS(ranks, file.path(mc_dir, str_glue("priority_ranks_{label}.rds")))
  })
}

ranks321 <- readRDS(here::here("output/rank_mc/priority_ranks_321.rds"))


# Part 3: Results tables (Table 1 + Appendix K) -------------------------------
# Per-species table of exposure (% overlap, 95% CI), collision (CV) and
# displacement (DV) sensitivity, IUCN category, vulnerability score (95% CI),
# and priority rank with its MC uncertainty band (central 95%).

rank_summary <- summarize_ranks()

# CA region — Table 1, main manuscript
results_table_CA <- clean_priority_vals(priority321, exposure, sensitivity, status,
                                        rank_summary, "CA", rank_band = "95")
write_excel_csv(results_table_CA, here::here("paper/CA_results_table.csv"))

# all lease + planning areas — Appendix K
results_table_all <- clean_priority_vals(priority321, exposure, sensitivity, status,
                                         rank_summary, "all", rank_band = "95")
write_excel_csv(results_table_all, here::here("paper/appendix_K/POCS_results_table.csv"))


# Part 4: Priority boxplots (Figure 4, panel 1 + Appendix K) ------------------
# Each species' full vulnerability distribution, ordered taxonomically with
# family shown as a background band. Boxes span the 2.5-97.5% quantiles.

sp_list <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
tax     <- read_csv(here::here("data/raw_data/Clements-v2024-October-2024-rev.csv"))

# CA region — Figure 4, main manuscript
boxplot_CA <- make_boxplot(sp_list, tax, priority321, "CA")
ggsave(here::here("paper/boxplot_CA.png"), plot = boxplot_CA,
       width = 190, height = 95, units = "mm", dpi = 500)

# all lease + planning areas — Appendix K
boxplot_all <- make_boxplot(sp_list, tax, priority321, "all")
ggsave(here::here("paper/appendix_K/boxplot_all.png"), plot = boxplot_all,
       width = 190, height = 170, units = "mm", dpi = 500)


# Part 5: Vulnerability scatter (Figure 4, panel 2) ---------------------------
# Exposure x sensitivity, colored by vulnerability score along each species'
# exposure CI, sized by IUCN status.

vuln_scatter <- make_vuln_scatter(priority321, w = c(3, 2, 1))
ggsave(here::here("paper/vulnerability_scatter.png"), plot = vuln_scatter,
       width = 210, height = 100, units = "mm", dpi = 500)


# Part 6: Priority rank ridge plot (Figure 5) ---------------------------------

cols321 <- score_colours(priority321, "CA", limits = c(2^-6, 2^6))

p_ridge <- ridgeplot(ranks321, priority321, cols321$common_name, cols321$color, "CA")
ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = p_ridge,
       width = 90, height = 160, units = "mm", dpi = 500)