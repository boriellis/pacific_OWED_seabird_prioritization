##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 04: Priority (Vulnerability) Scores ############################
#-------------------------------------------------------------------------
#
# Combines the three rescaled factors (exposure, sensitivity, status) into
# priority/vulnerability scores via calc_priority(), which raises each factor
# to a weight exponent and multiplies. The exposure distribution propagates
# through; sensitivity and status act as fixed per-species multipliers.
#
# The main analysis uses weights c(3, 2, 1) (exposure weighted most heavily).
# The remaining weight sets are the weight-sensitivity analysis, showing how
# the emphasis among factors affects priority scores.
#
# Inputs:  output/exposure_values/cleaned_exposure.rds
#          output/sensitivity_values/sensitivity_sum.rds
#          output/status_values/status.rds
# Outputs: output/priority_values/priority_scores_{weights}.rds  (one per weight set)
#-------------------------------------------------------------------------


# Setup -----------------------------------------------------------------------

library(tidyverse)
source(here::here("R/priority.R"))

exposure    <- read_rds(here::here("output/exposure_values/cleaned_exposure.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_values/sensitivity_sum.rds"))
status      <- read_rds(here::here("output/status_values/status.rds"))

out_dir <- here::here("output/priority_values")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Build priority tables across weight sets -------------------------------------
# 321 = main analysis (exposure heaviest); the rest are the weight-sensitivity
# analysis. Label is the weight vector concatenated (e.g. c(3,2,1) -> "321").

weight_sets <- list(
  "321" = c(3, 2, 1),
  "111" = c(1, 1, 1),
  "211" = c(2, 1, 1),
  "121" = c(1, 2, 1),
  "112" = c(1, 1, 2)
)

build_priority_table <- function(w) {
  calc_priority(exposure, sensitivity, status, w = w) %>%
    left_join(exposure,    by = c("alpha_code", "region")) %>%
    left_join(sensitivity, by = "alpha_code") %>%
    left_join(status,      by = "alpha_code") %>%
    select(-raw_overlap, -scaled_overlap)
}

iwalk(weight_sets, \(w, label) {
  message("priority table: weights ", label)
  tbl <- build_priority_table(w)
  saveRDS(tbl, file.path(out_dir, str_glue("priority_scores_{label}.rds")))
})
