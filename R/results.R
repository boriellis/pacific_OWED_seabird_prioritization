##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Results Function Definitions ###########################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/05_plot.R and scripts/06_sensitivity_analysis.R
# to propagate exposure uncertainty into rank space and build the manuscript
# results table.
#
# Workflow:
#   priority_mc()      - Monte Carlo distribution of priority ranks, per
#                         weighting scheme, propagating exposure uncertainty
#   summarize_ranks()   - mean/min/max/quantile rank summary from a single
#                         priority_mc() output
#   clean_priority_vals() - assemble the manuscript results table (exposure,
#                         sensitivity, status, vulnerability score, rank)
#
#-------------------------------------------------------------------------


# MONTE CARLO RANK DISTRIBUTION -------------------------------------------

#' Monte Carlo distribution of priority ranks
#'
#' Propagates exposure uncertainty into rank space. Each iteration draws one
#' rescaled-exposure value per species from its full distribution, computes the
#' three-factor vulnerability (ESS), and ranks all species within each region.
#' Repeating this yields a distribution of ranks per species — capturing rank
#' uncertainty, which (unlike each species' marginal ESS distribution) is
#' relational and cannot be obtained analytically.
#'
#' Note: this resamples from the already-computed bootstrap-based exposure
#' distribution; it does not generate exposure.
#'
#' @param e Cleaned exposure (bootstrap-based), with a `scaled_overlap`
#'   list-column, from clean_exposure().
#' @param se Sensitivity table (alpha_code, sensitivity, common_name).
#' @param st Status table (alpha_code, status).
#' @param w Length-3 vector of weight exponents (exposure, sensitivity, status).
#' @param n_mc Number of Monte Carlo iterations. Default 1000.
#'
#' @returns A tibble with n_mc rows per species x region, each giving that
#'   iteration's pri_rank.
#'
priority_mc <- function(e, se, st, w = c(1, 1, 1), n_mc = 1000) {
  
  priority_once <- function(iter) {
    e %>%
      mutate(scaled_overlap = map_dbl(scaled_overlap, \(x) sample(x, 1))) %>%
      left_join(se, by = "alpha_code") %>%
      left_join(st, by = "alpha_code") %>%
      group_by(region) %>%
      mutate(
        ess      = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3],
        pri_rank = min_rank(desc(ess))
      ) %>%
      ungroup() %>%
      select(region, alpha_code, common_name, pri_rank)
  }
  
  map(seq_len(n_mc), priority_once) %>% list_rbind()
}


# RANK SUMMARY --------------------------------------------------------------

#' Summarize MC priority ranks (exposure uncertainty) for one weighting
#'
#' Reads the Monte Carlo rank distribution for a single weighting and returns
#' each species' mean, min, max, and quantile ranks across the MC iterations —
#' i.e. how much its priority rank varies due to exposure uncertainty.
#'
#' @param mc_ranks_path Path to a priority_mc() output (e.g. the 321 weighting,
#'   output/rank_mc/priority_ranks_321.rds).
#'
#' @returns A tibble: alpha_code, region, mean_rank, min_rank, q1_rank,
#'   q3_rank, l_rank95, u_rank95, max_rank.
#'
summarize_ranks <- function(mc_ranks_path =
                              here::here("output/rank_mc/priority_ranks_321.rds")) {
  readRDS(mc_ranks_path) %>%
    group_by(alpha_code, region) %>%
    summarize(
      mean_rank = mean(pri_rank),
      min_rank  = min(pri_rank),
      q1_rank   = quantile(pri_rank, 0.25, names = FALSE),
      q3_rank   = quantile(pri_rank, 0.75, names = FALSE),
      l_rank95  = quantile(pri_rank, 0.025, names = FALSE),
      u_rank95  = quantile(pri_rank, 0.975, names = FALSE),
      max_rank  = max(pri_rank),
      .groups = "drop"
    )
}


# RESULTS TABLE ------------------------------------------------------------

#' Clean priority values into a manuscript results table
#'
#' Builds a per-region results table: exposure (% overlap with 95% CI),
#' collision (CV) and displacement (DV) sensitivity, IUCN category, the ESS
#' vulnerability score with CI, the point-estimate priority rank, and a
#' rank interval across Monte Carlo iterations (exposure-uncertainty rank
#' band). Data-deficient species (in the sensitivity/status inputs but absent
#' from the regional scores) are appended with NA scores and ranks.
#'
#' @param raw_scores Priority table for the main (321) weighting, from
#'   calc_priority() (output/priority_values/priority_scores_321.rds).
#' @param exposure Cleaned exposure (cleaned_exposure.rds); supplies the
#'   `raw_overlap` distributions, which are dropped from the priority tables.
#' @param se Sensitivity table (sensitivity_sum.rds); supplies common_name, CV, DV.
#' @param st Status table (status.rds); supplies rl_category.
#' @param rank_summary MC rank summary for the 321 weighting, from
#'   summarize_ranks(): alpha_code, region, mean_rank, min_rank, q1_rank,
#'   q3_rank, l_rank95, u_rank95, max_rank.
#' @param selection Region to build the table for (e.g. "CA", "all").
#' @param rank_band Which rank interval to report alongside the point-estimate
#'   rank: "range" for the full MC min-max, "iqr" for the interquartile
#'   (q1-q3) range, or "95" for the central 95% (2.5th-97.5th percentile)
#'   range. Default "range".
#'
#' @returns A formatted tibble ready to write as a manuscript CSV.
#'
clean_priority_vals <- function(raw_scores, exposure, se, st, rank_summary,
                                selection, rank_band = c("range", "iqr", "95")){
  
  rank_band <- match.arg(rank_band)
  
  # regional scores; exposure summarized from the raw overlap draws.
  # lwr/upr are the 2.5%/97.5% quantiles of the raw (unrescaled) distribution.
  regional_scores <- raw_scores %>%
    filter(region == selection) %>%
    left_join(
      exposure %>% select(alpha_code, region, raw_overlap),
      by = c("alpha_code", "region")
    ) %>%
    mutate(
      mean_raw_overlap = map_dbl(raw_overlap, mean, na.rm = TRUE),
      lwr_raw_overlap  = map_dbl(raw_overlap, \(x) quantile(x, 0.025, na.rm = TRUE)),
      upr_raw_overlap  = map_dbl(raw_overlap, \(x) quantile(x, 0.975, na.rm = TRUE))
    ) %>%
    select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap,
           upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr) %>%
    arrange(desc(ess)) %>%
    mutate(pri_rank = row_number())    # point-estimate rank (by mean ESS)
  
  # data-deficient species: present in sensitivity/status, absent from scores
  dd_species <- se %>%
    left_join(st, by = "alpha_code") %>%
    anti_join(regional_scores, by = "alpha_code") %>%
    mutate(
      mean_raw_overlap = NA_real_, lwr_raw_overlap = NA_real_,
      upr_raw_overlap  = NA_real_, ess = NA_real_,
      ess_lwr = NA_real_, ess_upr = NA_real_, pri_rank = NA_integer_
    ) %>%
    select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap,
           upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr, pri_rank)
  
  # MC rank band (exposure uncertainty) for this region; quantiles are rounded
  # since ranks are integers
  ranks <- rank_summary %>%
    filter(region == selection) %>%
    mutate(
      band_lwr = switch(rank_band,
                        range = min_rank,
                        iqr   = round(q1_rank),
                        `95`  = round(l_rank95)),
      band_upr = switch(rank_band,
                        range = max_rank,
                        iqr   = round(q3_rank),
                        `95`  = round(u_rank95))
    ) %>%
    select(alpha_code, band_lwr, band_upr)
  
  # combine, join MC rank band, format for manuscript
  bind_rows(regional_scores, dd_species) %>%
    left_join(ranks, by = "alpha_code") %>%
    mutate(
      CV = round(CV, 3),
      DV = round(DV, 3),
      exp_ci = sprintf("%.3f (%.3f, %.3f)",
                       mean_raw_overlap * 100, lwr_raw_overlap * 100, upr_raw_overlap * 100),
      ess_ci = sprintf("%.3f (%.3f, %.3f)", ess, ess_lwr, ess_upr),
      rank_range = sprintf("%d (%d-%d)", pri_rank, band_lwr, band_upr)
    ) %>%
    select(common_name, exp_ci, CV, DV, rl_category, ess_ci, pri_rank, rank_range)
}