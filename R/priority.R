##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ##################################
##########################################################################
# Priority Functions Definitions #####################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/04_multiply.R
#
#-------------------------------------------------------------------------





# COMBINE FACTORS INTO PRIORITY (VULNERABILITY) --------------------------

#' Combine exposure, sensitivity, and status into priority scores
#'
#' Multiplies the three vulnerability factors together, per draw, to produce
# 'vulnerability scores. Exposure enters as a full distribution
#' (one value per bootstrap draw, carried in a list-column), while sensitivity
#' and status enter as single per-species values; the exposure distribution
#' therefore propagates through to the final scores, while sensitivity and
#' status act as fixed multipliers. Each factor is raised to a user-supplied
#' weight exponent before multiplying, so weights adjust each factor's
#' leverage on the [0.5, 2.0] scale (a weight of 1 leaves a factor unchanged).
#'
#' Three cumulative quantities are returned: e (exposure only),
#' es (exposure x sensitivity), and ess (exposure x sensitivity x status,
#' i.e. full three-factor vulnerability).
#'
#' @param e Exposure, from clean_exposure(): one row per species x region with
#'   a `scaled_overlap` list-column of rescaled proportional-overlap values
#'   (one per retained bootstrap draw; counts vary by species after outlier
#'   removal, and elicited species pool draws across experts).
#' @param se Sensitivity, from rescale_sens(): alpha_code plus a single
#'   rescaled `sensitivity` value per species.
#' @param st Status, from clean_statuses(): alpha_code plus a single `status`
#'   weight per species.
#' @param w Length-3 vector of weight exponents applied to exposure,
#'   sensitivity, and status respectively. Defaults to c(1, 1, 1) (no
#'   reweighting).
#'
#' @returns A tibble with one row per species x region, giving the mean and
#'   95% interval bounds (lwr = 2.5%, upr = 97.5%) for e, es, and ess, plus an
#'   `ess_dist` list-column holding the full ESS distribution for each species
#'   x region (used downstream for resampling and boxplots).
#'
calc_priority <- function(e, 
                          se, 
                          st, 
                          w = c(1, 1, 1)) {
  
  # Expand the exposure distribution to one row per draw, attach the
  # (scalar) sensitivity and status values, and compute the cumulative
  # weighted products. Each factor is raised to its weight exponent.
  df <- e %>% 
    unnest(scaled_overlap) %>% 
    left_join(se, by = "alpha_code") %>% 
    left_join(st, by = "alpha_code") %>% 
    mutate(
      e   = scaled_overlap^w[1],
      es  = scaled_overlap^w[1] * sensitivity^w[2],
      ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3]
    )
  
  # Keep the full ESS distribution per species x region (variable length:
  # one value per exposure draw, so more for elicited species than modeled).
  ess_nested <- df %>%
    group_by(alpha_code, region) %>%
    summarize(ess_dist = list(unname(ess)), .groups = "drop")
  
  # Point estimate (mean) and 95% bounds for each cumulative quantity
  stats <- df %>%
    group_by(alpha_code, region) %>% 
    summarize(
      across(
        c(e, es, ess),
        list(
          mean = mean,
          median = median,
          lwr  = \(x) quantile(x, 0.025, names = FALSE),
          upr  = \(x) quantile(x, 0.975, names = FALSE)
        )
      ),
      .groups = "drop"
    ) %>% 
    rename_with(\(x) str_replace(x, "_mean", ""), .cols = ends_with("_mean"))
  
  # Join summary stats to the full-distribution column
  left_join(stats, ess_nested, by = c("alpha_code", "region"))
}
