##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ##################################
##########################################################################
# Sensitivity Functions Definitions #####################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/02_sensitivity.R to clean and combine sensitivity 
# values from the Kelsey et al 2025 pacific seabird vulnerability index to 
# offshore wind energy development
#
#-------------------------------------------------------------------------



# CLEAN AND COMBINE COLLISION & DISPLACEMENT VALUES ----------------------

#' Clean and combine collision and displacement sensitivity values
#'
#' Joins the collision (CV) and displacement (DV) vulnerability scores from
#' Kelsey et al. (2025) onto the project's main species list, using the
#' main list as the authoritative taxonomy. A few alpha codes in the
#' displacement file are corrected to match local taxonomy before joining.
#'
#' @param sp main species list (total_sp_list.csv).
#' @param cv Collision vulnerability table (POCS_VulnIndex_CV.csv), supplying
#'   CV_new per AlphaCode.
#' @param dv Displacement vulnerability table (POCS_VulnIndex_DV.csv),
#'   supplying DV_new per AlphaCode.
#'
#' @returns A tibble of alpha_code, common_name, and scientific_name (from the
#'   master taxonomy) with the current CV and DV values from Kelsey et al.
#'   (2025) joined on.

clean_sens <- function(sp, cv, dv) {
  
  # Reconcile displacement-file alpha codes to local taxonomy
  dv <- dv %>% 
    mutate(AlphaCode = case_when(
      AlphaCode == "ROTE" ~ "ROYT",
      AlphaCode == "WIPE" ~ "AWPE",
      TRUE ~ AlphaCode
    ))
  
  # Master list drives the taxonomy; CV and DV are joined on
  clean_sens <- sp %>% 
    drop_na(alpha_code) %>% 
    select(alpha_code, common_name, scientific_name) %>%
    left_join(select(dv, AlphaCode, DV = DV_new), 
              by = c("alpha_code" = "AlphaCode")) %>% 
    left_join(select(cv, AlphaCode, CV = CV_new), 
              by = c("alpha_code" = "AlphaCode"))
  
  return(clean_sens)
}


# RESCALE SENSITIVITY VALUES ---------------------------------------------

#' Rescale sensitivity values to the [0.5, 2.0] range
#'
#' Restricts sensitivity scores to regionally-relevant species and rescales
#' the selected metric to the common [0.5, 2.0] geometric range used across
#' the framework. Collision (CV) and displacement (DV) can be used alone, or
#' combined by first rescaling each to [0, 1] and then either summing them or
#' taking the higher of the two, so neither metric dominates on raw scale.
#'
#' @param sp Master species list, used to restrict to regional species
#'   (regional == "Y").
#' @param sens Cleaned sensitivity table from clean_sens().
#' @param sel Which sensitivity metric to rescale: "CV", "DV", their summed
#'   combination ("sum"), or the higher of the two ("highest").
#'
#' @returns The regional subset of `sens` with an added `sensitivity` column
#'   holding the selected metric rescaled to [0.5, 2.0].
#'
rescale_sens <- function(sp, 
                         sens,
                         sel = c("CV", "DV", "sum", "highest")) {
  sel <- match.arg(sel)
  
  # Regional species only
  regional_species <- sp %>% 
    drop_na(alpha_code) %>% 
    filter(regional == "Y") %>% 
    pull(alpha_code)  
  
  sens <- sens %>% 
    filter(alpha_code %in% regional_species) 
  
  # CV/DV alone go straight to the log rescale; sum/highest first put both
  # metrics on a common [0, 1] scale so they combine comparably.
  sens$sensitivity <- switch(
    sel, 
    CV  = log_rescale(sens$CV),
    DV  = log_rescale(sens$DV),
    sum = log_rescale(rescale_01(sens$CV) + rescale_01(sens$DV)),
    highest = {
      CV_01 <- rescale_01(sens$CV) 
      DV_01 <- rescale_01(sens$DV)
      log_rescale(pmax(CV_01, DV_01))
    }
  )
  return(sens)
}


# RESCALING SUBFUNCTIONS -------------------------------------------------

#' Rescale a vector to the [0.5, 2.0] geometric range
#'
#' Linear rescale in log space, so the minimum maps to 0.5, the maximum to
#' 2.0, and the midpoint to 1.0. This is the same geometric rescaling applied
#' to exposure values, giving sensitivity and exposure a common footing when
#' combined into vulnerability.
#'
#' @param x A numeric vector (CV, DV, or a combined metric).
#'
#' @returns `x` rescaled so min(x) -> 0.5 and max(x) -> 2.0.
#'
log_rescale <- function(x){
  log_y_rng <- log(c(0.5, 2.0))
  log_y <- log_y_rng[1] + (log_y_rng[2] - log_y_rng[1]) * (x - min(x)) / (max(x) - min(x))
  y <- exp(log_y)
  return(y)
}

#' Rescale a vector to [0, 1]
#'
#' @param x A numeric vector.
#' @returns `x` linearly rescaled so min(x) -> 0 and max(x) -> 1.
#'
rescale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

