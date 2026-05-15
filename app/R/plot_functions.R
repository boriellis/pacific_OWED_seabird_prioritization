#' Rescale to 0.5-2.0
#'
#' @param x is the vector of sensitivity numbers (either CV, DV, sum, or highest)
#'
#' @returns a rescaled vector where the lowest val in the range is 0.5 and the highest is 2.0

log_rescale <- function(x){
  log_y_rng <- log(c(0.5, 2.0))
  log_y <- log_y_rng[1] + (log_y_rng[2] - log_y_rng[1]) * (x - min(x)) / (max(x) - min(x))
  y <- exp(log_y)
  return(y)
}

#rescale a vector from 0-1
rescale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}



calc_priority <- function(e, 
                          se, 
                          st, 
                          w = c(1, 1, 1)) {
  
  # Step 1: Compute all per-draw quantities
  df <- e %>% 
    unnest(scaled_overlap) %>% 
    left_join(se, by = "alpha_code") %>% 
    left_join(st, by = "alpha_code") %>% 
    mutate(
      e  = scaled_overlap^w[1],
      es = scaled_overlap^w[1] * sensitivity^w[2],
      ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3]
    )
  
  # Step 2: Capture full ESS distribution for each species
  ess_nested <- df %>%
    group_by(alpha_code, region) %>%
    summarize(
      ess_dist = list(unname(ess)),   # <-- contains all 1000 values
      .groups = "drop"
    )
  
  # Step 3: Summarize statistics (your original output)
  stats <- df %>%
    group_by(alpha_code, region) %>% 
    summarize(
      across(
        e:ess,
        list(
          mean = mean,
          lwr = \(x) quantile(x, 0.025),
          upr = \(x) quantile(x, 0.975)
        )
      ),
      .groups = "drop"
    ) %>% 
    rename_with(\(x) str_replace(x, "_mean", ""), .cols = ends_with("_mean"))
  
  # Step 4: Join stats with the new nested column
  left_join(stats, ess_nested, by = c("alpha_code", "region"))
}

priority_mc_200 <- function(e, se, st, w = c(1, 1, 1)) {
  # This samples exposures ONCE per iteration, 200 times instead of 1000
  priority_once <- function(x) {
    e %>% 
      mutate(scaled_overlap = map_dbl(scaled_overlap, \(x) sample(x, 1))) %>% 
      left_join(se, by = "alpha_code") %>% 
      left_join(st, by = "alpha_code") %>% 
      group_by(region) %>% 
      mutate(e = scaled_overlap^w[1],
             es = scaled_overlap^w[1] * sensitivity^w[2],
             ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3],
             pri_rank = min_rank(desc(ess))) %>% 
      ungroup() %>% 
      select(region, alpha_code, common_name, pri_rank)
  }
  result <- map(1:200, priority_once) %>% 
    list_rbind()
}

