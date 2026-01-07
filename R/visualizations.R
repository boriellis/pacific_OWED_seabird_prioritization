






#' Clean priority values for results table
#'
#' @param raw_scores "output/priority_scores_1000.rds"
#' @param se "output/sensitivity_sum.rds"
#' @param st "output/status.rds"
#' @param selection is the region you want (e.g., "CA")
#'
#' @returns a formatted table to save as a csv that can be easily added to the manuscript
#' @export
#'
#' @examples
clean_priority_vals <- function(raw_scores, se, st, selection){
  #set up results df for selected region
  regional_scores <- raw_scores %>% 
    filter(region == selection) %>% 
    mutate(mean_raw_overlap = map_dbl(outliers_rm, mean),
           lwr_raw_overlap = map_dbl(outliers_rm, min), #this is technically the 2.5% quantile since I already capped values below that to that value at the winzorization step
           upr_raw_overlap = map_dbl(outliers_rm, max) #97.5% quantile of the raw. 
    ) %>%
    select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap, upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr) %>% 
    arrange(desc(ess)) %>%         
    mutate(pri_rank = row_number())
  #make a df of data deficient species
  dd_species <- se %>% 
    left_join(st, by = "alpha_code") %>% 
    anti_join(regional_scores, by = "alpha_code") %>% 
    mutate(mean_raw_overlap = NA_real_,
           lwr_raw_overlap = NA_real_,
           upr_raw_overlap = NA_real_,
           ess = NA_real_,
           ess_lwr = NA_real_,
           ess_upr = NA_real_
    ) %>% 
    select(alpha_code, common_name, mean_raw_overlap, lwr_raw_overlap, upr_raw_overlap, CV, DV, rl_category, ess, ess_lwr, ess_upr)
 
  #add the two together
  total_region_table <- bind_rows(regional_scores, dd_species)
  
  #format for output to manuscript table
  formatted_results_table <- total_region_table %>% 
    mutate(mean_exp_percent = mean_raw_overlap*100,
           lwr_exp_percent = lwr_raw_overlap*100,
           upr_exp_percent = upr_raw_overlap *100,
           CV = round(CV, 3),
           DV = round(DV, 3),
           exp_ci = sprintf("%.3f (%.3f, %.3f)", mean_exp_percent, lwr_exp_percent, upr_exp_percent),
           ess_ci = sprintf("%.3f (%.3f, %.3f)", ess, ess_lwr, ess_upr)
    ) %>% 
    select(common_name, exp_ci, CV, DV, rl_category, ess_ci, pri_rank)
}

  



#' Returns 1000 ranks, not priority values
#'
#' @param e "output/cleaned_exposure_1000sims.rds" 
#' @param se "output/sensitivity_sum.rds"
#' @param st "output/status.rds"
#' @param w vector of exponential weights - default is 1,1,1
#'
#' @returns
#' @export
#'
#' @examples
priority_mc <- function(e, 
                        se, 
                        st, 
                        w = c(1, 1, 1)) {
  # This samples exposures ONCE
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
  result <- map(1:1000, priority_once) %>% 
    list_rbind()
}

