



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




# saveRDS(result, "paper/112priority_ranks_1000_for_plots.rds")
