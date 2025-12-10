#this function multiplies exposure, sensitivity, and status with a user defined weight to make priority vals
calc_priority <- function(e, 
                          se, 
                          st, 
                          w = c(1, 1, 1)) {
  e %>% 
    unnest(scaled_overlap) %>% 
    left_join(se, by = "alpha_code") %>% 
    left_join(st, by = "alpha_code") %>% 
    mutate(
      e  = scaled_overlap^w[1],
      es = scaled_overlap^w[1] * sensitivity^w[2],
      ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3]
    ) %>% 
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
}



# Returns ranks, not direct priority values
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
      select(region, alpha_code, pri_rank)
  }
  result <- map(1:100, priority_once) %>% 
    list_rbind()
}
