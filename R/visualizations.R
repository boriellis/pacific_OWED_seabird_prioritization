





# results table -----------------------------------------------------------



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

  



# taxonomy plot -----------------------------------------------------------

make_boxplot <- function(sp_list, tax, priority_dists, selection){
  tax <- tax %>% 
    rename(scientific_name = `scientific name`) 
  
  sp_list_ordered <- sp_list %>% 
    left_join(tax, by = "scientific_name") %>% 
    select(index = 'sort v2024',
           alpha_code,
           order,
           family = taxonomy
    )
  
  output_w_taxonomy <- priority_dists %>% 
    left_join(sp_list_ordered, by = "alpha_code")
  
  
  #now plot:
  plot_df <- output_w_taxonomy %>%   # <-- whatever your output object is called
    filter(region == selection) %>%
    unnest(ess_dist)   # expands each 1000-value vector into long format
  
  plot_df <- plot_df %>%
    arrange(-index) %>%     # same ordering used for x
    mutate(
      family = factor(family, levels = unique(family))
    )
  
  # Step 1: Get species ordering and rectangle positions
  species_order <- plot_df %>%
    arrange(-index) %>%
    distinct(alpha_code, family) %>%
    mutate(
      x_start = row_number() - 0.5,
      x_end = row_number() + 0.5
    )
  
  # Step 2: Define vertical range for rectangles (adjust as needed)
  y_min <- min(plot_df$ess_dist, na.rm = TRUE)
  y_max <- max(plot_df$ess_dist, na.rm = TRUE)
  
  rects <- species_order %>%
    mutate(
      ymin = y_min,
      ymax = y_max
    )
  
  #step 3 - plot
  
  ggplot(
    plot_df, 
    aes(
      x = reorder(alpha_code, -index),
      y = ess_dist,
      fill = family       # fill mapped globally so boxplots get family colors
    )
  ) +
    # Rectangles with family fill, legend shown
    geom_rect(
      data = rects,
      aes(
        xmin = x_start,
        xmax = x_end,
        ymin = ymin,
        ymax = ymax,
        fill = family
      ),
      inherit.aes = FALSE,
      alpha = 0.4,
      color = NA,
      show.legend = TRUE
    ) +
    # Boxplots with fill by family, but exclude from legend
    stat_summary(
      fun.data = function(x) {
        data.frame(
          ymin   = as.numeric(quantile(x, 0.025)),
          lower  = as.numeric(quantile(x, 0.25)),
          middle = as.numeric(quantile(x, 0.5)),
          upper  = as.numeric(quantile(x, 0.75)),
          ymax   = as.numeric(quantile(x, 0.975))
        )
      },
      geom = "boxplot",
      outlier.shape = NA,
      show.legend = FALSE    # hide boxplots from legend
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    scale_y_log10() +
    scale_fill_manual(values = c(
      "Pelecanidae (Pelicans)" = "#001959",
      "Phalacrocoracidae (Cormorants and Shags)" = "#0E395E",
      "Procellariidae (Shearwaters and Petrels)" = "#165061",
      "Hydrobatidae (Northern Storm-Petrels)" = "#27635F",
      "Diomedeidae (Albatrosses)" = "#47704F",
      "Gaviidae (Loons)" = "#6C7B3B",
      "Podicipedidae (Grebes)" = "#97882C",
      "Laridae (Gulls, Terns, and Skimmers)" = "#C49138",
      "Alcidae (Auks, Murres, and Puffins)" = "#EA995E",
      "Stercorariidae (Skuas and Jaegers)" = "#FBA894",
      "Scolopacidae (Sandpipers and Allies)" = "#FCB9C6",
      "Anatidae (Ducks, Geese, and Waterfowl)" = "#F9CCF9"
    )) +
    labs(
      x = "Species (ordered by index)",
      y = "ESS distribution",
      fill = "Family"
    )
}





# make rank MCs -----------------------------------------------------------




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




