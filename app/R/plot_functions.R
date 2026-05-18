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





# PLOTS -------------------------------------------------------------------

make_boxplot_app <- function(priority_scores, app_data) {
  
  # Join taxonomy and index back in
  tax_lookup <- app_data %>%
    distinct(alpha_code, common_name, index, order, family)
  
  # Get the ess_dist data for species with sufficient data
  plot_df <- priority_scores %>%
    left_join(tax_lookup, by = "alpha_code") %>%
    unnest(ess_dist)
  
  all_species <- tax_lookup %>%
    filter(alpha_code %in% plot_df$alpha_code) %>%
    arrange(index) %>%
    mutate(x_pos = row_number())
  
  family_order <- all_species %>%
    arrange(index) %>%
    distinct(family) %>%
    pull(family)
  
  # Compute summary stats for tooltip
  tooltip_df <- priority_scores %>%
    left_join(tax_lookup, by = "alpha_code") %>%
    mutate(
      median_ess = map_dbl(ess_dist, median),
      lwr_ess = map_dbl(ess_dist, \(x) quantile(x, 0.025)),
      upr_ess = map_dbl(ess_dist, \(x) quantile(x, 0.975)),
      tooltip_text = sprintf(
        "%s<br>%s<br>%s<br>Median: %.3f (%.3f, %.3f)",
        common_name, order, family, median_ess, lwr_ess, upr_ess
      )
    )
  
  
  # Rectangle backgrounds by family
  species_order <- all_species %>%
    mutate(
      x_start = row_number() - 0.5,
      x_end = row_number() + 0.5
    )

  
  y_min <- min(plot_df$ess_dist, na.rm = TRUE)
  y_max <- max(plot_df$ess_dist, na.rm = TRUE)
  
  plot_df <- plot_df %>%
    mutate(family = factor(family, levels = family_order))
  
  rects <- species_order %>%
    filter(alpha_code %in% plot_df$alpha_code) %>%
    mutate(
      family = factor(family, levels = family_order),
      ymin = y_min, 
      ymax = y_max
    )
  
  # Join tooltip to plot_df
  plot_df <- plot_df %>%
    left_join(tooltip_df %>% select(alpha_code, tooltip_text), by = "alpha_code")
 
  p <- ggplot(
    plot_df,
    aes(
      x = reorder(alpha_code, index),
      y = ess_dist,
      fill = family
    )
  ) +
    # Family background rectangles
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
    # Boxplots
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
      show.legend = FALSE
    ) +
    scale_y_log10() +
    scale_fill_manual(
      breaks = family_order,
      values = setNames(
        c("#001959", "#0E395E", "#165061", "#27635F", "#47704F", 
          "#6C7B3B", "#97882C", "#C49138", "#EA995E", "#FBA894", 
          "#FCB9C6", "#F9CCF9"),
        c("Pelecanidae (Pelicans)",
          "Phalacrocoracidae (Cormorants and Shags)",
          "Procellariidae (Shearwaters and Petrels)",
          "Hydrobatidae (Northern Storm-Petrels)",
          "Diomedeidae (Albatrosses)",
          "Gaviidae (Loons)",
          "Podicipedidae (Grebes)",
          "Laridae (Gulls, Terns, and Skimmers)",
          "Alcidae (Auks, Murres, and Puffins)",
          "Stercorariidae (Skuas and Jaegers)",
          "Scolopacidae (Sandpipers and Allies)",
          "Anatidae (Ducks, Geese, and Waterfowl)")
      ),
      limits = family_order
    ) +
    guides(fill = guide_legend(ncol = 3)) +
    labs(
      x = "Species",
      y = "Priority Score",
      fill = "Family"
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
  
  p
}
