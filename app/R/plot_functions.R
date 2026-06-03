#' Rescale to 0.5-2.0
#'
#' @param x is the vector of sensitivity numbers (either CV, DV, sum, or highest)
#'
#' @returns a rescaled vector where the lowest val in the range is 0.5 and the highest is 2.0

log_rescale <- function(x){
  log_y_rng <- log(c(0.01, 100))
  log_y <- log_y_rng[1] + (log_y_rng[2] - log_y_rng[1]) * (x - min(x)) / (max(x) - min(x))
  y <- exp(log_y)
  return(y)
}

#rescale a vector from 0-1
rescale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#' Subfunction to rescale exposure from 0.5-2
#'
#' @param overlap_list is, I think, the list of vectors of each species/region overlap values
#'
#' @returns those values rescaled from 0.5 min to 2.0 max 
#' @export
#'
#' @examples
rescale_overlap <- function(overlap_list) {
  all_overlaps <- unlist(overlap_list)
  min_overlap <- min(all_overlaps)
  max_overlap <- max(all_overlaps)
  log_rescale <- function(x) {
    log_y_rng <- log(c(0.01, 100))
    log_y <- log_y_rng[1] + 
      (log_y_rng[2] - log_y_rng[1]) * 
      (x - min_overlap) / (max_overlap - min_overlap)
    y <- exp(log_y)
    return(y)
  }
  map(overlap_list, log_rescale)
}



clean_statuses <- function(sp, iucn){
  sp_iucn_sciname <- tibble(
    sp_sciname = c("Phalaropus tricolor", "Chroicocephalus philadelphia", "Stercorarius maccormicki", "Larus brachyrhynchus", "Sula brewsteri"), 
    iucn_sciname = c("Steganopus tricolor", "Larus philadelphia", "Catharacta maccormicki", "Larus delawarensis", "Sula leucogaster")
  )
  #sp retains some rows for unused group model names
  sp_clean <- drop_na(sp, alpha_code) %>% 
    left_join(sp_iucn_sciname, by = c(scientific_name = "sp_sciname")) %>% 
    mutate(iucn_sciname = coalesce(iucn_sciname, scientific_name))
  iucn_clean <- select(iucn, 
                       rl_category = `RL Category`,
                       iucn_sciname = `Scientific name`)
  #set status values to RL categories from 0.5-2 with a fixed multiplier of 4^(1/4) between each 
  rl_status <- 10^(-2:2)
  names(rl_status) <- c("LC", "NT", "VU", "EN", "CR")
  result <- sp_clean %>% 
    left_join(iucn_clean, by = "iucn_sciname") %>% 
    mutate(status = rl_status[rl_category]) %>% 
    select(alpha_code, rl_category, status)
  return(result)
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


# Get top N species by mean ess, with a priority-gradient color assigned by rank
get_top10 <- function(priority_scores, app_data, n = 10) {
  tax_lookup <- app_data %>%
    distinct(alpha_code, common_name)
  
  # Gradient: darkest = highest priority (rank 1), lightest = rank n
  pal <- colorRampPalette(c(
    "#662506", "#993404", "#CC4C02", "#EC7014",
    "#FE9929", "#FEC44F", "#FEE391", "#FFF7BC"
  ))
  
  priority_scores %>%
    left_join(tax_lookup, by = "alpha_code") %>%
    arrange(desc(ess)) %>%
    slice(1:n) %>%
    mutate(
      priority_rank = row_number(),
      color = pal(n)
    ) %>%
    select(alpha_code, common_name, ess, priority_rank, color)
}

make_ess_plot <- function(priority_scores, top10) {
  
  # Long format: one row per species per Priority stage, with mean/lwr/upr
  foo_long <- priority_scores %>%
    filter(alpha_code %in% top10$alpha_code) %>%
    select(alpha_code, e, e_lwr, e_upr, es, es_lwr, es_upr, ess, ess_lwr, ess_upr) %>%
    mutate(across(c(e_lwr, e_upr, es_lwr, es_upr, ess_lwr, ess_upr), as.numeric)) %>%
    rename(e_mean = e, es_mean = es, ess_mean = ess) %>%
    pivot_longer(
      -alpha_code,
      names_to = c("Priority", ".value"),
      names_sep = "_"
    ) %>%
    mutate(Priority = factor(Priority, levels = c("e", "es", "ess"))) %>%
    left_join(top10 %>% select(alpha_code, common_name, color), by = "alpha_code")
  
  # Color lookup ordered by priority rank (for legend ordering)
  legend_info <- top10 %>% arrange(priority_rank)
  color_lookup <- setNames(legend_info$color, legend_info$common_name)
  
  ggplot(foo_long, aes(x = Priority, y = mean, group = common_name)) +
    geom_ribbon(
      aes(ymin = lwr, ymax = upr, fill = common_name),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_line(
      aes(color = common_name),
      linewidth = 1.2
    ) +
    scale_fill_manual(values = color_lookup) +
    scale_color_manual(
      values = color_lookup,
      breaks = legend_info$common_name,
      guide = guide_legend(order = 1)
    ) +
    scale_x_discrete(
      labels = c("e" = "E", "es" = "E*Se", "ess" = "E*Se*St"),
      expand = c(0, 0)
    ) +
    theme_bw() +
    labs(x = NULL, y = "Score", color = "Species") +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(face = "bold", margin = margin(r = 15))
    )
}

