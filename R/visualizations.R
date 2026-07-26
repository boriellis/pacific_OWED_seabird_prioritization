
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
#' distribution; it does not generate exposure. It is distinct from the
#' per-pixel Monte Carlo used in earlier exposure drafts.
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

#' Summarize MC priority ranks (exposure uncertainty) for one weighting
#'
#' Reads the Monte Carlo rank distribution for a single weighting and returns
#' each species' mean, min, and max rank across the MC iterations — i.e. how
#' much its priority rank varies due to exposure uncertainty.
#'
#' @param mc_ranks_path Path to a priority_mc() output (e.g. the 321 weighting,
#'   output/rank_mc/priority_ranks_321.rds).
#'
#' @returns A tibble: alpha_code, region, mean_rank, min_rank, max_rank.
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
      l_rank95   = quantile(pri_rank, 0.025, names = FALSE),
      u_rank95   = quantile(pri_rank, 0.975, names = FALSE),
      max_rank  = max(pri_rank),
      .groups = "drop"
    )
}



# RESULTS TABLE ----------------------------------------------------------

#' Clean priority values into a manuscript results table
#'
#' Builds a per-region results table: exposure (% overlap with 95% CI),
#' collision (CV) and displacement (DV) sensitivity, IUCN category, the ESS
#' vulnerability score with CI, the point-estimate priority rank, and the
#' min/max rank across Monte Carlo iterations (exposure-uncertainty rank band).
#' Data-deficient species (in the sensitivity/status inputs but absent from the
#' regional scores) are appended with NA scores and ranks.
#'
#' @param raw_scores Priority table for the main (321) weighting, from
#'   calc_priority() (output/priority_values/priority_scores_321.rds).
#' @param se Sensitivity table (sensitivity_sum.rds); supplies common_name, CV, DV.
#' @param st Status table (status.rds); supplies rl_category.
#' @param rank_summary MC rank summary for the 321 weighting, from
#'   summarize_ranks(): alpha_code, region, mean_rank, min_rank, max_rank.
#' @param selection Region to build the table for (e.g. "CA", "all").
#'
#'
#' @param rank_band Which rank interval to report alongside the point-estimate
#'   rank: "range" for the full MC min–max, or "iqr" for the interquartile
#'   (q1–q3) range. Default "range".
#' @returns A formatted tibble ready to write as a manuscript CSV.
#'
clean_priority_vals <- function(raw_scores, exposure, se, st, rank_summary,
                                selection, rank_band = c("range", "iqr", "95")){
  
  rank_band <- match.arg(rank_band)
  
  # regional scores; exposure summarized from the raw overlap draws.
  # lwr/upr are the 2.5%/97.5% quantiles of the (unwinsorized) distribution.
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

  


# PRIORITY BOXPLOT -------------------------------------------------------

# Family colours for the taxonomic background bands (scico "lipari"-style ramp,
# ordered from most to least pelagic).
FAMILY_COLOURS <- c(
  "Pelecanidae (Pelicans)"                    = "#001959",
  "Phalacrocoracidae (Cormorants and Shags)"  = "#0E395E",
  "Procellariidae (Shearwaters and Petrels)"  = "#165061",
  "Hydrobatidae (Northern Storm-Petrels)"     = "#27635F",
  "Diomedeidae (Albatrosses)"                 = "#47704F",
  "Gaviidae (Loons)"                          = "#6C7B3B",
  "Podicipedidae (Grebes)"                    = "#97882C",
  "Laridae (Gulls, Terns, and Skimmers)"      = "#C49138",
  "Alcidae (Auks, Murres, and Puffins)"       = "#EA995E",
  "Stercorariidae (Skuas and Jaegers)"        = "#FBA894",
  "Scolopacidae (Sandpipers and Allies)"      = "#FCB9C6",
  "Anatidae (Ducks, Geese, and Waterfowl)"    = "#F9CCF9"
)

#' Boxplot of priority score distributions by species
#'
#' Plots each species' full ESS (vulnerability) distribution as a boxplot,
#' ordered taxonomically, with background rectangles shading each species'
#' family. Boxes show the 2.5%, 25%, 50%, 75%, and 97.5% quantiles of the
#' bootstrap-derived priority distribution.
#'
#' @param sp_list Master species list (alpha_code, scientific_name).
#' @param tax Taxonomy table supplying taxonomic sort order, order, and family.
#' @param priority_dists Priority table from calc_priority(), carrying the
#'   `ess_dist` list-column of per-draw vulnerability scores.
#' @param selection Region to plot (e.g. "CA", "all").
#'
#' @returns A ggplot object.
#'
make_boxplot <- function(sp_list, tax, priority_dists, selection){
  
  tax <- tax %>% rename(scientific_name = `scientific name`)
  
  sp_list_ordered <- sp_list %>%
    left_join(tax, by = "scientific_name") %>%
    select(index = `sort v2024`, alpha_code, order, family = taxonomy)
  
  # attach taxonomy, subset to region, expand each species' priority
  # distribution to long format (length varies by species after outlier removal)
  plot_df <- priority_dists %>%
    left_join(sp_list_ordered, by = "alpha_code") %>%
    filter(region == selection) %>%
    unnest(ess_dist)
  
  # Species axis order: taxonomic index, ascending. Derived once here so the
  # background rectangles and the boxplots cannot drift out of alignment.
  species_order <- plot_df %>%
    distinct(common_name, family, index) %>%
    arrange(index) %>%
    mutate(x_pos = row_number())
  
  stopifnot(
    "taxonomy join left species without a sort index" = !any(is.na(species_order$index))
  )
  
  plot_df <- plot_df %>%
    mutate(
      common_name = factor(common_name, levels = species_order$common_name),
      family      = factor(family, levels = rev(unique(species_order$family)))
    )
  
  # Background rectangles: one per species, spanning the full y range
  rects <- species_order %>%
    mutate(
      family = factor(family, levels = levels(plot_df$family)),
      xmin = x_pos - 0.5,
      xmax = x_pos + 0.5,
      ymin = min(plot_df$ess_dist, na.rm = TRUE),
      ymax = max(plot_df$ess_dist, na.rm = TRUE)
    )
  
  ggplot(plot_df, aes(x = common_name, y = ess_dist, fill = family)) +
    # family background bands
    geom_rect(
      data = rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = family),
      inherit.aes = FALSE, alpha = 0.4, color = NA, show.legend = TRUE
    ) +
    # boxplots: 2.5 / 25 / 50 / 75 / 97.5 quantiles of the priority distribution
    stat_summary(
      fun.data = \(x) data.frame(
        ymin   = quantile(x, 0.025, names = FALSE),
        lower  = quantile(x, 0.25,  names = FALSE),
        middle = quantile(x, 0.50,  names = FALSE),
        upper  = quantile(x, 0.75,  names = FALSE),
        ymax   = quantile(x, 0.975, names = FALSE)
      ),
      geom = "boxplot", width = 0.8, linewidth = 0.15, show.legend = FALSE
    ) +
    scale_y_log10() +
    scale_fill_manual(values = FAMILY_COLOURS, limits = rev) +
    guides(fill = guide_legend(ncol = 3)) +
    labs(x = "Species", y = "Vulnerability Score", fill = "Family") +
    theme_classic() +
    theme(
      axis.title       = element_text(size = 8),
      axis.text        = element_text(size = 7),
      axis.title.x     = element_text(face = "bold", margin = margin(t = 5)),
      axis.title.y     = element_text(face = "bold"),
      axis.text.x      = element_text(angle = 75, hjust = 1),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.key.width = unit(0.5, "cm"),
      legend.key.height= unit(0.3, "cm"),
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 7)
    )
}







# make ridge plot -----------------------------------------------------------

#' Ridge plot of priority rank distributions
#'
#' Shows the rank distribution of the top-ranked species, capturing rank
#' uncertainty propagated from exposure. Species are selected as the top N by
#' mean priority score (ESS), and ordered with the best-ranked at the top.
#'
#' @param dataset MC rank output from priority_mc() (alpha_code, region,
#'   common_name, pri_rank).
#' @param scores Priority table from calc_priority() (alpha_code, region, ess),
#'   used to select the top N species by mean priority score.
#' @param spref Ordered species vector matching colref.
#' @param colref Colour vector.
#' @param selection Region to plot (e.g. "CA").
#' @param top_n Number of species to show, by mean priority score. Default 10.
#' @param x Optional x-axis upper limit; defaults to the number of ranked species.
#'
ridgeplot <- function(dataset, scores, spref, colref, selection, top_n = 57, x = NULL){
  
  priority_colors <- tibble(common_name = spref, color = colref)
  
  # species -> colour lookup; unmatched species fall back to green
  all_species_colors <- dataset %>%
    distinct(common_name) %>%
    left_join(priority_colors, by = "common_name") %>%
    mutate(color = if_else(is.na(color), "#008000", color))
  
  regional <- dataset %>%
    filter(region == selection) %>%
    left_join(all_species_colors, by = "common_name")
  
  n_species <- n_distinct(regional$common_name)
  if (is.null(x)) x <- n_species
  
  # top N species by mean priority score (ESS) in this region
  keep <- scores %>%
    filter(region == selection, !is.na(ess)) %>%
    slice_max(ess, n = top_n) %>%
    select(alpha_code, ess)
  
  foo <- regional %>%
    inner_join(keep, by = "alpha_code") %>%
    mutate(common_name = fct_reorder(common_name, ess))
  
  ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
    ggridges::geom_density_ridges(
      stat = "binline", binwidth = 1, scale = 4,
      alpha = 0.8, linewidth = 0.05, color = "grey10"
    ) +
    scale_fill_identity() +
    coord_cartesian(xlim = c(0, x)) +
    theme_classic() +
    labs(x = str_glue("Priority Rank (out of {n_species})"), y = NULL) +
    theme(
      legend.position = "none",
      axis.title      = element_text(size = 8),
      axis.text       = element_text(size = 7),
      axis.title.x    = element_text(face = "bold", margin = margin(t = 8))
    )
}



#' Stacked histogram of top-N rank occupancy
#'
#' Shows which species occupy each of the top N priority ranks across Monte
#' Carlo iterations. Species listed in `sp_colors` are shown individually in
#' their assigned colour; all others are pooled into a single grey "Other"
#' segment. Legend is ordered by mean rank (across each species' full rank
#' distribution), with "Other" last.
#'
#' @param dataset MC rank output from priority_mc() (alpha_code, region,
#'   common_name, pri_rank).
#' @param sp_colors Tibble of common_name and color for the species to show
#'   individually. Any species not listed is pooled into "Other".
#' @param selection Region to plot (e.g. "CA").
#' @param top_n Number of rank positions to show. Default 5.
#' @param other_color Fill for the pooled "Other" segment.
#'
stackedhist <- function(dataset, sp_colors, selection, top_n = 5,
                        other_color = "#BDBDBD"){
  
  named <- sp_colors %>% select(common_name, color)
  
  regional <- dataset %>%
    filter(region == selection) %>%
    left_join(named, by = "common_name") %>%
    mutate(
      plot_group = if_else(is.na(color), "Other", common_name),
      color      = if_else(is.na(color), other_color, color)
    )
  
  # legend order: named species by mean rank (full distribution), Other last
  legend_info <- regional %>%
    filter(plot_group != "Other") %>%
    group_by(plot_group, color) %>%
    summarize(mean_rank = mean(pri_rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_rank) %>%
    mutate(label = str_glue("{plot_group}\n(mean rank = {round(mean_rank, 1)})")) %>%
    bind_rows(tibble(plot_group = "Other", color = other_color,
                     mean_rank = Inf, label = "Other"))
  
  # collapse to counts per rank x group so Other is a single segment
  plot_df <- regional %>%
    filter(pri_rank <= top_n) %>%
    count(pri_rank, plot_group, color) %>%
    mutate(color = factor(color, levels = rev(legend_info$color)))
  
  ggplot(plot_df, aes(x = pri_rank, y = n, fill = color)) +
    geom_col(position = "stack") +
    scale_x_continuous(breaks = 1:top_n) +
    scale_fill_identity(
      guide  = "legend",
      breaks = legend_info$color,
      labels = legend_info$label
    ) +
    guides(fill = guide_legend(ncol = 2, override.aes = list(size = 3))) +
    labs(x = "Rank", y = "Frequency", fill = "Species") +
    theme_classic() +
    theme(
      legend.position = "right",
      legend.key.size = unit(1, "cm"),
      legend.title    = element_text(size = 10),
      legend.text     = element_text(size = 10),
      axis.title      = element_text(size = 14),
      axis.text       = element_text(size = 12),
      axis.title.x    = element_text(face = "bold", margin = margin(t = 15)),
      axis.title.y    = element_text(face = "bold", margin = margin(r = 15))
    )
}



#' make main stacked histogram to wrap
#'
#' @param dataset is the particular weighted simulation file you want  (321 in this case)
#' @param spref is the ordered species vector to match the colors
#' @param colref is the color vector
#' @param selection region (e.g., "CA")
#'
#' @returns
#' @export
#'
#' @examples
stackedhist3 <- function(dataset, spref, colref, selection){
  # Create a tibble of priority species and colors
  all_species <- unique(dataset$common_name)
  all_codes <- unique(dataset$alpha_code)
  priority_colors <- tibble(common_name = spref, color = colref)
  all_species_colors <- tibble(common_name = all_species,
                               alpha_code = all_codes) %>%
    left_join(priority_colors, by = "common_name") %>%
    mutate(color = if_else(is.na(color), "#008000", color))
  result_colored <- dataset %>%
    left_join(all_species_colors, by = "common_name")
  
  #join w/ main
  foo <- result_colored %>% 
    filter(region == selection) %>% 
    mutate(common_name = fct_reorder(common_name, pri_rank, .desc = TRUE))
  foo_keep <- foo %>%
    group_by(common_name) %>%
    summarize(keep = any(pri_rank <= 10)) %>%
    filter(keep)
  
  foo <- foo %>%
    semi_join(foo_keep, by = "common_name") %>%
    mutate(
      # Order so top of plot is level 1
      common_name = fct_reorder(common_name, pri_rank, .desc = TRUE)
    )
  
  #custom legends
  legend_info <- foo %>%
    group_by(common_name, color) %>%
    summarise(mean_rank = mean(pri_rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_rank)
  fill_breaks <- legend_info$color
  fill_labels <- paste0(
    legend_info$common_name,
    "\n(mean rank = ",
    round(legend_info$mean_rank, 1),
    ")"
  )
  
  foo %>%
    filter(pri_rank <= 10) %>%
    ggplot(aes(x = pri_rank, fill = color)) +
    geom_bar(position = "stack") +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_identity(
      guide = "legend",
      breaks = fill_breaks,
      labels = fill_labels
    ) +
    theme_classic() +
    labs(
      x = "Rank",
      y = "Frequency",
    ) +
    theme(legend.position = "none") +
    labs(fill = "Species") +
    guides(
      fill = guide_legend(
        override.aes = list(size = 3)
      )
    ) +
    theme(
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(face = "bold", margin = margin(t = 17)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 17))
    )
}



#' inefficiently get a legend to screenshot for the main uncertainty figure
#'
#' @param dataset is the particular weighted simulation file you want  (111, 211, etc in this case)
#' @param spref is the ordered species vector to match the colors
#' @param colref is the color vector
#' @param selection region (e.g., "CA")
#'
#' @returns
#' @export
#'
#' @examples
stackedhist4 <- function(dataset, spref, colref, selection){
  # Create a tibble of priority species and colors
  all_species <- unique(dataset$common_name)
  all_codes <- unique(dataset$alpha_code)
  priority_colors <- tibble(common_name = spref, color = colref)
  all_species_colors <- tibble(common_name = all_species,
                               alpha_code = all_codes) %>%
    left_join(priority_colors, by = "common_name") %>%
    mutate(color = if_else(is.na(color), "#008000", color))
  result_colored <- dataset %>%
    left_join(all_species_colors, by = "common_name")
  
  #join w/ main
  foo <- result_colored %>% 
    filter(region == selection) %>% 
    mutate(common_name = fct_reorder(common_name, pri_rank, .desc = TRUE))
  foo_keep <- foo %>%
    group_by(common_name) %>%
    summarize(keep = any(pri_rank <= 10)) %>%
    filter(keep)
  
  foo <- foo %>%
    semi_join(foo_keep, by = "common_name") %>%
    mutate(
      # Order so top of plot is level 1
      common_name = fct_reorder(common_name, pri_rank, .desc = TRUE)
    )
  
  #custom legends
  legend_info <- foo %>%
    group_by(common_name, color) %>%
    summarise(mean_rank = mean(pri_rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_rank)
  fill_breaks <- legend_info$color
  fill_labels <- paste0(
    legend_info$common_name,
    "\n(mean rank = ",
    round(legend_info$mean_rank, 1),
    ")"
  )
  
  foo %>%
    filter(pri_rank <= 10) %>%
    ggplot(aes(x = pri_rank, fill = color)) +
    geom_bar(position = "stack") +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_identity(
      guide = "legend",
      breaks = fill_breaks,
      labels = fill_labels
    ) +
    theme_classic() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(legend.position = "right") +
    labs(fill = "Species") +
    guides(
      fill = guide_legend(
        ncol = 1,
        override.aes = list(size = 3)
      )
    ) +
    theme(
      legend.key.size = unit(1.2, "cm"),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 17),
    )
}




#' make sensitivity analysis stacked histograms
#'
#' @param dataset is the particular weighted simulation file you want  (111, 211, etc in this case)
#' @param spref is the ordered species vector to match the colors
#' @param colref is the color vector
#' @param selection region (e.g., "CA")
#'
#' @returns
#' @export
#'
#' @examples
stackedhist2 <- function(dataset, spref, colref, selection){
  # Create a tibble of priority species and colors
  all_species <- unique(dataset$common_name)
  all_codes <- unique(dataset$alpha_code)
  priority_colors <- tibble(common_name = spref, color = colref)
  all_species_colors <- tibble(common_name = all_species,
                               alpha_code = all_codes) %>%
    left_join(priority_colors, by = "common_name") %>%
    mutate(color = if_else(is.na(color), "#008000", color))
  result_colored <- dataset %>%
    left_join(all_species_colors, by = "common_name")
  
  #join w/ main
  foo <- result_colored %>% 
    filter(region == selection) %>% 
    mutate(common_name = fct_reorder(common_name, pri_rank, .desc = TRUE))
  foo_keep <- foo %>%
    group_by(common_name) %>%
    summarize(keep = any(pri_rank <= 10)) %>%
    filter(keep)
  
  foo <- foo %>%
    semi_join(foo_keep, by = "common_name") %>%
    mutate(
      # Order so top of plot is level 1
      common_name = fct_reorder(common_name, pri_rank, .desc = TRUE)
    )
  
  #custom legends
  legend_info <- foo %>%
    group_by(common_name, color) %>%
    summarise(mean_rank = mean(pri_rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_rank)
  fill_breaks <- legend_info$color
  fill_labels <- paste0(
    legend_info$common_name,
    "\n(mean rank = ",
    round(legend_info$mean_rank, 1),
    ")"
  )
  
  foo %>%
    filter(pri_rank <= 5) %>%
    ggplot(aes(x = pri_rank, fill = color)) +
    geom_bar(position = "stack") +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_identity(
      guide = "legend",
      breaks = fill_breaks,
      labels = fill_labels
    ) +
    theme_classic() +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(legend.position = "right") +
    labs(fill = "Species") +
    guides(
      fill = guide_legend(
        ncol = 1,
        override.aes = list(size = 3)
      )
    ) +
    theme(
      legend.key.size = unit(1.2, "cm"),
      legend.title = element_text(size = 17),
      legend.text = element_text(size = 14),
      axis.text = element_text(size = 17),
    )
}


