##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Visualization Function Definitions ######################################
#-------------------------------------------------------------------------
#
# Functions called by scripts/05_plot.R and scripts/06_sensitivity_analysis.R
# to produce the manuscript figures.
#
# Workflow:
#   score_colours()    - map vulnerability scores to lipari colours (used by
#                        ridgeplot() and Appendix J panels)
#   make_boxplot()     - Figure 4 (panel 1): per-species vulnerability score
#                         boxplots, ordered taxonomically
#   make_vuln_scatter() - Figure 4 (panel 2): exposure x sensitivity scatter,
#                         colored by vulnerability score
#   ridgeplot()         - Figure 5 / Appendix J: rank distribution ridge plot
#
#-------------------------------------------------------------------------


# FAMILY COLOURS -----------------------------------------------------------

# Taxonomic family background bands for make_boxplot() (scico "lipari"-style
# ramp, ordered from most to least pelagic).
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

# SCORE COLOURS -----------------------------------------------------------

#' Map vulnerability scores to lipari colours on a log2 scale
#'
#' Used to colour points/ridges by vulnerability score (ess) on the same
#' log2 scale used elsewhere in the manuscript (e.g. the vulnerability
#' heatmap, Figure 1).
#'
#' @param scores Priority table from calc_priority() (alpha_code, common_name,
#'   region, ess).
#' @param selection Region to build colours for (e.g. "CA").
#' @param limits Score range the palette spans, in raw (not log2) units.
#'   Default c(2^-6, 2^6), i.e. 0.0156 to 64, the framework's full
#'   theoretical range.
#' @param direction Palette direction; use -1 to reverse.
#' @param n_colours Number of colours to sample from the lipari ramp before
#'   interpolating. Default 256.
#'
#' @returns A tibble of alpha_code, common_name, ess, and hex colour, ordered
#'   by descending score.
#'
score_colours <- function(scores, selection,
                          limits = c(2^-6, 2^6),
                          direction = 1,
                          n_colours = 256) {
  
  ramp <- scales::gradient_n_pal(
    scico::scico(n_colours, palette = "lipari", direction = direction)
  )
  
  scores %>%
    filter(region == selection, !is.na(ess)) %>%
    mutate(
      # position on the log2 scale, 0 = lower limit, 1 = upper limit
      pos   = (log2(ess) - log2(limits[1])) / (log2(limits[2]) - log2(limits[1])),
      pos   = pmin(pmax(pos, 0), 1),   # clamp anything outside the range
      color = ramp(pos)
    ) %>%
    arrange(desc(ess)) %>%
    select(alpha_code, common_name, ess, color)
}

# PRIORITY BOXPLOT -----------------------------------------------------------

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
      legend.position  = "none",
      legend.box       = "horizontal",
      legend.key.width = unit(0.5, "cm"),
      legend.key.height= unit(0.3, "cm"),
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 7)
    )
}

# VULNERABILITY SCATTER -------------------------------------------------

#' Exposure x sensitivity scatter, colored by vulnerability score
#'
#' Plots each species by its weighted, rescaled exposure (x) and sensitivity
#' (y), sized by IUCN status. A horizontal bar spans each species' exposure
#' 95% CI, colored along its length by the vulnerability score that exposure
#' value would produce (holding sensitivity and status fixed at their point
#' estimates) — showing how much vulnerability would shift across the
#' exposure uncertainty range. Color follows the log2 vulnerability scale,
#' spanning the framework's full theoretical range (0.0156-64).
#'
#' @param priority_dists Priority table from calc_priority() (one row per
#'   species x region), carrying e, e_lwr, e_upr, sensitivity, status, ess,
#'   and rl_category.
#' @param selection Region to plot (e.g. "CA"). Default "CA".
#' @param w Length-3 vector of weight exponents (exposure, sensitivity,
#'   status), matching whatever weighting produced `priority_dists`. Default
#'   c(3, 2, 1), the main-analysis weighting.
#' @param n_seg Number of segments per exposure-CI bar; higher = smoother
#'   color gradient. Default 30.
#'
#' @returns A ggplot object.
#'
make_vuln_scatter <- function(priority_dists, selection = "CA",
                              w = c(3, 2, 1), n_seg = 30) {
  
  d <- priority_dists %>%
    filter(region == selection, !is.na(ess)) %>%
    mutate(
      x = e,                  # weighted, rescaled exposure (scaled_overlap^w[1])
      y = sensitivity^w[2],   # weighted, rescaled sensitivity
      log2_ess = log2(ess),
      rl_category = factor(rl_category, levels = c("LC", "NT", "VU", "EN", "CR"))
    )
  
  # Recompute ESS along each species' exposure CI, holding sensitivity/status
  # fixed at their point estimates, so the bar's color gradient reflects the
  # actual weighted vulnerability formula (Equation 2).
  d_segments <- d %>%
    rowwise() %>%
    mutate(
      x_seq   = list(seq(e_lwr, e_upr, length.out = n_seg)),
      ess_seq = list(x_seq * sensitivity^w[2] * status^w[3])
    ) %>%
    ungroup() %>%
    select(alpha_code, y, x_seq, ess_seq) %>%
    unnest(c(x_seq, ess_seq)) %>%
    group_by(alpha_code) %>%
    mutate(
      x_end        = lead(x_seq),
      log2_ess_seg = log2((ess_seq + lead(ess_seq)) / 2)   # midpoint ESS of each mini-segment
    ) %>%
    filter(!is.na(x_end)) %>%
    ungroup()
  
  ggplot(d, aes(x = x, y = y)) +
    geom_segment(
      data = d_segments,
      aes(x = x_seq, xend = x_end, y = y, yend = y, color = log2_ess_seg),
      linewidth = 0.4, alpha = 0.5
    ) +
    geom_point(aes(color = log2_ess, size = rl_category), stroke = 0) +
    scale_x_log10(breaks = 2^(-3:3), labels = 2^(-3:3)) +
    scale_y_log10(breaks = 2^(-2:2), labels = 2^(-2:2)) +
    scale_color_gradientn(
      colours = scico::scico(256, palette = "lipari"),
      limits  = c(-6, 6),
      breaks  = seq(-6, 6, 2),
      labels  = c("0.0156", "0.0625", "0.25", "1", "4", "16", "64"),
      name    = "Vulnerability"
    ) +
    scale_size_manual(
      values = c(LC = 3, NT = 4, VU = 5, EN = 6, CR = 7),
      name   = "IUCN status",
      drop   = FALSE
    ) +
    labs(x = "Weighted exposure (rescaled, log scale)",
         y = "Weighted sensitivity (rescaled, log scale)") +
    theme_classic() +
    theme(
      axis.title        = element_text(size = 8),
      axis.text         = element_text(size = 7),
      axis.title.x      = element_text(face = "bold"),
      axis.title.y      = element_text(face = "bold"),
      legend.key.width  = unit(0.5, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title      = element_text(size = 8),
      legend.text       = element_text(size = 7)
    )
}


# RANK RIDGE PLOT ------------------------------------------------------------

#' Ridge plot of priority rank distributions
#'
#' Shows the rank distribution of each species, capturing rank uncertainty
#' propagated from exposure. Species are ordered by mean priority score
#' (ESS), best-ranked at the top.
#'
#' @param dataset MC rank output from priority_mc() (alpha_code, region,
#'   common_name, pri_rank).
#' @param scores Priority table from calc_priority() (alpha_code, region, ess),
#'   used to order species by mean priority score.
#' @param spref Ordered species vector matching colref.
#' @param colref Colour vector.
#' @param selection Region to plot (e.g. "CA").
#' @param top_n Number of species to show, by mean priority score. Default 57
#'   (all species).
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

