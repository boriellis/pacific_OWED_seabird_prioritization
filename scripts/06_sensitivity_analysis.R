##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 06: Weighting Sensitivity Analysis (Appendix J) #################
#-------------------------------------------------------------------------
#
# Robustness of species prioritization to the choice of exponential weights
# in Eq. 2 (exposure, sensitivity, status). Compares the main-analysis
# weighting (3,2,1) against four alternatives: (1,1,1), (2,1,1), (1,2,1),
# (1,1,2). Rankings for all five schemes are drawn from the Monte Carlo
# resampling already produced in scripts/05_plot.R (Part 2); that must be
# run first (or output/rank_mc/priority_ranks_{weights}.rds must already
# exist) before this script is run.
#
#   PART A - Spearman concordance: pairwise rank correlation of mean ESS
#            across all five weighting schemes.
#   PART B - median-rank-by-scheme table: for every species, its median MC
#            rank under each scheme; also flags species meeting a median
#            rank of <=10 under every scheme.
#   PART C - ridge plots: rank distributions under each weighting scheme,
#            species/order fixed to the main-analysis (321) weighting so
#            panels are directly comparable when combined in Illustrator.
#
# Inputs:  output/priority_values/priority_scores_{weights}.rds
#          output/rank_mc/priority_ranks_{weights}.rds
# Outputs: paper/appendix_J/  (concordance table/plot, median rank table,
#          per-scheme ridge plots)
#-------------------------------------------------------------------------


# Setup -----------------------------------------------------------------------

packages <- c("tidyverse", "here", "ggridges", "scico", "scales")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/visualizations.R"))

out_dir <- here::here("paper/appendix_J")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

weight_labels <- c("321", "111", "211", "121", "112")

priority_sets <- map(weight_labels, \(label) {
  readRDS(here::here(str_glue("output/priority_values/priority_scores_{label}.rds")))
}) |> set_names(weight_labels)

rank_sets <- map(weight_labels, \(label) {
  readRDS(here::here(str_glue("output/rank_mc/priority_ranks_{label}.rds")))
}) |> set_names(weight_labels)


#=============================================================================
# PART A — Spearman concordance across weighting schemes
#=============================================================================

ess_by_weights <- imap_dfr(priority_sets, \(tbl, label) {
  tbl %>%
    filter(region == "CA", !is.na(ess)) %>%
    select(alpha_code, ess) %>%
    mutate(weights = label)
})

ess_wide <- ess_by_weights %>%
  pivot_wider(names_from = weights, values_from = ess)

# all pairwise Spearman correlations
concordance_pairs <- combn(weight_labels, 2, simplify = FALSE) %>%
  map_dfr(\(pair) tibble(
    scheme_a = pair[1], scheme_b = pair[2],
    rho = cor(ess_wide[[pair[1]]], ess_wide[[pair[2]]], method = "spearman")
  )) %>%
  arrange(rho)

write_csv(concordance_pairs, file.path(out_dir, "A_spearman_concordance_pairs.csv"))
range(concordance_pairs$rho)   # for the manuscript text

# symmetric matrix form
concordance_matrix <- matrix(1, nrow = length(weight_labels), ncol = length(weight_labels),
                             dimnames = list(weight_labels, weight_labels))
walk(seq_len(nrow(concordance_pairs)), \(i) {
  a <- concordance_pairs$scheme_a[i]; b <- concordance_pairs$scheme_b[i]
  concordance_matrix[a, b] <<- concordance_pairs$rho[i]
  concordance_matrix[b, a] <<- concordance_pairs$rho[i]
})
concordance_matrix_df <- as_tibble(concordance_matrix, rownames = "scheme")
write_csv(concordance_matrix_df, file.path(out_dir, "A_spearman_concordance_matrix.csv"))

# staircase heatmap: full grid minus diagonal/duplicates
nice_labels <- c("321" = "(3,2,1)", "111" = "(1,1,1)", "211" = "(2,1,1)",
                 "121" = "(1,2,1)", "112" = "(1,1,2)")
ordered_names <- nice_labels[weight_labels]

plot_df <- concordance_matrix_df %>%
  pivot_longer(-scheme, names_to = "scheme2", values_to = "rho") %>%
  rename(scheme1 = scheme) %>%
  mutate(
    scheme1_idx = match(scheme1, weight_labels),
    scheme2_idx = match(scheme2, weight_labels)
  ) %>%
  filter(scheme2_idx < scheme1_idx) %>%
  mutate(
    scheme_x = factor(nice_labels[scheme2], levels = ordered_names),
    scheme_y = factor(nice_labels[scheme1], levels = rev(ordered_names[-1]))
  )

p_concordance <- ggplot(plot_df, aes(x = scheme_x, y = scheme_y, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, limits = c(-1, 1), name = expression(rho)
  ) +
  scale_x_discrete(position = "top", drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.text.y = element_text(hjust = 1)
  )

ggsave(file.path(out_dir, "A_spearman_concordance_plot.png"), p_concordance,
       width = 140, height = 120, units = "mm", dpi = 500)


#=============================================================================
# PART B — Median rank by weighting scheme
#=============================================================================

med_rank_by_scheme <- imap_dfr(rank_sets, \(tbl, label) {
  tbl %>%
    filter(region == "CA") %>%
    group_by(alpha_code, common_name) %>%
    summarize(med_rank = median(pri_rank), .groups = "drop") %>%
    mutate(weights = label)
})

med_rank_wide <- med_rank_by_scheme %>%
  select(-alpha_code) %>%
  pivot_wider(names_from = weights, values_from = med_rank, names_prefix = "median_rank_") %>%
  arrange(median_rank_321)

write_csv(med_rank_wide, file.path(out_dir, "B_median_rank_by_scheme.csv"))

# species meeting median rank <=10 under EVERY scheme (supporting check for
# the manuscript text; not a separate appendix table)
med_rank_by_scheme %>%
  group_by(alpha_code, common_name) %>%
  summarize(worst_med = max(med_rank), n_schemes = n(), .groups = "drop") %>%
  filter(n_schemes == length(weight_labels), worst_med <= 10) %>%
  arrange(worst_med)


#=============================================================================
# PART C — Ridge plots per weighting scheme
#=============================================================================
# Species/order fixed to the main-analysis (321) weighting throughout, so
# panels are directly comparable when combined in Illustrator.

anchor_limits <- list("321" = c(2^-6, 2^6), "111" = c(2^-3, 2^3),
                      "211" = c(2^-4, 2^4), "121" = c(2^-4, 2^4),
                      "112" = c(2^-4, 2^4))

iwalk(weight_labels |> set_names(weight_labels), \(label, .) {
  cols <- score_colours(priority_sets[[label]], "CA", limits = anchor_limits[[label]])
  p <- ridgeplot(rank_sets[[label]], priority_sets[["321"]],
                 cols$common_name, cols$color, "CA")
  ggsave(file.path(out_dir, str_glue("{label}_ridgeplot.png")), plot = p,
         width = 60, height = 200, units = "mm", dpi = 500)
})
