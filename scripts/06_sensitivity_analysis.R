##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 06: Weighting Sensitivity Analysis ##############################
#-------------------------------------------------------------------------
#
# Robustness of species prioritization to the choice of exponential weights
# in Eq. 2 (exposure, sensitivity, status). Compares the main-analysis
# weighting (3,2,1) against four alternatives: (1,1,1), (2,1,1), (1,2,1),
# (1,1,2).
#
#   PART A - Spearman concordance: pairwise rank correlation of mean ESS
#            across all five weighting schemes.
#   PART B - median-rank-by-scheme table: for every species, its median MC
#            rank under each scheme, flagging which schemes place it in the
#            top 10.
#   PART C - faceted ridge plot: rank distributions across schemes, species
#            fixed to the top-10-under-321 set and ordering.
#
# Inputs:  output/priority_values/priority_scores_{weights}.rds
#          output/rank_mc/priority_ranks_{weights}.rds
# Outputs: output/sensitivity_analysis/  (comparison tables + figures)
#-------------------------------------------------------------------------


# Setup -----------------------------------------------------------------------

packages <- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

out_dir <- here::here("paper/appendix_F")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

weight_labels <- c("321", "111", "211", "121", "112")


#=============================================================================
# PART A — Spearman concordance across weighting schemes
#=============================================================================

# mean ESS per species under each weighting (point estimates, region = CA)
ess_by_weights <- map_dfr(weight_labels, \(label) {
  readRDS(here::here(str_glue("output/priority_values/priority_scores_{label}.rds"))) %>%
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

# symmetric matrix form, useful for a compact table in the appendix
concordance_matrix <- matrix(1, nrow = length(weight_labels), ncol = length(weight_labels),
                             dimnames = list(weight_labels, weight_labels))
walk(seq_len(nrow(concordance_pairs)), \(i) {
  a <- concordance_pairs$scheme_a[i]; b <- concordance_pairs$scheme_b[i]
  concordance_matrix[a, b] <<- concordance_pairs$rho[i]
  concordance_matrix[b, a] <<- concordance_pairs$rho[i]
})

concordance_matrix_df <- as_tibble(concordance_matrix, rownames = "scheme")
write_csv(concordance_matrix_df, file.path(out_dir, "A_spearman_concordance_matrix.csv"))



nice_labels <- c("321" = "(3,2,1)", "111" = "(1,1,1)", "211" = "(2,1,1)",
                 "121" = "(1,2,1)", "112" = "(1,1,2)")

plot_df <- concordance_matrix_df %>%
  pivot_longer(-scheme, names_to = "scheme2", values_to = "rho") %>%
  rename(scheme1 = scheme) %>%
  mutate(
    scheme1 = factor(nice_labels[scheme1],  levels = nice_labels[weight_labels]),
    scheme2 = factor(nice_labels[scheme2], levels = rev(nice_labels[weight_labels]))
  )

p_concordance <- ggplot(plot_df, aes(x = scheme1, y = scheme2, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, limits = c(-1, 1), name = expression(rho)
  ) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.text.y = element_text(hjust = 1)
  )

p_concordance



ggsave(file.path(out_dir, "A_spearman_concordance_plot.png"), p_concordance,
       width = 140, height = 120, units = "mm", dpi = 500)






#=============================================================================
# PART B — Median rank by weighting scheme
#=============================================================================

med_rank_by_scheme <- map_dfr(weight_labels, \(label) {
  readRDS(here::here(str_glue("output/rank_mc/priority_ranks_{label}.rds"))) %>%
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

#=============================================================================
# APPENDIX D — iterations retained per species, by season and annual
#=============================================================================


sp       <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
raw_1000 <- readRDS(here::here("output/exposure_values/raw_exposure.rds"))
elicited <- c("HAPE", "TOSP", "STAL")

boot_dir <- here::here("data/raw_data/leirness_bootstrapped_models")
out_dir <- here::here("paper/appendix_D")
seasons  <- c("spring", "summer", "fall", "winter")
k        <- 1000

# --- per-season retained iteration counts, one row per model x season ------
season_pattern <- "_(spring|summer|fall|winter)_"
files <- dir(boot_dir, pattern = "\\.tif$", full.names = TRUE)

seasonal_counts <- map_dfr(files, \(f) {
  r      <- rast(f)
  maxes  <- global(r, "max", na.rm = TRUE)[, 1]
  cutoff <- k * median(maxes, na.rm = TRUE)
  
  tibble(
    exposure_model = str_extract(basename(f), paste0("^.+(?=", season_pattern, ")")),
    season      = str_extract(basename(f), season_pattern) |> str_remove_all("_"),
    n_retained  = sum(maxes <= cutoff, na.rm = TRUE)
  )
})

# wide: one row per model, one column per season (models not run in a season -> NA)
seasonal_wide <- seasonal_counts %>%
  pivot_wider(names_from = season, values_from = n_retained) %>%
  select(exposure_model, any_of(seasons))   # enforce spring/summer/fall/winter column order

# guarantee all four season columns exist even if one season is present in zero files
missing_seasons <- setdiff(seasons, names(seasonal_wide))
for (s in missing_seasons) seasonal_wide[[s]] <- NA_integer_
seasonal_wide <- seasonal_wide %>% select(exposure_model, all_of(seasons))


# --- annual retained counts (existing logic) --------------------------------
iterations_table <- raw_1000 %>%
  distinct(alpha_code, n_boot) %>%
  left_join(sp %>% select(alpha_code, common_name, exposure_model), by = "alpha_code") %>%
  mutate(exposure_model = if_else(alpha_code %in% elicited, "elicited", exposure_model)) %>%
  select(common_name, exposure_model, n_retained_annual = n_boot) %>%
  arrange(exposure_model, common_name)

stopifnot(nrow(iterations_table) == n_distinct(raw_1000$alpha_code))


# --- join seasonal counts onto the species table -----------------------------
# elicited species have no seasonal model of their own; seasons are NA by construction
iterations_table <- iterations_table %>%
  left_join(seasonal_wide, by = "exposure_model") %>%
  relocate(spring, summer, fall, winter, .after = exposure_model)

write_csv(iterations_table, file.path(out_dir, "D_iterations_retained.csv"))

iterations_table <- iterations_table %>%
  mutate(n_removed = 200 - n_retained_annual)

max(iterations_table$n_removed)
median(iterations_table$n_removed)

