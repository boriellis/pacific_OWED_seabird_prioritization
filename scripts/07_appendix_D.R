##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 07: Bootstrap Iterations Retained (Appendix D) ##################
#-------------------------------------------------------------------------
#
# Reports the number of bootstrap iterations retained per species, per
# season and annually, after exclusion of iterations containing
# biologically implausible predicted densities (median*k rule, k=1000; see
# scripts/01_exposure.R). Re-scans the raw seasonal bootstrap rasters to
# compute per-season counts (these are not saved anywhere else in the
# pipeline); annual counts are read from the already-computed raw exposure.
#
# Inputs:  data/raw_data/leirness_bootstrapped_models/ (raw seasonal
#          bootstraps; slow to scan, ~36 GB, gitignored)
#          data/raw_data/total_sp_list.csv
#          output/exposure_values/raw_exposure.rds
# Outputs: paper/appendix_D/D_iterations_retained.csv
#-------------------------------------------------------------------------


# Setup -----------------------------------------------------------------------

packages <- c("tidyverse", "terra", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

sp       <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
raw_1000 <- readRDS(here::here("output/exposure_values/raw_exposure.rds"))
elicited <- c("HAPE", "TOSP", "STAL")

boot_dir <- here::here("data/raw_data/leirness_bootstrapped_models")
out_dir  <- here::here("paper/appendix_D")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

seasons <- c("spring", "summer", "fall", "winter")
k       <- 1000


# Per-season retained iteration counts -----------------------------------------
# One row per model x season, using the same median*k exclusion rule as
# build_keep_index() (R/exposure.R).

season_pattern <- "_(spring|summer|fall|winter)_"
files <- dir(boot_dir, pattern = "\\.tif$", full.names = TRUE)

seasonal_counts <- map_dfr(files, \(f) {
  r      <- rast(f)
  maxes  <- global(r, "max", na.rm = TRUE)[, 1]
  cutoff <- k * median(maxes, na.rm = TRUE)
  
  tibble(
    exposure_model = str_extract(basename(f), paste0("^.+(?=", season_pattern, ")")),
    season         = str_extract(basename(f), season_pattern) |> str_remove_all("_"),
    n_retained     = sum(maxes <= cutoff, na.rm = TRUE)
  )
})

# wide: one row per model, one column per season (models not run in a
# season -> NA)
seasonal_wide <- seasonal_counts %>%
  pivot_wider(names_from = season, values_from = n_retained) %>%
  select(exposure_model, any_of(seasons))

missing_seasons <- setdiff(seasons, names(seasonal_wide))
for (s in missing_seasons) seasonal_wide[[s]] <- NA_integer_
seasonal_wide <- seasonal_wide %>% select(exposure_model, all_of(seasons))


# Annual retained counts --------------------------------------------------------

iterations_table <- raw_1000 %>%
  distinct(alpha_code, n_boot) %>%
  left_join(sp %>% select(alpha_code, common_name, exposure_model), by = "alpha_code") %>%
  mutate(exposure_model = if_else(alpha_code %in% elicited, "elicited", exposure_model)) %>%
  select(common_name, exposure_model, n_retained_annual = n_boot) %>%
  arrange(exposure_model, common_name)

stopifnot(nrow(iterations_table) == n_distinct(raw_1000$alpha_code))


# Combine and write --------------------------------------------------------------
# Elicited species have no seasonal model of their own; seasons are NA by
# construction (no matching exposure_model in seasonal_wide).

iterations_table <- iterations_table %>%
  left_join(seasonal_wide, by = "exposure_model") %>%
  relocate(spring, summer, fall, winter, .after = exposure_model)

write_csv(iterations_table, file.path(out_dir, "D_iterations_retained.csv"))

# quick sanity check: how many iterations were excluded per species overall
iterations_table %>%
  mutate(n_removed = 200 - n_retained_annual) %>%
  summarize(max_removed = max(n_removed), median_removed = median(n_removed))