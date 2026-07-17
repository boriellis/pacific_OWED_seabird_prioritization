##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script S1: Exposure Processing Sensitivity Analyses ###################
#-------------------------------------------------------------------------
#
# Robustness of the exposure pipeline to its two processing parameters:
#
#   PART A - outlier cutoff (k): the median*k rule that flags/drops anomalous
#            bootstrap iterations. Compared at k = 10, 100, 1000, holding the
#            rescaling anchor fixed at the main-analysis value (0.99). Shown at
#            the exposure level only (distributions + retained sample sizes);
#            k is a data-cleaning choice, so its due diligence is demonstrated
#            before rescaling/priority.
#
#   PART B - rescaling anchor: the central quantile span onto which pooled
#            overlaps are mapped to [0.5, 2.0]. Compared at 0.95 / 0.99 / 0.998,
#            holding k fixed at the main-analysis value (1000). Shown through to
#            final priority ranks, since the anchor shapes the vulnerability
#            scores directly.
#
# Reads pre-computed raw-exposure objects (one per k) rather than regenerating
# rasters. Those objects take several hours each to produce; they are generated
# by scripts/01_exposure.R by setting k to 10, 100, and 1000 in turn.
#
# Inputs:  output/raw_exposure_k{10,100,1000}.rds
#          output/sensitivity_sum.rds, output/status.rds  (for Part B ranks)
# Outputs: output/sensitivity_analysis/  (comparison tables + figures)
#-------------------------------------------------------------------------


# Setup -----------------------------------------------------------------------

packages <- c("tidyverse", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/exposure.R"))    # clean_exposure / rescale_overlap
source(here::here("R/priority.R"))    # calc_priority  (Part B)

out_dir <- here::here("output/sensitivity_analysis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# small helper: per-species summary of a list-column of overlap values
summarize_overlap <- function(df, col) {
  df %>%
    mutate(
      n_boot = map_int({{ col }}, length),
      mean   = map_dbl({{ col }}, mean,   na.rm = TRUE),
      median = map_dbl({{ col }}, median, na.rm = TRUE),
      lwr    = map_dbl({{ col }}, \(x) quantile(x, 0.025, na.rm = TRUE)),
      upr    = map_dbl({{ col }}, \(x) quantile(x, 0.975, na.rm = TRUE))
    )
}


#=============================================================================
# PART A — Outlier cutoff (k) sensitivity
#   Vary k; hold anchor fixed at 0.99. Exposure level only.
#=============================================================================

k_values <- c(10, 100, 1000)

# raw exposure for each k (pre-computed; see header)
raw_by_k <- map(k_values, \(k) {
  readRDS(here::here(str_glue("output/raw_exposure_k{k}.rds")))
}) |> set_names(k_values)


# --- A1: retained sample size per species across k --------------------------
# How aggressively each cutoff trims. LOON-family species lose the most.

n_boot_by_k <- imap_dfr(raw_by_k, \(obj, k) {
  obj %>% mutate(k = as.integer(k)) %>% select(k, region, alpha_code, n_boot)
})

n_boot_wide <- n_boot_by_k %>%
  filter(region == "CA") %>%
  select(-region) %>%
  pivot_wider(names_from = k, values_from = n_boot, names_prefix = "k") %>%
  arrange(k1000)

write_csv(n_boot_wide, file.path(out_dir, "A_retained_n_by_k.csv"))


# --- A2: overlap summaries across k -----------------------------------------
# Mean/median/95% bounds of RAW overlap per species x k (region "all").

raw_summary_by_k <- imap_dfr(raw_by_k, \(obj, k) {
  obj %>%
    summarize_overlap(raw_overlap) %>%
    mutate(k = as.integer(k)) %>%
    select(k, region, alpha_code, n_boot, mean, median, lwr, upr)
}) %>%
  filter(region == "all") %>%
  arrange(alpha_code, k)

write_csv(raw_summary_by_k, file.path(out_dir, "A_overlap_summary_by_k.csv"))


# --- A3: distribution plot, good vs. bad species across k -------------------
# Good species (no outliers) should be invariant to k; bad species (outlier-
# affected) should tighten as the cutoff tightens.

good  <- c("BVSH", "CATE", "PFSH")
bad   <- c("COLO", "BOGU", "REPH")     # REPH = PHAL-driven
focal <- c(good, bad)

draws_by_k <- imap_dfr(raw_by_k, \(obj, k) {
  obj %>%
    filter(alpha_code %in% focal, region == "all") %>%
    mutate(k = factor(k, levels = c("1000", "100", "10"))) %>%
    select(k, alpha_code, raw_overlap) %>%
    unnest(raw_overlap)
})

p_k <- draws_by_k %>%
  ggplot(aes(raw_overlap, y = k, color = k)) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 0.5) +
  facet_wrap(~ alpha_code, scales = "free_x", ncol = 3) +
  scale_x_log10() +
  labs(x = "raw proportional overlap (log)", y = "outlier cutoff (k)",
       title = "Exposure distributions across outlier cutoffs") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "A_distributions_by_k.png"), p_k,
       width = 10, height = 6, dpi = 300)


#=============================================================================
# PART B — Rescaling anchor sensitivity
#   Vary anchor; hold k fixed at 1000. Through to final priority ranks.
#=============================================================================

anchors <- c(`0.95` = 0.95, `0.99` = 0.99, `0.998` = 0.998)

raw_1000 <- raw_by_k[["1000"]]

# rescale the same raw exposure under each anchor
cleaned_by_anchor <- map(anchors, \(a) clean_exposure(raw_1000, anchor = a))


# --- B1: clamping cost per anchor -------------------------------------------
# Fraction of draws capped at each rail. Tighter anchors clamp more; the
# ceiling matters most (high-exposure tail).

clamp_report <- imap_dfr(cleaned_by_anchor, \(obj, a) {
  v <- obj %>% filter(region == "CA") %>% pull(scaled_overlap) %>% unlist()
  tibble(anchor = a,
         pct_floor   = mean(v <= 0.5 + 1e-9) * 100,
         pct_ceiling = mean(v >= 2.0 - 1e-9) * 100)
})

write_csv(clamp_report, file.path(out_dir, "B_clamp_report.csv"))


# --- B2: between-species spread per anchor ----------------------------------
# Looser anchors compress the middle; check species stay distinguishable.

spread_report <- imap_dfr(cleaned_by_anchor, \(obj, a) {
  obj %>% filter(region == "CA") %>%
    mutate(med = map_dbl(scaled_overlap, median)) %>%
    summarize(anchor = a,
              range_of_medians = diff(range(med)),
              sd_of_medians    = sd(med))
})

write_csv(spread_report, file.path(out_dir, "B_spread_report.csv"))


# --- B3: rescaled distribution plot per anchor ------------------------------

p_anchor <- imap_dfr(cleaned_by_anchor, \(obj, a) {
  obj %>% filter(region == "CA") %>% mutate(anchor = a) %>% unnest(scaled_overlap)
}) %>%
  ggplot(aes(scaled_overlap, color = alpha_code)) +
  geom_density() +
  scale_y_continuous(transform = "log1p") +
  facet_wrap(~ anchor, ncol = 3) +
  labs(x = "rescaled exposure", title = "Rescaled exposure across anchors") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "B_distributions_by_anchor.png"), p_anchor,
       width = 10, height = 5, dpi = 300)


# --- B4: impact on final priority ranks -------------------------------------
# The decision-critical check: run each anchor's exposure through calc_priority
# (main-analysis weights) and compare resulting ESS ranks.

sensitivity <- read_rds(here::here("output/sensitivity_sum.rds"))
status      <- read_rds(here::here("output/status.rds"))

ess_by_anchor <- imap_dfr(cleaned_by_anchor, \(exp_obj, a) {
  calc_priority(exp_obj, sensitivity, status, w = c(3, 2, 1)) %>%
    select(alpha_code, region, ess) %>%
    mutate(anchor = a)
})

# ranks per anchor (CA), wide, sorted by the main-analysis (0.99) ranking
rank_by_anchor <- ess_by_anchor %>%
  filter(region == "CA") %>%
  group_by(anchor) %>%
  mutate(rank = rank(-ess, ties.method = "min")) %>%
  ungroup() %>%
  select(alpha_code, anchor, rank) %>%
  pivot_wider(names_from = anchor, values_from = rank, names_prefix = "rank_") %>%
  mutate(rank_range = pmax(rank_0.95, rank_0.99, rank_0.998) -
           pmin(rank_0.95, rank_0.99, rank_0.998)) %>%
  arrange(rank_0.99)

write_csv(rank_by_anchor, file.path(out_dir, "B_rank_by_anchor.csv"))

# concordance: Spearman on ESS across anchor pairs (one-number robustness stat)
rank_concordance <- ess_by_anchor %>%
  filter(region == "CA") %>%
  select(alpha_code, anchor, ess) %>%
  pivot_wider(names_from = anchor, values_from = ess) %>%
  summarize(
    rho_95_99   = cor(`0.95`, `0.99`,  method = "spearman"),
    rho_99_998  = cor(`0.99`, `0.998`, method = "spearman"),
    rho_95_998  = cor(`0.95`, `0.998`, method = "spearman")
  )

write_csv(rank_concordance, file.path(out_dir, "B_rank_concordance.csv"))