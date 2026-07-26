##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# Author: Aspen Ellis (aaellis@ucsc.edu) ################################
##########################################################################
# Script 05: Results Tables and Figures #################################
#-------------------------------------------------------------------------
#
# Produces the manuscript outputs from the priority scores:
#   - results tables (exposure, sensitivity, status, vulnerability, ranks)
#   - priority-score boxplots ordered taxonomically
#
# CA-region outputs go in the main manuscript; region-wide ("all") outputs
# go in the supplement.
#
# Rank uncertainty is derived from a Monte Carlo resampling of the exposure
# distribution (priority_mc): each iteration draws one exposure value per
# species, recomputes vulnerability, and re-ranks, giving a distribution of
# ranks per species. This propagates exposure uncertainty into rank space,
# which cannot be obtained from each species' marginal score distribution
# because ranks are relational.
#
# Inputs:  output/exposure_values/cleaned_exposure.rds
#          output/sensitivity_values/sensitivity_sum.rds
#          output/status_values/status.rds
#          output/priority_values/priority_scores_321.rds
#          data/raw_data/ (species list, Clements taxonomy)
# Outputs: output/rank_mc/priority_ranks_{weights}.rds
#          paper/CA_results_table.csv, paper/POCS_results_table.csv
#          paper/boxplot_CA.png, paper/boxplot_all.png
#-------------------------------------------------------------------------


# Part 1: Setup ---------------------------------------------------------------

packages <- c("tidyverse", "sf", "terra", "tidyterra", "ggridges", "scico", "here")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/visualizations.R"))

# the three rescaled factors and the main-analysis (321) priority scores
exposure    <- read_rds(here::here("output/exposure_values/cleaned_exposure.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_values/sensitivity_sum.rds"))
status      <- read_rds(here::here("output/status_values/status.rds"))
priority321    <- read_rds(here::here("output/priority_values/priority_scores_321.rds"))


priority111    <- read_rds(here::here("output/priority_values/priority_scores_111.rds"))
priority211    <- read_rds(here::here("output/priority_values/priority_scores_211.rds"))
priority121    <- read_rds(here::here("output/priority_values/priority_scores_121.rds"))
priority112    <- read_rds(here::here("output/priority_values/priority_scores_112.rds"))

# Part 2: Rank Monte Carlo ----------------------------------------------------
# SLOW. Resamples the exposure distribution to build rank distributions, for
# each weight set. Only needs rerunning if the upstream exposure, sensitivity,
# or status values change; otherwise the saved outputs are reused below.

regenerate_mc <- FALSE   # set TRUE to rebuild the rank MC outputs

mc_dir <- here::here("output/rank_mc")

if (regenerate_mc) {
  dir.create(mc_dir, recursive = TRUE, showWarnings = FALSE)
  
  weight_sets <- list("321" = c(3, 2, 1), "111" = c(1, 1, 1), "211" = c(2, 1, 1),
                      "121" = c(1, 2, 1), "112" = c(1, 1, 2))
  
  iwalk(weight_sets, \(w, label) {
    message("rank MC: weights ", label)
    ranks <- priority_mc(exposure, sensitivity, status, w = w)
    saveRDS(ranks, file.path(mc_dir, str_glue("priority_ranks_{label}.rds")))
  })
}


# Part 3: Results tables ------------------------------------------------------
# Per-species table of exposure (% overlap, 95% CI), collision (CV) and
# displacement (DV) sensitivity, IUCN category, vulnerability score (95% CI),
# and priority rank with its MC uncertainty band.

# mean/min/max rank across MC iterations (exposure uncertainty), 321 weighting
rank_summary <- summarize_ranks()

# CA region — main manuscript
results_table_CA <- clean_priority_vals(priority, exposure, sensitivity, status, rank_summary, "CA", "95")       

write_excel_csv(results_table_CA, here::here("paper/CA_results_table.csv"))

# all lease areas — supplement
results_table_all <- clean_priority_vals(
  raw_scores = priority, exposure = exposure,
  se = sensitivity, st = status,
  rank_summary = rank_summary, selection = "all", "95"
)
write_excel_csv(results_table_all, here::here("paper/POCS_results_table.csv"))


# Part 4: Priority boxplots ---------------------------------------------------
# Each species' full vulnerability distribution, ordered taxonomically with
# family shown as a background band. Boxes span the 2.5-97.5% quantiles.

sp_list <- read_csv(here::here("data/raw_data/total_sp_list.csv"))
tax     <- read_csv(here::here("data/raw_data/Clements-v2024-October-2024-rev.csv"))

# CA region — main manuscript
boxplot_CA <- make_boxplot(sp_list, tax, priority, "CA")
ggsave(here::here("paper/boxplot_CA.png"), plot = boxplot_CA,
       width = 190, height = 170, units = "mm", dpi = 500)

# all lease areas — supplement
boxplot_all <- make_boxplot(sp_list, tax, priority, "all")
ggsave(here::here("paper/boxplot_all.png"), plot = boxplot_all,
       width = 190, height = 170, units = "mm", dpi = 500)






###########################################################################
#####################                           ###########################
#####################      UNCERTAINTY PLOTS    ###########################
#####################                           ###########################
###########################################################################



# set up ------------------------------------------------------------------

# set colors
topsps_CA <- c("Pink-footed Shearwater", "Cassin's Auklet", "Red Phalarope", "Red-necked Phalarope", "Ashy Storm-Petrel", "Guadalupe Murrelet", "Pomarine Jaeger",  "Rhinoceros Auklet", "Craveri's Murrelet", "Sooty Shearwater", "Scripps's Murrelet", "Townsend's Storm-Petrel", "Buller's Shearwater", "Sabine's Gull", "South Polar Skua", "Marbled Murrelet", "Bonaparte's Gull", "Black Scoter", "Short-tailed Albatross","Northern Fulmar", "Hawaiian Petrel", "California Gull")

# if you want colors to be ordered by priority
spcolors_pri <- c("#F9DD8B", "#FBD588", "#F7C98D", "#F3BB84", "#E5A67C", "#E9946F", "#E88164", "#DB705F", "#C5655F", "#AE6363", "#996169", "#86606E", "#755F72", "#655E76", "#565B7A", "#405578", "#25486D", "#103657", "#07243E", "#021326", "#020F1C",  "#01080F")

spcolors_pri2 <- c("#F9CCF9", "#FBC2DE", "#FDB9C3", "#FDB0AA", "#FCA78E", "#F49F72", "#E69858", "#D29243", "#BC8E33", "#A48A2C", "#8D842E", "#767F36", "#627940", "#4F734B", "#3B6C55", "#2B655D", "#1E5C61", "#165261", "#114761", "#0E3B5F", "#0B2B5C",  "#001959")




#alternate color option:

library(scico)
library(scales)

#' Map vulnerability scores to lipari colours on a log2 scale centred at 1
#'
#' @param scores Priority table from calc_priority() (alpha_code, common_name,
#'   region, ess).
#' @param selection Region to build colours for (e.g. "CA").
#' @param limits Score range the palette spans. Default c(2^-6, 2^6), i.e.
#'   0.0156 to 64, symmetric around 1 on a log2 scale.
#' @param direction Palette direction; use -1 to reverse.
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

cols321 <- score_colours(priority321, "CA", limits = c(2^-6, 2^6))
cols111 <- score_colours(priority111, "CA", limits = c(2^-3, 2^3))
cols211 <- score_colours(priority211, "CA", limits = c(2^-4, 2^4))
cols121 <- score_colours(priority121, "CA", limits = c(2^-4, 2^4))
cols112 <- score_colours(priority112, "CA", limits = c(2^-4, 2^4))

# load differently weighted simulations

ranks321 <- readRDS(here::here("output/rank_mc/priority_ranks_321.rds"))
ranks111 <- readRDS(here::here("output/rank_mc/priority_ranks_111.rds"))
ranks211 <- readRDS(here::here("output/rank_mc/priority_ranks_211.rds"))
ranks121 <- readRDS(here::here("output/rank_mc/priority_ranks_121.rds"))
ranks112 <- readRDS(here::here("output/rank_mc/priority_ranks_112.rds"))


# ridge plots -------------------------------------------------------------

#main plot
p1 <- ridgeplot(ranks321, priority321, cols321$common_name, cols$color, "CA")
ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = p1, width = 90, height = 160, units = "mm", dpi = 500)

p2 <- ridgeplot(ranks111, priority321, cols111$common_name, cols$color, "CA")
p3 <- ridgeplot(ranks211, priority321, cols211$common_name, cols$color, "CA")
p4 <- ridgeplot(ranks121, priority321, cols121$common_name, cols$color, "CA")
p5 <- ridgeplot(ranks112, priority321, cols112$common_name, cols$color, "CA")






#main plot to wrap with others
p_wrap <- ridgeplot3(ranks321, topsps_CA, spcolors_pri, "CA", 57)
ggsave(here::here("paper/3_2_1_ridgeplot_wrap.png"), plot = p_wrap, width = 90, height = 160, units = "mm", dpi = 300)

#sensitivity plots to combine in illustrator 
p2 <- ridgeplot2(ranks111, topsps_CA, spcolors_pri, "CA", 37)
ggsave(here::here("paper/1_1_1_ridgeplot.png"), plot = p2, width = 8, height = 6, units = "in", dpi = 300)

p3 <- ridgeplot2(ranks211, topsps_CA, spcolors_pri, "CA", 43)
ggsave(here::here("paper/2_1_1_ridgeplot.png"), plot = p3, width = 8, height = 6, units = "in", dpi = 300)

p4 <- ridgeplot2(ranks121, topsps_CA, spcolors_pri, "CA", 25)
ggsave(here::here("paper/1_2_1_ridgeplot.png"), plot = p4, width = 8, height = 6, units = "in", dpi = 300)

p5 <- ridgeplot2(ranks112, topsps_CA, spcolors_pri, "CA", 20)
ggsave(here::here("paper/1_1_2_ridgeplot.png"), plot = p5, width = 8, height = 6, units = "in", dpi = 300)
















# stacked histograms -------------------------------------------------------

#colors for hists

hist_sp_colors <- tribble(
  ~alpha_code, ~common_name, ~color,
  "PFSH", "Pink-footed Shearwater", "#FF3C0C", 
  "REPH", "Red Phalarope","#3B2749", 
  "CAAU", "Cassin's Auklet", "#304D84",
  "RNPH", "Red-necked Phalarope",  "#3971B1", 
  "ASSP", "Ashy Storm-Petrel","#918DAC", 
  "GUMU", "Guadalupe Murrelet", "#BB9CA9",
  "POJA", "Pomarine Jaeger", "#EBA6A3", 
  "BOGU", "Bonaparte's Gull", "#FE9F87",
  "CRMU", "Craveri's Murrelet",  "#FF813D",
  "RHAU", "Rhinoceros Auklet",  "#FF5B1A", 
  "TOSP", "Townsend's Storm-Petrel", "#F5474B",
  "BULS", "Buller's Shearwater",  "#430B1D", 
  "BLSC", "Black Scoter", "#ED677B", 
  "MAMU", "Marbled Murrelet", "#E587AC", 
  "SCMU", "Scripps's Murrelet", "#DF9ED0",
  "HAPE", "Hawaiian Petrel",  "#D9AFEA"
)

hist_sp_colors <- tribble(
  ~alpha_code, ~common_name, ~color,
  "PFSH", "Pink-footed Shearwater", "#EC6F87", 
  "REPH", "Red Phalarope","#F54748", 
  "CAAU", "Cassin's Auklet", "#FE936C",
  "RNPH", "Red-necked Phalarope",  "#FF5518", 
  "ASSP", "Ashy Storm-Petrel","#FF7627",
  "GUMU", "Guadalupe Murrelet", "#3971B1",
  "TOSP", "Townsend's Storm-Petrel", "#304D84",
  "BLSC", "Black Scoter", "#3A3057", 
  "MAMU", "Marbled Murrelet", "#430B1D", 
)



#main plot

h1 <- stackedhist(ranks321, hist_sp_colors, "CA")
h2 <- stackedhist_t(ranks111, hist_sp_colors, "CA")
h3 <- stackedhist_t(ranks211, hist_sp_colors, "CA")
h4 <- stackedhist_t(ranks121, hist_sp_colors, "CA")
h5 <- stackedhist_t(ranks112, hist_sp_colors, "CA")

ggsave(here::here("paper/3_2_1_stackedhist.png"), plot = h1, width = 10, height = 8, units = "in", dpi = 300)

#for illustrator combo
h_wrap <- stackedhist3(ranks321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_stackedhist_wrap.png"), plot = h_wrap, width = 8, height = 6 , units = "in", dpi = 300)

#for legend screenshot
legend <- stackedhist4(ranks321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_legend.png"), plot = legend, width = 10, height = 12 , units = "in", dpi = 300)



#sensitivity plots to combine in illustrator 

h2 <- stackedhist2(ranks111, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_1_stackedhist.png"), plot = h2, width = 8, height = 6, units = "in", dpi = 300)

h3 <- stackedhist2(ranks211, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/2_1_1_stackedhist.png"), plot = h3, width = 8, height = 6, units = "in", dpi = 300)

h4 <- stackedhist2(ranks121, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_2_1_stackedhist.png"), plot = h4, width = 8, height = 6, units = "in", dpi = 300)

h5 <- stackedhist2(ranks112, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_2_stackedhist.png"), plot = h5, width = 8, height = 6, units = "in", dpi = 300)





weight_labels <- c("321", "111", "211", "121", "112")

all_ranks <- map_dfr(weight_labels, \(label) {
  readRDS(here::here(str_glue("output/rank_mc/priority_ranks_{label}.rds"))) %>%
    mutate(weights = label)
})

# per-weighting percentage of iterations in the top 10
pct_top10 <- all_ranks %>%
  filter(region == "CA") %>%
  group_by(alpha_code, common_name, weights) %>%
  summarize(pct_top10 = mean(pri_rank <= 10) * 100, .groups = "drop")

# wide view: how each species fares under each weighting
pct_wide <- pct_top10 %>%
  pivot_wider(names_from = weights, values_from = pct_top10) %>%
  arrange(desc(`321`))

pct_wide %>% print(n = Inf)


# CONSISTENT: top 10 in >=95% of iterations under every weighting
consistent <- pct_top10 %>%
  group_by(alpha_code, common_name) %>%
  summarize(min_pct = min(pct_top10), .groups = "drop") %>%
  filter(min_pct >= 50) %>%
  arrange(desc(min_pct))

consistent





priority_sets <- list(
  "321" = priority321, "111" = priority111, "211" = priority211,
  "121" = priority121, "112" = priority112
)

point_ranks <- imap_dfr(priority_sets, \(tbl, label) {
  tbl %>%
    filter(region == "CA", !is.na(ess)) %>%
    mutate(rank = rank(-ess, ties.method = "min")) %>%
    select(alpha_code, common_name, rank) %>%
    mutate(weights = label)
})

# wide view: point-estimate rank under each weighting
point_wide <- point_ranks %>%
  pivot_wider(names_from = weights, values_from = rank) %>%
  arrange(`321`)

point_wide %>% print(n = Inf)

# top 10 by point estimate under EVERY weighting
point_wide %>%
  filter(if_all(c(`321`, `111`, `211`, `121`, `112`), \(x) x <= 10))

# top 10 under at least one, with a count of how many
point_ranks %>%
  filter(rank <= 10) %>%
  count(alpha_code, common_name, name = "n_schemes") %>%
  arrange(desc(n_schemes))



ess_wide <- imap_dfr(priority_sets, \(tbl, label) {
  tbl %>% filter(region == "CA", !is.na(ess)) %>%
    select(alpha_code, ess) %>% mutate(weights = label)
}) %>%
  pivot_wider(names_from = weights, values_from = ess)

# all pairwise Spearman correlations
labels <- names(priority_sets)
concordance <- combn(labels, 2, simplify = FALSE) %>%
  map_dfr(\(pair) tibble(
    a = pair[1], b = pair[2],
    rho = cor(ess_wide[[pair[1]]], ess_wide[[pair[2]]], method = "spearman")
  )) %>%
  arrange(rho)

concordance
range(concordance$rho)   # the [X]-[Y] for the manuscript sentence


#PLOTTING SCRIPT TO MOVE TO SHINY APP 

library(plotly)

d <- priority321 %>%
  filter(region == "CA", !is.na(ess)) %>%
  mutate(x = log2(e),      # weighted, so planes are 45°
         y = log2(sensitivity^2),
         z = log2(status))

# grid for the iso-vulnerability planes: x + y + z = log2(V)
gx <- seq(min(d$x), max(d$x), length.out = 30)
gy <- seq(min(d$y), max(d$y), length.out = 30)
plane <- function(V) {
  z <- outer(gx, gy, \(a, b) log2(V) - a - b)
  z[z < -1 | z > 1] <- NA        # drop anything outside the threat range
  z
}

pal <- scico::scico(256, palette = "lipari")
colorscale <- lapply(seq_along(pal), \(i) list((i - 1) / (length(pal) - 1), pal[i]))

p <- plot_ly() %>%
  add_markers(
    data = d, x = ~x, y = ~y, z = ~z,
    text = ~common_name,
    marker = list(
      size = 4,
      color = ~log2(ess),
      colorscale = colorscale,
      cmin = -6, cmax = 6,
      colorbar = list(
        title = "Vulnerability",
        tickvals = seq(-6, 6, 2),
        ticktext = c("0.0156", "0.0625", "0.25", "1", "4", "16", "64")
      )
    )
  ) %>%
  layout(scene = list(
    xaxis = list(title = "log2(Exposure³)",    range = c(-3, 3), autorange = FALSE),
    yaxis = list(title = "log2(Sensitivity²)", range = c(-2, 2), autorange = FALSE),
    zaxis = list(title = "log2(Threat)",       range = c(-1, 1), autorange = FALSE),
    camera = list(eye = list(x = 1.6, y = 1.6, z = 1.0)),
    aspectmode = "manual",
    aspectratio = list(x = 3, y = 2, z = 1)
  ))
p





library(tidyverse)
library(scico)

d <- priority321 %>%
  filter(region == "CA", !is.na(ess)) %>%
  mutate(
    x = e,                  # weighted, rescaled exposure (scaled_overlap^3)
    e_lwr,
    e_upr,
    y = sensitivity^2,      # weighted, rescaled sensitivity
    log2_ess = log2(ess)    # colour on the log2 vulnerability scale
  )

# IUCN category as an ordered factor so the shape legend reads LC -> CR
d <- d %>%
  mutate(rl_category = factor(rl_category, levels = c("LC", "NT", "VU", "EN", "CR")))

vuln_scatter <- ggplot(d, aes(x = x, y = y, color = log2_ess)) +
  geom_point(stroke = 0, alpha = 0.8, aes(size = rl_category)) +
  geom_errorbar(aes(xmin = e_lwr, xmax = e_upr), width = 0) +
  scale_x_log10(breaks = 2^(-3:3), labels = 2^(-3:3)) +
  scale_y_log10(breaks = 2^(-2:2), labels = 2^(-2:2)) +
  scale_color_gradientn(
    colours = scico(256, palette = "lipari"),
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
  labs(x = "Mean exposure\u00b3 (rescaled, log scale)",
       y = "Sensitivity\u00b2 (rescaled, log scale)") +
  theme_classic() +
  theme(
    axis.title       = element_text(size = 8),
    axis.text        = element_text(size = 7),
    axis.title.x    = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold"),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height= unit(0.3, "cm"),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7)
  ) 
vuln_scatter


ggsave(here::here("paper/vulnerability_scatter.png"), plot = vuln_scatter, width = 190, height = 120, units = "mm", dpi = 500)


n_seg <- 30  # segments per bar; more = smoother gradient

d_segments <- d %>%
  rowwise() %>%
  mutate(
    x_seq = list(seq(e_lwr, e_upr, length.out = n_seg)),
    # recompute ESS at each x, holding sensitivity/status fixed at their point estimates
    ess_seq = list(x_seq * sensitivity * status)
  ) %>%
  ungroup() %>%
  select(alpha_code, y, x_seq, ess_seq) %>%
  unnest(c(x_seq, ess_seq)) %>%
  group_by(alpha_code) %>%
  mutate(
    x_end   = lead(x_seq),
    log2_ess_seg = log2((ess_seq + lead(ess_seq)) / 2)   # midpoint ESS of each mini-segment
  ) %>%
  filter(!is.na(x_end)) %>%
  ungroup()

vuln_scatter <- ggplot(d, aes(x = x, y = y)) +
  geom_segment(
    data = d_segments,
    aes(x = x_seq, xend = x_end, y = y, yend = y, color = log2_ess_seg),
   linewidth = 0.4, alpha = 0.5,
  ) +
  geom_point(aes(color = log2_ess, size = rl_category), stroke = 0) +
  scale_x_log10(breaks = 2^(-3:3), labels = 2^(-3:3)) +
  scale_y_log10(breaks = 2^(-2:2), labels = 2^(-2:2)) +
  scale_color_gradientn(
    colours = scico(256, palette = "lipari"),
    limits  = c(-6, 6),
    breaks  = seq(-6, 6, 2),
    labels  = c("0.0156", "0.0625", "0.25", "1", "4", "16", "64"),
    name    = "Vulnerability"
  ) +
  scale_size_manual(
    values = c(LC = 3, NT = 4, VU = 5, EN = 6),
    name    = "IUCN status",
    drop    = FALSE
  ) +
  labs(x = "Mean exposure\u00b3 (rescaled, log scale)",
       y = "Sensitivity\u00b2 (rescaled, log scale)") +
  theme_classic() +
  theme(
    axis.title       = element_text(size = 8),
    axis.text        = element_text(size = 7),
    axis.title.x     = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold"),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height= unit(0.3, "cm"),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7)
  )

vuln_scatter

ggsave(here::here("paper/vulnerability_scatter.png"), plot = vuln_scatter, width = 210, height = 170, units = "mm", dpi = 500)

