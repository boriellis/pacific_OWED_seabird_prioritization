library(tidyverse)
library(ggridges)
library(tidyr)
library(ggplot2)
library(scico)

#how to generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all


e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))
# 
# w <- c(1, 1, 2)
# 
# priority_once <- function(x) {
#   e %>%
#     mutate(scaled_overlap = map_dbl(scaled_overlap, \(x) sample(x, 1))) %>%
#     left_join(se, by = "alpha_code") %>%
#     left_join(st, by = "alpha_code") %>%
#     group_by(region) %>%
#     mutate(e = scaled_overlap^w[1],
#            es = scaled_overlap^w[1] * sensitivity^w[2],
#            ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3],
#            pri_rank = min_rank(desc(ess))) %>%
#     ungroup() %>%
#     select(region, alpha_code, common_name, pri_rank)
# }
# result <- map(1:1000, priority_once) %>%
#   list_rbind()
# 
# 
# saveRDS(result, "paper/112priority_ranks_1000_for_plots.rds")

topsps_CA <- c("Pink-footed Shearwater", "Cassin's Auklet", "Red Phalarope", "Red-necked Phalarope", "Ashy Storm-Petrel", "Guadalupe Murrelet", "Pomarine Jaeger",  "Rhinoceros Auklet", "Craveri's Murrelet", "Sooty Shearwater", "Scripps's Murrelet", "Townsend's Storm-Petrel", "Buller's Shearwater", "Sabine's Gull", "South Polar Skua", "Marbled Murrelet", "Bonaparte's Gull", "Black Scoter", "Short-tailed Albatross","Northern Fulmar", "Hawaiian Petrel", "California Gull")


#if you want colors to be ordered by priority
spcolors_pri <- c("#FBE9B1", "#FDE1AB", "#F1CC9B", "#F3BB84", "#E5A67C", "#E9946F", "#E88164", "#DB705F", "#C5655F", "#AE6363", "#996169", "#86606E", "#755F72", "#655E76", "#565B7A", "#405578", "#25486D", "#103657", "#07243E", "#021326", "#FF46A2",  "#FF46A2")

#if you want species colors to be ordered taxonomically:
#spcolors_pri <- c("#0B2932", "#653429", "#FBA894", "#103C49", "#FA9EB0", "#FCB9C6", "#165061", "#9E752E", "#27635F", "#FA9075", "#753D2F", "#9F5240", "#B16847", "#47704F", "#C17B49", "#227D96", "#225855", "#EA995E", "#07243E", "#C49138")



# 3,2,1 -------------------------------------------------------------------
 
result <- readRDS(here::here("paper/321priority_ranks_1000_for_plots.rds"))
# Create a tibble of priority species and colors
all_species <- unique(result$common_name)
all_codes <- unique(result$alpha_code)
priority_colors <- tibble(common_name = topsps, color = spcolors_pri)
all_species_colors <- tibble(common_name = all_species,
                             alpha_code = all_codes) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))
result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")

#join w/ main
foo <- result_colored %>% 
  filter(region == "CA") %>% 
  mutate(common_name = fct_reorder(common_name, pri_rank, .desc = TRUE))
foo_keep <- foo %>%
  group_by(common_name) %>%
#  summarize(keep = any(pri_rank <= 10)) %>%
  filter(keep)

foo <- foo %>%
  semi_join(foo_keep, by = "common_name") %>%
  mutate(
    # Order so top of plot is level 1
    common_name = fct_reorder(common_name, pri_rank, .desc = TRUE)
  )


## RIDGE PLOT ##
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 50)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))
        )


ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

## STACKED HISTOGRAM ##

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

#plot
p <- foo %>%
  filter(pri_rank <= 5) %>%
  ggplot(aes(x = pri_rank, fill = color)) +
  geom_bar(position = "stack") +
  scale_fill_identity(
    guide = "legend",
    breaks = fill_breaks,
    labels = fill_labels
  ) +
  theme_classic() +
  labs(
    x = "Species",
    y = "Frequency",
  ) +
  theme(legend.position = "right") +
  labs(fill = "Species") +
  guides(fill = guide_legend(
    override.aes = list(size = 5) 
  )) +
  theme(
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

p

ggsave(here::here("paper/3_2_1_stackedhist.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


## ESS ##

source(here::here("R/priority.R"))
foo3 <- calc_priority(e, se, st, w = c(3, 2, 1))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    linewidth = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p

ggsave(here::here("paper/3_2_1_ess.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)



# 1,1,1 -------------------------------------------------------------------
result <- readRDS(here::here("paper/111priority_ranks_1000_for_plots.rds"))
# Create a tibble of priority species and colors
all_species <- unique(result$common_name)
all_codes <- unique(result$alpha_code)
priority_colors <- tibble(common_name = topsps, color = spcolors_pri)
all_species_colors <- tibble(common_name = all_species,
                             alpha_code = all_codes) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))
result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")

#join w/ main
foo <- result_colored %>% 
  filter(region == "CA") %>% 
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


## RIDGE PLOT ##
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 37)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


ggsave(here::here("paper/1_1_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

## STACKED HISTOGRAM ##

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

#plot
p <- foo %>%
  filter(pri_rank <= 5) %>%
  ggplot(aes(x = pri_rank, fill = color)) +
  geom_bar(position = "stack") +
  scale_fill_identity(
    guide = "legend",
    breaks = fill_breaks,
    labels = fill_labels
  ) +
  theme_classic() +
  labs(
    x = "Species",
    y = "Frequency",
  ) +
  theme(legend.position = "right") +
  labs(fill = "Species") +
  guides(fill = guide_legend(
    override.aes = list(size = 5) 
  )) +
  theme(
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

p

ggsave(here::here("paper/1_1_1_stackedhist.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


## ESS ##

source(here::here("R/priority.R"))
foo2 <- calc_priority(e, se, st, w = c(1, 1, 1))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    size = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p

ggsave(here::here("paper/1_1_1_ess.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


# 2, 1, 1 -----------------------------------------------------------------
result <- readRDS(here::here("paper/211priority_ranks_1000_for_plots.rds"))
# Create a tibble of priority species and colors
all_species <- unique(result$common_name)
all_codes <- unique(result$alpha_code)
priority_colors <- tibble(common_name = topsps, color = spcolors_pri)
all_species_colors <- tibble(common_name = all_species,
                             alpha_code = all_codes) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))
result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")

#join w/ main
foo <- result_colored %>% 
  filter(region == "CA") %>% 
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


## RIDGE PLOT ##
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 37)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


ggsave(here::here("paper/2_1_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

## STACKED HISTOGRAM ##

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

#plot
p <- foo %>%
  filter(pri_rank <= 5) %>%
  ggplot(aes(x = pri_rank, fill = color)) +
  geom_bar(position = "stack") +
  scale_fill_identity(
    guide = "legend",
    breaks = fill_breaks,
    labels = fill_labels
  ) +
  theme_classic() +
  labs(
    x = "Species",
    y = "Frequency",
  ) +
  theme(legend.position = "right") +
  labs(fill = "Species") +
  guides(fill = guide_legend(
    override.aes = list(size = 5) 
  )) +
  theme(
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

p

ggsave(here::here("paper/2_1_1_stackedhist.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


## ESS ##

source(here::here("R/priority.R"))
foo2 <- calc_priority(e, se, st, w = c(2, 1, 1))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    size = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p

ggsave(here::here("paper/2_1_1_ess.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

# 1, 2, 1 -----------------------------------------------------------------

result <- readRDS(here::here("paper/121priority_ranks_1000_for_plots.rds"))
# Create a tibble of priority species and colors
all_species <- unique(result$common_name)
all_codes <- unique(result$alpha_code)
priority_colors <- tibble(common_name = topsps, color = spcolors_pri)
all_species_colors <- tibble(common_name = all_species,
                             alpha_code = all_codes) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))
result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")

#join w/ main
foo <- result_colored %>% 
  filter(region == "CA") %>% 
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


## RIDGE PLOT ##
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 25)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


ggsave(here::here("paper/1_2_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

## STACKED HISTOGRAM ##

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

#plot
p <- foo %>%
  filter(pri_rank <= 5) %>%
  ggplot(aes(x = pri_rank, fill = color)) +
  geom_bar(position = "stack") +
  scale_fill_identity(
    guide = "legend",
    breaks = fill_breaks,
    labels = fill_labels
  ) +
  theme_classic() +
  labs(
    x = "Species",
    y = "Frequency",
  ) +
  theme(legend.position = "right") +
  labs(fill = "Species") +
  guides(fill = guide_legend(
    override.aes = list(size = 5) 
  )) +
  theme(
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

p

ggsave(here::here("paper/1_2_1_stackedhist.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


## ESS ##

source(here::here("R/priority.R"))
foo2 <- calc_priority(e, se, st, w = c(1, 2, 1))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    size = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p

ggsave(here::here("paper/1_2_1_ess.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)
# 1, 1, 2 -----------------------------------------------------------------
result <- readRDS(here::here("paper/112priority_ranks_1000_for_plots.rds"))
# Create a tibble of priority species and colors
all_species <- unique(result$common_name)
all_codes <- unique(result$alpha_code)
priority_colors <- tibble(common_name = topsps, color = spcolors_pri)
all_species_colors <- tibble(common_name = all_species,
                             alpha_code = all_codes) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))
result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")

#join w/ main
foo <- result_colored %>% 
  filter(region == "CA") %>% 
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


## RIDGE PLOT ##
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 20)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )


ggsave(here::here("paper/1_1_2_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

## STACKED HISTOGRAM ##

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

#plot
p <- foo %>%
  filter(pri_rank <= 5) %>%
  ggplot(aes(x = pri_rank, fill = color)) +
  geom_bar(position = "stack") +
  scale_fill_identity(
    guide = "legend",
    breaks = fill_breaks,
    labels = fill_labels
  ) +
  theme_classic() +
  labs(
    x = "Species",
    y = "Frequency",
  ) +
  theme(legend.position = "right") +
  labs(fill = "Species") +
  guides(fill = guide_legend(
    override.aes = list(size = 5) 
  )) +
  theme(
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  )

p

ggsave(here::here("paper/1_1_2_stackedhist.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


## ESS ##

source(here::here("R/priority.R"))
foo2 <- calc_priority(e, se, st, w = c(1, 1, 2))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    size = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p

ggsave(here::here("paper/1_1_2_ess.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)
 













# taxonomic histogram -----------------------------------------------------

#prep the df 
sp_list <- read_csv("data/raw_data/total_sp_list.csv")
tax <- read_csv("data/raw_data/Clements-v2024-October-2024-rev.csv")
priority_dists <- read_rds("output/priority_scores_1000.rds")

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
  filter(region == "CA") %>%
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
  


ggsave(here::here("paper/test_boxplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)




  # scratch -----------------------------------------------------------------


"Pelecanidae (Pelicans)" = "#001959",
"Phalacrocoracidae (Cormorants and Shags)" = "#0E395E",
"Procellariidae (Shearwaters and Petrels)" = "#165061",
  "Pink-footed Shearwater"
  "Buller's Shearwater"
  "Northern Fulmar"
  "Hawaiian Petrel"
"Hydrobatidae (Northern Storm-Petrels)" = "#27635F",
  "Ashy Storm-Petrel"
  "Townsend's Storm-Petrel"
"Diomedeidae (Albatrosses)" = "#47704F",
  "Short-tailed Albatross"
"Gaviidae (Loons)" = "#6C7B3B",
"Podicipedidae (Grebes)" = "#97882C",
"Laridae (Gulls, Terns, and Skimmers)" = "#C49138",
  "Sabine's Gull"
  "Bonaparte's Gull"
"Alcidae (Auks, Murres, and Puffins)" = "#EA995E",
  "Cassin's Auklet"
  "Guadalupe Murrelet", 
  "Rhinoceros Auklet", 
  "Craveri's Murrelet"
  "Scripps's Murrelet"
  "Marbled Murrelet"
"Stercorariidae (Skuas and Jaegers)" = "#FBA894",
  "South Polar Skua"
  "Pomarine Jaeger"
"Scolopacidae (Sandpipers and Allies)" = "#FCB9C6",
  "Red Phalarope", 
  "Red-necked Phalarope",
"Anatidae (Ducks, Geese, and Waterfowl)" = "#F9CCF9"
  "Black Scoter" "#F9CCF9"

  
  
  "Pink-footed Shearwater" "#0B2932"
  "Buller's Shearwater" "#103C49"
  "Northern Fulmar" "#165061"
  "Hawaiian Petrel" "#227D96"
  "Ashy Storm-Petrel" "#27635F"
  "Townsend's Storm-Petrel" "#225855"
  "Short-tailed Albatross" "#47704F"
  "Sabine's Gull" "#9E752E"
  "Bonaparte's Gull" "#C49138"
  "Cassin's Auklet" "#653429"
  "Guadalupe Murrelet", "#753D2F"
  "Rhinoceros Auklet", "#9F5240"
  "Craveri's Murrelet", "#B16847"
  "Scripps's Murrelet" "#C17B49"
  "Marbled Murrelet" "#EA995E"
  "South Polar Skua" "#FBA894"
  "Pomarine Jaeger" "#FA9075"
  "Red Phalarope", "#FA9EB0"
  "Red-necked Phalarope", "#FCB9C6"
  "Black Scoter" "#F9CCF9"



"#4F2F59"
"#0E395E"
"#27635F"
"#97882C"

[1] "#001959" "#0E395E" "#165061" "#27635F" "#47704F" "#6C7B3B" "#97882C" "#C49138"
[9] "#EA995E" "#FBA894" "#FCB9C6" "#F9CCF9"

"#FFCE66" "#E2A257" "#C67B4A" "#A85940" "#863C38" "#632A3D" "#4F2F59" "#4B4A85"
[9] "#546DAB" "#6191C8" "#70B9E3" "#80E6FF"

[1] "#260C3F" "#3A2959" "#4B4270" "#5E5985" "#79638D" "#96658E" "#B86790" "#D17CA1"
[9] "#DB99BB" "#E2B8D3" "#EAD3E8" "#F0EAF9"

[1] "#351338" "#4B1925" "#5F1F14" "#733303" "#734D00" "#716516" "#687A42" "#628B6E"
[9] "#639E9A" "#86B0C0" "#AEC0DE" "#DAD2FF"

[1] "#9EB0FF" "#66A7E2" "#3788B0" "#245D79" "#153544" "#101519" "#250C00" "#441301"
[9] "#6E2813" "#9F5240" "#CD7E75" "#FFACAC"





#still want to alter blank space on x axis, fix x axis labels, fix formatting on axis labels


# colors_full <- scico::scico(n = 20, palette = "lipari")
# 
# #~taxonomic order color vectors in case I want to use these after all 
# topsps <- c("Pink-footed Shearwater", "Cassin's Auklet", "Guadalupe Murrelet", "Ashy Storm-Petrel", "Marbled Murrelet", "Townsend's Storm-Petrel", "Craveri's Murrelet", "Buller's Shearwater", "Scripps's Murrelet", "Red Phalarope", "Short-tailed Albatross", "Red-necked Phalarope","Hawaiian Petrel", "Bonaparte's Gull",  "Sabine's Gull", "Black Scoter", "Northern Fulmar", "South Polar Skua", "Pomarine Jaeger", "Rhinoceros Auklet")
# spcolors_tax <- c("#B06264","#041A2F", "#082743", "#5C5D78", "#0B2D4C","#3E5578", "#062038", "#755F72", "#1E4368", "#D46B5E","#C2655F", "#E98466","#89606E",      "#E6C399", "#F4E4C3", "#E89B74", "#97606A", "#E6A77D", "#E5B58A", "#284A6F" )
# 
# 




#test


foo2 <- read_rds(here::here("output/priority_scores_1000_112.rds"))



foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")

foo_colored <- foo_colored %>%
  filter(Priority %in% c("e", "es", "ess"))

sp_keep <- foo_colored %>% 
  filter(region == "CA", Priority == "ess") %>% 
  arrange(desc(mean)) %>% 
  slice(1:10)

color_lookup <- sp_keep %>% 
  distinct(common_name, color) %>%
  deframe()  


# Prepare legend info with descending mean
legend_info <- sp_keep %>%
  arrange(desc(mean)) %>% 
  select(common_name, color, mean)

# Named vectors for scales keyed by alpha_code
color_lookup <- legend_info$color
names(color_lookup) <- legend_info$common_name

p <- foo_colored %>%
  filter(region == "CA", common_name %in% legend_info$common_name) %>%
  ggplot(aes(x = Priority, y = mean, group = common_name)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = common_name),
    alpha = 0.2,
    show.legend = FALSE   # hide ribbons from legend
  ) +
  geom_line(
    aes(color = common_name),
    linewidth = 1.2
  ) +
  scale_fill_manual(values = color_lookup) +   # fill ribbons by species
  scale_color_manual(
    values = color_lookup,
    breaks = legend_info$common_name,           # order legend by descending mean
    guide = guide_legend(order = 1)
  ) +
  scale_x_discrete(
    labels = c(
      "e"   = "E",
      "es"  = "E*Se",
      "ess" = "E*Se*St"
    ), 
    expand = c(0, 0)
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Score"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), 
    #    axis.text.x = element_text(angle = 50, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15))
  ) +
  labs(color = "Species")

p



