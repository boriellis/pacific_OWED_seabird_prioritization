library(tidyverse)
library(ggridges)
library(scico)

#how to generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all


# e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
# se <- read_rds(here::here("output/sensitivity_sum.rds"))
# st <- read_rds(here::here("output/status.rds"))
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

topsps <- c("Pink-footed Shearwater", "Cassin's Auklet", "South Polar Skua","Buller's Shearwater","Red Phalarope", "Red-necked Phalarope", "Northern Fulmar", "Sabine's Gull", "Ashy Storm-Petrel", "Pomarine Jaeger", "Guadalupe Murrelet", "Rhinoceros Auklet", "Craveri's Murrelet", "Short-tailed Albatross", "Scripps's Murrelet", "Hawaiian Petrel", "Townsend's Storm-Petrel", "Marbled Murrelet", "Bonaparte's Gull", "Black Scoter")

spcolors_pri <- c("#FBE9B1", "#FDE1AB", "#F1CC9B", "#F3BB84", "#E5A67C", "#E9946F", "#E88164", "#DB705F", "#C5655F", "#AE6363", "#996169", "#86606E", "#755F72", "#655E76", "#565B7A", "#405578", "#25486D", "#103657", "#07243E", "#021326")



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
  filter(region == "all") %>% 
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
foo2 <- calc_priority(e, se, st, w = c(3, 2, 1))


foo_long <- foo2 %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
foo_colored <- foo_long %>%
  left_join(all_species_colors, by = "alpha_code")


sp_keep <- foo_colored %>% 
  filter(region == "all", Priority == "ess") %>% 
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
  filter(region == "all", common_name %in% legend_info$common_name) %>%
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
  filter(region == "all") %>% 
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
  filter(region == "all", Priority == "ess") %>% 
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
  filter(region == "all", common_name %in% legend_info$common_name) %>%
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
  filter(region == "all") %>% 
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
  filter(region == "all", Priority == "ess") %>% 
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
  filter(region == "all", common_name %in% legend_info$common_name) %>%
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
  filter(region == "all") %>% 
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
  filter(region == "all", Priority == "ess") %>% 
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
  filter(region == "all", common_name %in% legend_info$common_name) %>%
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
  filter(region == "all") %>% 
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
  filter(region == "all", Priority == "ess") %>% 
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
  filter(region == "all", common_name %in% legend_info$common_name) %>%
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
 











# scratch -----------------------------------------------------------------









#still want to alter blank space on x axis, fix x axis labels, fix formatting on axis labels


# colors_full <- scico::scico(n = 20, palette = "lipari")
# 
# #~taxonomic order color vectors in case I want to use these after all 
# topsps <- c("Pink-footed Shearwater", "Cassin's Auklet", "Guadalupe Murrelet", "Ashy Storm-Petrel", "Marbled Murrelet", "Townsend's Storm-Petrel", "Craveri's Murrelet", "Buller's Shearwater", "Scripps's Murrelet", "Red Phalarope", "Short-tailed Albatross", "Red-necked Phalarope","Hawaiian Petrel", "Bonaparte's Gull",  "Sabine's Gull", "Black Scoter", "Northern Fulmar", "South Polar Skua", "Pomarine Jaeger", "Rhinoceros Auklet")
# spcolors_tax <- c("#B06264","#041A2F", "#082743", "#5C5D78", "#0B2D4C","#3E5578", "#062038", "#755F72", "#1E4368", "#D46B5E","#C2655F", "#E98466","#89606E",      "#E6C399", "#F4E4C3", "#E89B74", "#97606A", "#E6A77D", "#E5B58A", "#284A6F" )
# 
# 





