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



result <- readRDS(here::here("paper/321priority_ranks_1000_for_plots.rds"))

# All unique species
all_species <- unique(result$common_name)

topsps <- c("Pink-footed Shearwater", "Cassin's Auklet", "Guadalupe Murrelet", "Ashy Storm-Petrel", "Marbled Murrelet", "Townsend's Storm-Petrel", "Craveri's Murrelet", "Buller's Shearwater", "Scripps's Murrelet", "Red Phalarope", "Short-tailed Albatross", "Red-necked Phalarope","Hawaiian Petrel", "Bonaparte's Gull",  "Sabine's Gull", "Black Scoter", "Northern Fulmar", "South Polar Skua", "Pomarine Jaeger", "Rhinoceros Auklet")
spcolors <- c("#B06264","#041A2F", "#082743", "#5C5D78", "#0B2D4C","#3E5578", "#062038", "#755F72", "#1E4368", "#D46B5E","#C2655F", "#E98466","#89606E",      "#E6C399", "#F4E4C3", "#E89B74", "#97606A", "#E6A77D", "#E5B58A", "#284A6F" )


# Create a tibble of priority species and colors
priority_colors <- tibble(common_name = topsps, color = spcolors)

# Create a tibble with all species, default color is "grey40"
all_species_colors <- tibble(common_name = all_species) %>%
  left_join(priority_colors, by = "common_name") %>%
  mutate(color = if_else(is.na(color), "grey40", color))

# Check the result
print(all_species_colors)

result_colored <- result %>%
  left_join(all_species_colors, by = "common_name")


#set up for plots
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

# colors_full <- scico::scico(n = 19, palette = "lipari")
# colors_clip <- colors_full[3:18]


#ridge plot
ggplot(foo, aes(x = pri_rank, y = common_name, fill = color)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_identity() +  
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Species",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
        )


ggsave(here::here("paper/2_1_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)



#stacked histogram
colors_match <- c("#13385A","#465779", "#5A5C79","#E57A61", "#EA906D", "#E7A279","#E5B488", "#E8C89E", "#F1DDBA" )
p <- foo %>% 
  filter(pri_rank <= 5) %>% 
  ggplot(aes(x = pri_rank, fill = common_name)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = colors_match) +
  theme_bw() +
  theme(legend.position = "bottom")
p
plotly::ggplotly(p)





# ESS multiplication figure
source(here::here("R/priority.R"))
foo <- calc_priority(e, se, st, w = c(3, 2, 1))
foo_long <- foo %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")

sp_keep <- foo_long %>% 
  filter(region == "all", Priority == "ess") %>% 
  arrange(desc(upr)) %>% 
  slice(1:5)

p <- foo_long %>% 
  filter(region == "all") %>% 
  semi_join(sp_keep, by = "alpha_code") %>% 
  ggplot(aes(x = Priority, y = mean, group = alpha_code)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = alpha_code),
              alpha = 0.2) +
  geom_line(aes(color = alpha_code)) + 
  theme_bw() + 
  theme(legend.position = "none")
plotly::ggplotly(p)







lipari <-  c("#021326", , ,  "#103557", "#163C5F", ,"#284A6F", "#334F74", , "#47587A", "#4F5B7A", "#565B7A", "#5C5D78", "#635D77","#695D75", "#6E5E74", , "#7B5F71", "#825F6F", "#89606E", "#90616C", ,, ,  , , "#CA675E", , "#DB705F", "#E1765F", "#E67D62",, "#EA8D6B", "#E9946F",   ,, , "#E6BC91", , "#E9CBA2", "#EDD3AD", "#F0DBB7", , "#F8ECCE", "#FDF4D9")