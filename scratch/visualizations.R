library(tidyverse)
library(ggridges)
library(scico)

e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

w <- c(3, 2, 1)

priority_once <- function(x) {
  e %>% 
    mutate(scaled_overlap = map_dbl(scaled_overlap, \(x) sample(x, 1))) %>% 
    left_join(se, by = "alpha_code") %>% 
    left_join(st, by = "alpha_code") %>% 
    group_by(region) %>% 
    mutate(e = scaled_overlap^w[1],
           es = scaled_overlap^w[1] * sensitivity^w[2],
           ess = scaled_overlap^w[1] * sensitivity^w[2] * status^w[3],
           pri_rank = min_rank(desc(ess))) %>% 
    ungroup() %>% 
    select(region, alpha_code, common_name, pri_rank)
}
result <- map(1:100, priority_once) %>% 
  list_rbind()


#ridge plot
foo <- result %>% 
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

colors_full <- scico::scico(n = 19, palette = "lipari")
colors_clip <- colors_full[3:17]


ggplot(foo, aes(x = pri_rank, y = common_name, fill = common_name)) +
  ggridges::geom_density_ridges(
    stat = "binline", 
    binwidth = 1,
    scale = 4,
    alpha = 0.7,
    color = "grey20"
  ) +
  scale_fill_manual(values = colors_clip)  +   # Reverse so highest factor = brightest

  coord_cartesian(xlim = c(0, 50)) +
  theme_bw() +
  labs(
    x = "Priority Rank (out of 57)",
    y = "Common Name",
    fill = "Species"
  ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8)
        )
ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)



#stacked histogram
p <- foo %>% 
  filter(pri_rank <= 10) %>% 
  ggplot(aes(x = pri_rank, fill = alpha_code)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "bottom")
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


#colors2 <- c("#DFA739", "#DD9B36", "#DB8F32", "#D7802E", "#D9702E", "#D4652A", "#CF5925", "#C34E27", "#BE4629", "#B83E2A", "#A8382A", "#983229", "#882D28", "#782827", "#5C1F1E")
