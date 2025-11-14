library(tidyverse)
library(ggridges)

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
    select(region, alpha_code, pri_rank)
}
result <- map(1:100, priority_once) %>% 
  list_rbind()


#ridge plot
foo <- result %>% 
  filter(region == "all") %>% 
  mutate(alpha_code = fct_reorder(alpha_code, pri_rank, .desc = TRUE))
foo_keep <- foo %>%
  group_by(alpha_code) %>%
  summarize(keep = any(pri_rank <= 10)) %>%
  filter(keep)

foo %>% 
 semi_join(foo_keep, by = "alpha_code") %>%
  ggplot(aes(x = pri_rank, y = alpha_code, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    stat = "binline",
    binwidth = 1,
    scale = 1,
    alpha = 0.6,
    color = NA
  ) +
  scale_fill_viridis_c(direction = -1) + 
  coord_cartesian(xlim = c(0, 50)) +  # <-- here is the x limit
  theme_bw()
#edits I'll want - species names, axis labels, color scheme, legend label 



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
