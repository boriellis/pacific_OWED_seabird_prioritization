library(tidyverse)
source(here::here("R/priority.R"))

exposure <- read_rds(here::here("output/exposure_20sims.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_sum.rds"))
status <- read_rds(here::here("output/status.rds"))

# Max's attempt to plot
foo <- calc_priority(exposure, sensitivity, status, w = c(1, 1, 1))
foo_long <- foo %>% 
  rename_with(\(x) paste0(x, "_mean"), c(e, es, ess)) %>% 
  pivot_longer(-c(alpha_code, region),
               names_to = c("Priority", ".value"),
               names_sep = "_")
p <- foo_long %>% 
  filter(region == "all") %>% 
  ggplot(aes(x = Priority, y = mean, group = alpha_code)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = alpha_code),
              alpha = 0.5) +
  geom_line(aes(color = alpha_code)) + 
  theme_bw() + 
  theme(legend.position = "none")
plotly::ggplotly(p)
