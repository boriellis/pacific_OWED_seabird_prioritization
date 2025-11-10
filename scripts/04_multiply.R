library(tidyverse)
source(here::here("R/priority.R"))

exposure <- read_rds(here::here("output/exposure_1000sims.rds"))
sensitivity <- read_rds(here::here("output/sensitivity_sum.rds"))
status <- read_rds(here::here("output/status.rds"))

# Table
priority_table <- calc_priority(exposure, 
                                sensitivity, 
                                status, 
                                w = c(3, 2, 1)) %>% 
  left_join(exposure, by = c("alpha_code", "region")) %>% 
  left_join(sensitivity, by = "alpha_code") %>% 
  left_join(status, by = "alpha_code") %>% 
  filter(region == "CA")
  

# Max's attempt to plot
foo <- calc_priority(exposure, sensitivity, status, w = c(3, 2, 1))
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
