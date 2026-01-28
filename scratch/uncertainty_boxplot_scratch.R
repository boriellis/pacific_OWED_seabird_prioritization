#this script is where I'm messing around with trying to get numbers I can use to answer the uncertainty question. 

library(dplyr)
library(ggplot2)
library(forcats)

result <- readRDS(here::here("paper/321priority_ranks_1000_for_plots.rds"))

p_simple <- result %>%
  filter(
    region == "CA",
    pri_rank >= 40,
    pri_rank <= 57
  ) %>%
  mutate(
    pri_rank = factor(pri_rank, levels = 40:57)
  ) %>%
  ggplot(aes(x = pri_rank, fill = common_name)) +
  geom_bar() +
  theme_classic() +
  labs(
    x = "Priority rank",
    y = "Frequency",
    fill = "Species"
  )

p_simple



ggplot(result %>% filter(region == "CA"),
       aes(x = pri_rank, y = fct_reorder(common_name, pri_rank))) +
  geom_density_ridges(scale = 1) +
  labs(x = "Rank", y = "Species")




rank_summary <- result %>%
  filter(region == "CA") %>%
  group_by(common_name) %>%
  summarise(
    min_rank = min(pri_rank),
    q2.5      = quantile(pri_rank, 0.025),
    median   = median(pri_rank),
    q97.5      = quantile(pri_rank, 0.975),
    max_rank = max(pri_rank),
    .groups = "drop"
  )





test2 <-  rank_summary %>%
  arrange(median) %>%
  mutate(
    rank_range = q97.5 - q2.5
  )


test2 %>%
  summarise(
    median_range = median(rank_range),
    p75_range    = quantile(rank_range, 0.75),
    p90_range    = quantile(rank_range, 0.90)
  )


ggplot(test2, aes(rank_range)) + 
  stat_ecdf()
