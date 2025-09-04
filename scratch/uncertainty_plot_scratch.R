e <- read_rds(here::here("output/exposure_20sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

w <- c(1, 1, 1)

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

foo <- result %>% 
  filter(region == "all") %>% 
  mutate(alpha_code = fct_reorder(alpha_code, pri_rank, .desc = TRUE))
foo_keep <- foo %>% 
  group_by(alpha_code) %>% 
  summarize(keep = any(pri_rank <= 10)) %>% 
  filter(keep)

foo %>% 
  semi_join(foo_keep, by = "alpha_code") %>% 
  ggplot(aes(x = pri_rank, y = alpha_code)) + 
  ggridges::geom_density_ridges(stat = "binline", binwidth = 1, scale = 1) + 
  theme_bw()


p <- foo %>% 
  filter(pri_rank <= 5) %>% 
  ggplot(aes(x = pri_rank, fill = alpha_code)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "bottom")
plotly::ggplotly(p)
