library(tidyverse)

risk_trace <- expand_grid(
  e = c(0.1, 0.5, 0.9),
  s = c(2/3, 1, 3/2),
  t = 10^seq(-0.25, 0.25, length.out = 5)
) %>% 
  mutate(i = seq_along(e),
         es = e * s,
         est = e * s * t,
         bin = cut(est, 4)) %>% 
  relocate(i)

risk_trace %>% 
  mutate(origin = factor(e)) %>% 
  pivot_longer(c(e, es, est), names_to = "x", values_to = "y") %>% 
  ggplot(aes(x, y, group = i, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  theme_classic()
