#this is the script where we decided to rescale everything from 0.5-2 and then just use exponents to weight it down the line


library(tidyverse)

risk_trace <- expand_grid(
  e = c(0.1, 0.5, 0.9), #this is three values representing a low, mid, and high exposure
  s = c(2/3, 1, 3/2), #these are a low, mid, and high sensitivity
  t = 10^seq(-0.25, 0.25, length.out = 5) #this is setting up some numbers for threat 
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

# Additive alternative
risk_trace_add <- expand_grid(
  e = c(0.1, 0.5, 0.9), #this is three values representing a low, mid, and high exposure
  s = c(-0.5, 0, 0.5), #these are a low, mid, and high sensitivity
  t = c(-0.5, 0, 0.5) #this is setting up some numbers for threat 
) %>% 
  mutate(i = seq_along(e),
         es = e + s,
         est = e + s + t,
         bin = cut(est, 4)) %>% 
  relocate(i)

risk_trace_add %>% 
  mutate(origin = factor(e)) %>% 
  pivot_longer(c(e, es, est), names_to = "x", values_to = "y") %>% 
  ggplot(aes(x, y, group = i, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  theme_classic()

# Multiplicative, all 0-1
risk_trace_01 <- expand_grid(
  e = c(0.1, 0.5, 0.9), #this is three values representing a low, mid, and high exposure
  s = c(0.1, 0.5, 0.9), #these are a low, mid, and high sensitivity
  t = c(0.1, 0.5, 0.9) #this is setting up some numbers for threat 
) %>% 
  mutate(i = seq_along(e),
         es = e * s,
         est = e * s * t,
         bin = cut(est, 4)) %>% 
  relocate(i)

risk_trace_01 %>% 
  mutate(origin = factor(e)) %>% 
  pivot_longer(c(e, es, est), names_to = "x", values_to = "y") %>% 
  ggplot(aes(x, y, group = i, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  theme_classic()


# Multiplicative, with weighting
risk_trace_wgt <- expand_grid(
  e = c(0.1, 0.5, 0.9), #this is three values representing a low, mid, and high exposure
  s = c(0.1, 0.5, 0.9), #these are a low, mid, and high sensitivity
  t = c(0.1, 0.5, 0.9) #this is setting up some numbers for threat 
) %>% 
  mutate(i = seq_along(e),
         es = e^2 * s,
         est = e^2 * s * t,
         bin = cut(est, 4)) %>% 
  relocate(i)

risk_trace_wgt %>% 
  mutate(origin = factor(e)) %>% 
  pivot_longer(c(e, es, est), names_to = "x", values_to = "y") %>% 
  ggplot(aes(x, y, group = i, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  theme_classic()

# Multiplicative, weighted, centered on 1
status_mult <- 4^(1/4)
risk_trace_go_up <- expand_grid(
  e = c(0.5, 1, 2), #this is three values representing a low, mid, and high exposure
  se = c(0.5, 1, 2), #these are a low, mid, and high sensitivity
  st = 0.5 * status_mult^(0:4) #this is setting up some numbers for threat 
) %>% 
  mutate(i = seq_along(e),
         `e*se` = e^2 * se,
         `e*se*st` = e^2 * se * st,
         bin = cut(`e*se*st`, 4)) %>% 
  relocate(i)

risk_trace_go_up %>% 
  mutate(origin = factor(e)) %>% 
  pivot_longer(c(e, `e*se`, `e*se*st`), names_to = "x", values_to = "y") %>% 
  ggplot(aes(x, y, group = i, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  theme_classic()
