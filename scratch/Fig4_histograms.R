# Part 1: Load Packages -------------------------------------------------------
packages<- c("tidyverse", "sf", "terra", "dplyr", "tidyterra")

pacman::p_load(packages, character.only = TRUE); rm(packages)

exposure <- readRDS(here::here("output/cleaned_exposure_1000sims.rds"))

exposure_summary <- exposure %>%
  filter(region == "CA") %>%
  mutate(
    mean_outliers_rm = map_dbl(outliers_rm, mean, na.rm = TRUE),
    mean_scaled_overlap = map_dbl(scaled_overlap, mean, na.rm = TRUE)
  )




PFSH_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "PFSH") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_vline(aes(xintercept = mean(outliers_rm)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.00005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  )






ggsave(here::here("paper/fig4/PFSH_hist_raw.png"), plot = PFSH_hist_raw, width = 3, height = 1, units = "in", dpi = 300)



MAMU_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "MAMU") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_vline(aes(xintercept = mean(outliers_rm)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.00005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  ) 


ggsave(here::here("paper/fig4/MAMU_hist_raw.png"), plot = MAMU_hist_raw, width = 3, height = 1, units = "in", dpi = 300)



BRAC_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "BRAC") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_vline(aes(xintercept = mean(outliers_rm)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.00005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  ) 

ggsave(here::here("paper/fig4/BRAC_hist_raw.png"), plot = BRAC_hist_raw,  width = 3, height = 1, units = "in", dpi = 300)









PFSH_hist_rescaled <- exposure %>%
  filter(region == "CA", alpha_code == "PFSH") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_vline(aes(xintercept = mean(scaled_overlap)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  )


ggsave(here::here("paper/fig4/PFSH_hist_rescaled.png"), plot = PFSH_hist_rescaled, width = 3, height = 1, units = "in", dpi = 300)



MAMU_hist_rescaled <- exposure %>%
  filter(region == "CA", alpha_code == "MAMU") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_vline(aes(xintercept = mean(scaled_overlap)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  )

ggsave(here::here("paper/fig4/MAMU_hist_rescaled.png"), plot = MAMU_hist_rescaled, width = 3, height = 1, units = "in", dpi = 300)




BRAC_hist_rescaled <- exposure %>%
  filter(region == "CA", alpha_code == "BRAC") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_vline(aes(xintercept = mean(scaled_overlap)), color = "red", linewidth = 0.3, linetype = "dashed") +
  geom_histogram(binwidth = 0.005, fill = "#a9a9a9") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  scale_y_continuous(n.breaks = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_test() +
  theme(
    axis.text = element_text(size = 12)
  )

ggsave(here::here("paper/fig4/BRAC_hist_rescaled.png"), plot = BRAC_hist_rescaled, width = 3, height = 1, units = "in", dpi = 300)

