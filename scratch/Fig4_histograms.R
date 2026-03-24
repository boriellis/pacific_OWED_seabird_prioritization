# Part 1: Load Packages -------------------------------------------------------
packages<- c("tidyverse", "sf", "terra", "dplyr", "tidyterra")

pacman::p_load(packages, character.only = TRUE); rm(packages)

exposure <- readRDS(here::here("output/cleaned_exposure_1000sims.rds"))





PFSH_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "PFSH") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_histogram(binwidth = 0.00005, fill = "#AE6366") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()

ggsave(here::here("paper/fig4/PFSH_hist_raw.png"), plot = PFSH_hist_raw, width = 10, height = 8, units = "in", dpi = 300)



MAMU_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "MAMU") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_histogram(binwidth = 0.00005, fill = "#25486D") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()


BRAC_hist_raw <- exposure %>%
  filter(region == "CA", alpha_code == "BRAC") %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm)) +
  geom_histogram(binwidth = 0.00005, fill = "#021326") +
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()





PFSH_hist_rescaled <- exposure %>%
  filter(region == "CA", alpha_code == "PFSH") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_histogram(binwidth = 0.005, fill = "#AE6366") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()



MAMU_hist_rescaled <- exposure %>%
  filter(region == "CA", alpha_code == "MAMU") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_histogram(binwidth = 0.005, fill = "#25486D") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()


BRAC_hist_rescaled  <- exposure %>%
  filter(region == "CA", alpha_code == "BRAC") %>%
  unnest(scaled_overlap) %>%
  ggplot(aes(x = scaled_overlap)) +
  geom_histogram(binwidth = 0.005, fill = "#021326") +
  coord_cartesian(xlim = c(0.5, 2), ylim = c(0,800)) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()





exposure %>%
  filter(region == "CA", alpha_code %in% c("PFSH", "MAMU", "BRAC")) %>%
  unnest(outliers_rm) %>%
  ggplot(aes(x = outliers_rm, fill = alpha_code)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  facet_wrap(~ alpha_code, ncol = 1) +   # <-- 3 rows stacked vertically
  coord_cartesian(xlim = c(0, 0.007), ylim = c(0, 300)) +
  scale_fill_manual(values = c(
    "PFSH" = "#AE6366",
    "MAMU" = "#25486D",
    "BRAC" = "#021326"
  )) +
  labs(
    x = "Proportion of regional density that overlaps with WEAs",
    y = NULL
  ) +
  theme_test()
