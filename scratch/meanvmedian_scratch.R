library(tidyverse)

raw_scores <- read_rds(here::here("output/priority_scores_1000_321.rds"))

anmu_ca <- priority_dists %>%
  filter(alpha_code == "ANMU", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(anmu_ca$ess_dist)
median_val <- median(anmu_ca$ess_dist)

ggplot(anmu_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for ANMU in CA"
  )


hape_ca <- priority_dists %>%
  filter(alpha_code == "HAPE", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(hape_ca$ess_dist)
median_val <- median(hape_ca$ess_dist)

ggplot(hape_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for HAPE in CA"
  )




pfsh_ca <- priority_dists %>%
  filter(alpha_code == "PFSH", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(pfsh_ca$ess_dist)
median_val <- median(pfsh_ca$ess_dist)

ggplot(pfsh_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for pfsh in CA"
  )




bogu_ca <- priority_dists %>%
  filter(alpha_code == "BOGU", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(bogu_ca$ess_dist)
median_val <- median(bogu_ca$ess_dist)

ggplot(bogu_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for BOGU in CA"
  )


stal_ca <- priority_dists %>%
  filter(alpha_code == "STAL", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(stal_ca$ess_dist)
median_val <- median(stal_ca$ess_dist)

ggplot(stal_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for STAL in CA"
  )


tosp_ca <- priority_dists %>%
  filter(alpha_code == "TOSP", region == "CA") %>%
  unnest(ess_dist)

mean_val   <- mean(tosp_ca$ess_dist)
median_val <- median(tosp_ca$ess_dist)

ggplot(tosp_ca, aes(x = ess_dist)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of ESS for TOSP in CA"
  )




#raw exposure
anmu_ca <- priority_dists %>%
  filter(alpha_code == "ANMU", region == "CA") %>%
  unnest(outliers_rm)

mean_val   <- mean(anmu_ca$outliers_rm)
median_val <- median(anmu_ca$outliers_rm)

ggplot(anmu_ca, aes(x = outliers_rm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of raw overlap for ANMU in CA"
  )


hape_ca <- priority_dists %>%
  filter(alpha_code == "HAPE", region == "CA") %>%
  unnest(outliers_rm)

mean_val   <- mean(hape_ca$outliers_rm)
median_val <- median(hape_ca$outliers_rm)

ggplot(hape_ca, aes(x = outliers_rm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of raw overlap for HAPE in CA"
  )

spsk_ca <- priority_dists %>%
  filter(alpha_code == "SPSK", region == "CA") %>%
  unnest(outliers_rm)

mean_val   <- mean(spsk_ca$outliers_rm)
median_val <- median(spsk_ca$outliers_rm)

ggplot(spsk_ca, aes(x = outliers_rm)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of raw overlap for SPSK in CA"
  )



#rescaled exposure?

rescaled_exp <- read_rds(here::here("app/app_data.rds"))

anmu_ca <- rescaled_exp %>%
  filter(alpha_code == "ANMU", region == "CA") %>%
  unnest(scaled_overlap)

mean_val   <- mean(anmu_ca$scaled_overlap)
median_val <- median(anmu_ca$scaled_overlap)

ggplot(anmu_ca, aes(x = scaled_overlap)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of scaled overlap for ANMU in CA"
  )


bogu_ca <- rescaled_exp %>%
  filter(alpha_code == "BOGU", region == "CA") %>%
  unnest(scaled_overlap)

mean_val   <- mean(bogu_ca$scaled_overlap)
median_val <- median(bogu_ca$scaled_overlap)

ggplot(bogu_ca, aes(x = scaled_overlap)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of scaled overlap for BOGU in CA"
  )


hape_ca <- rescaled_exp %>%
  filter(alpha_code == "HAPE", region == "CA") %>%
  unnest(scaled_overlap)

mean_val   <- mean(hape_ca$scaled_overlap)
median_val <- median(hape_ca$scaled_overlap)

ggplot(hape_ca, aes(x = scaled_overlap)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  geom_vline(aes(xintercept = mean_val),   color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median_val), color = "darkorange", linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean_val,   y = Inf, label = paste0("Mean: ",   round(mean_val,   4)),
           color = "firebrick",  hjust = -0.1, vjust = 1.5, size = 4) +
  annotate("text", x = median_val, y = Inf, label = paste0("Median: ", round(median_val, 4)),
           color = "darkorange", hjust = -0.1, vjust = 3.5, size = 4) +
  theme_classic() +
  labs(
    x = "ESS",
    y = "Count",
    title = "Distribution of scaled overlap for HAPE in CA"
  )




