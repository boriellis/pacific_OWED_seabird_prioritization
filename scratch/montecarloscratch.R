library(here)
library(tidyverse)

# All data
alldat <- read_csv(here("data/processed_data/all_cleaned_data.csv"))

# Rescaled data frame
threat_ratio <- 1.22
lookup <- tibble(
  iucn_status = c("LC", "NT", "VU", "EN", "CR"),
  iucn_value = threat_ratio^c(-1:3)
)
rescale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
rescaled_dat <- alldat %>% 
  filter(!is.na(exposure_model)) %>% 
  left_join(lookup, by = "iucn_status") %>%
  mutate(
    rescaled_DV = rescale_01(DV) + 0.0001,
    rescaled_CV = rescale_01(CV) + 0.0001,
    summed_sens = (rescaled_DV/2 + rescaled_CV/2), 
    highest_sens = pmax(rescaled_CV, rescaled_DV),
    highest_sens_source = case_when(
      rescaled_DV > rescaled_CV ~ "DV",
      rescaled_CV > rescaled_DV ~ "CV",
      TRUE ~ "tie"
    ),
    # Dynamically select the exposure based on user input from the dropdown
    selected_exposure = propALL,
    #rescale exposure column to 1 for ease of presentation (optional)
    rescaled_exposure = rescale_01(selected_exposure) + 0.0001, 
    # Dynamically select the column based on user input from the dropdown
    selected_sensitivity = summed_sens,
    
    # Use the dynamically selected column for sensitivity calculation
    rescaled_sensitivity = 0.618 + (selected_sensitivity - min(selected_sensitivity)) * 
      (1 / 0.618 - 0.618) / 
      (max(selected_sensitivity) - min(selected_sensitivity))
  )

# Prioritization data frame
prioritizationdf <- rescaled_dat %>%
  mutate(
    es = rescaled_exposure * rescaled_sensitivity,
    est = rescaled_exposure * rescaled_sensitivity * iucn_value,
    bin = cut(est, 4, labels = c("Low", "Moderate", "High", "Extreme"))
  )

###########################################################################
#####################                           ###########################
#####################   BEGIN THE MONTE CARLO   ###########################
#####################                           ###########################
###########################################################################

# This function randomly bins species ONCE
# x = priority scores [0, 1]
priority_mc <- function(x) {
  bkpts <- sort(runif(3))
  bkpts <- c(-Inf, bkpts, Inf)
  cut(x, bkpts, labels = FALSE)
}

foo <- replicate(1e3, priority_mc(prioritizationdf$est))
prioritizationdf$bin_mean = rowMeans(foo)
prioritizationdf$bin_lwr = apply(foo, 1, \(x) quantile(x, 0.025))
prioritizationdf$bin_upr = apply(foo, 1, \(x) quantile(x, 0.975))

# Hey this doesn't tell us anything
ggplot(prioritizationdf, aes(est, bin_mean, ymin = bin_lwr, ymax = bin_upr)) +
  geom_pointrange()
