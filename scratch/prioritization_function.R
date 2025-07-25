
owed_prioritization <- function(alpha_code, exposure, sensitivity, status, sens_low, status_ratio) {
  # Rescale exposure 0.0001 - 1.0001
  rescaled_exposure <- (exposure - min(exposure)) / (max(exposure) - min(exposure)) + 0.0001
  
  # Rescale sensitivity
  sens_high <- 1 / sens_low
  rescaled_sensitivity <- sens_low + (sensitivity - min(sensitivity)) * 
    (sens_high - sens_low) / 
    (max(sensitivity) - min(sensitivity))
  
  # Rescale status
  status_num <- c(LC = -1, NT = 0, VU = 1, EN = 2, CR = 3)
  rescaled_status <- status_ratio ^ status_num[status]
  
  # Go forth and multiply
  tibble(alpha_code = alpha_code,
         priority_value = rescaled_exposure * rescaled_sensitivity * rescaled_status,
         priority_rank = rank(priority_value))
}

# What does this look for ALL regions, mean values?
all_dat <- read_csv("data/")