###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################

# Part 1: Load Packages -------------------------------------------------------

# Load packages
packages<- c("tidyverse", "here", "dplyr")

pacman::p_load(packages, character.only = TRUE); rm(packages)

# Part 2: Import Data -----------------------------------------------
alldat <- read_csv(here::here("data/processed_data/cleaned_data.csv"))

# Part 3: Rescaling values  -----------------------------------------------

#setting up values for rescaling
#assigning some arbitrary values to threat to start so I have something to work with
lookup <- tibble(
  iucn_status = c("LC", "NT", "VU", "EN", "CR"),
  iucn_value = c(0.5623413, 0.7498942, 1.0000000, 1.3335214, 1.7782794)
)
# Set a range to use for rescaling sensitivity
sens_min_new <- 2 / 3
sens_max_new <- 3 / 2

#create a dataframe with all the relevant numbers rescaled as necessary
rescaled_dat <- alldat %>% 
  #Remove species that don't have spatial data (come back to deal with these)
  filter(!is.na(exposure_model)) %>% 
  left_join(lookup, by = "iucn_status") %>% #join the values for IUCN status from above
  #rescaling CV, DV, and exposure from 0 to 1 to start, adding the 0.0001 to get rid of zeroes
  mutate(rescaled_propALL = (propALL - min(propALL)) / (max(propALL) - min(propALL)) +0.0001,
         rescaled_DV = (DV - min(DV)) / (max(DV) - min(DV)) +0.0001,
         rescaled_CV = (CV - min(CV)) / (max(CV) - min(CV)) +0.0001
  ) %>% 
  # add a single column to contain whichever of the rescaled CV or DV #s is higher, and state which number is used in a second column 
  mutate(
    highest_sens = pmax(rescaled_CV, rescaled_DV),
    highest_sens_source = case_when( #listing which is the higher value
      rescaled_DV > rescaled_CV ~ "DV",
      rescaled_CV > rescaled_DV ~ "CV",
      TRUE ~ "tie" # Handle the case when both values are equal 
    )
  ) %>% 
  #now rescale the sensitivity column to whatever min and max I define above
  mutate(
    rescaled_sensitivity = sens_min_new + (highest_sens - min(highest_sens)) * (sens_max_new - sens_min_new) / (max(highest_sens) - min(highest_sens))
  )


  

# Plot it  ----------------------------------------------------------------
#create columsn of the products at each step
prioritizationdf <- rescaled_dat %>% 
  mutate(es = rescaled_propALL * rescaled_sensitivity,
         est = rescaled_propALL * rescaled_sensitivity * iucn_value,
         bin = cut(est, 4))

prioritizationdf %>% 
  mutate(origin = factor(rescaled_propALL)) %>% 
  pivot_longer(c(rescaled_propALL, es, est), names_to = "x", values_to = "y") %>% 
  mutate(
    x = factor(x, levels = c("rescaled_propALL", "es", "est"))  # Set the order of x-axis categories after pivoting
  ) %>% 
  ggplot(aes(x, y, group = alpha_code, color = origin)) +
  geom_line(aes(color = origin), show.legend = FALSE) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 6) +
  geom_text(data = . %>% 
              group_by(alpha_code) %>% 
              filter(row_number() == n()),  # Get the last row of each group for labeling
            aes(label = alpha_code), 
            vjust = -0.5,  # Adjust vertical position
            hjust = -1,   # Adjust horizontal position
            size = 1.5) +   # Text size
  theme_classic() +
  guides(color = "none") +#gets rid of the legend for the line colors/origin points
  theme(
    legend.key.size = unit(0.1, 'cm'),  # Adjust the size of legend keys
    legend.text = element_text(size = 6)  # Adjust the text size in the legend
  )


