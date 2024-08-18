###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################

# Part 1: Load Packages -------------------------------------------------------

# Load packages
packages<- c("tidyverse", "here", "dplyr")

pacman::p_load(packages, character.only = TRUE); rm(packages)

# Part 2: Import and Clean Data -----------------------------------------------
alldat <- read_csv(here::here("data/processed_data/cleaned_data.csv"))

#creating a dataframe without species missing spatial data for the sake of testing it 
alldat_trim <- alldat %>% 
  filter(!is.na(exposure_model))


# Part 3: Rescaling values  -----------------------------------------------

#assigning some arbitrary values to threat to start so I have something to work with
# Create a lookup table
lookup <- tibble(
  iucn_status = c("LC", "NT", "VU", "EN", "CR"),
  iucn_value = c(0.5623413, 0.7498942, 1.0000000, 1.3335214, 1.7782794)
)
# Join the lookup table with your data frame
alldat_2 <- alldat_trim %>%
  left_join(lookup, by = "iucn_status")


#rescale other values to between 0 and 1 and get a single sensitivity column:
alldat_rescale <- alldat_2 %>%
  mutate(rescaled_propALL = (propALL - min(propALL)) / (max(propALL) - min(propALL)) +0.0001,
         rescaled_DV = (DV - min(DV)) / (max(DV) - min(DV)) +0.0001,
         rescaled_CV = (CV - min(CV)) / (max(CV) - min(CV)) +0.0001
         ) %>% #rescaling each from 0 to 1 to start, adding the 0.0001 to get rid of pesky zeroes
  mutate(
    highest_sens = pmax(rescaled_CV, rescaled_DV),
    highest_sens_source = case_when( #listing which is the higher value
      rescaled_DV > rescaled_CV ~ "DV",
      rescaled_CV > rescaled_DV ~ "CV",
      TRUE ~ "tie" 
      )# Handle the case when both values are equal 
    ) 
  

#in a stubborn and ugly way, rescale the sensitivity to between 2/3 and 3/2 to match Max's example code
# Define new range
min_new <- 2 / 3
max_new <- 3 / 2

cleandf <- alldat_rescale %>%
  mutate(
    rescaled_sensitivity = min_new + (highest_sens - min(highest_sens)) * (max_new - min_new) / (max(highest_sens) - min(highest_sens)) # Rescale the 'value' column to be between 2/3 and 3/2
  ) %>% 
  select(species = alpha_code,
         exposure = rescaled_propALL,
         sensitivity = rescaled_sensitivity,
         threat = iucn_value)


# Plot it  ----------------------------------------------------------------

prioritizationdf <- cleandf %>% 
  mutate(es = exposure * sensitivity,
         est = exposure * sensitivity * threat,
         bin = cut(est, 4))


prioritizationdf %>% 
  mutate(origin = factor(exposure)) %>% 
  pivot_longer(c(exposure, es, est), names_to = "x", values_to = "y") %>% 
  mutate(
    x = factor(x, levels = c("exposure", "es", "est"))  # Set the order of x-axis categories after pivoting
  ) %>% 
  ggplot(aes(x, y, group = species, color = origin)) +
  geom_line(aes(color = origin)) +
  geom_point(aes(fill = bin), shape = 21, color = "white", size = 8) +
  geom_text(data = . %>% 
              group_by(species) %>% 
              filter(row_number() == n()),  # Get the last row of each group for labeling
            aes(label = species), 
            vjust = -0.5,  # Adjust vertical position
            hjust = 1.2,   # Adjust horizontal position
            size = 3) +   # Text size
  theme_classic()

