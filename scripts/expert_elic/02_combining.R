
# Part 1: Load Packages -------------------------------------------------------
packages<- c("sf", "terra", "dplyr", "tidyverse")

pacman::p_load(packages, character.only = TRUE); rm(packages)

ex_sums <- read_csv(here::here("data/processed_data/unk_HML.csv"))
orig_data <- read_csv(here::here("data/processed_data/cleaned_data.csv"))

ex_sums <- ex_sums %>% 
  mutate(
    alpha_code = case_when(
      species == "Hawaiian Petrel" ~ "HAPE",
      species == "Short-tailed Albatross" ~ "STAL",
      species == "Townsend's Storm-Petrel" ~ "TOSP"
      )
  ) %>% 
  rename_with(
    ~ paste0("prop_", .),
    .cols = -c(species, estimate, alpha_code)
  )

#join the other df with this one

joined_df <- ex_sums %>%
  left_join(
    orig_data %>% filter(alpha_code %in% unique(ex_sums$alpha_code)),
    by = "alpha_code"
  )


#my brain shut down on this - currently trying to join the dataframes together but hit a wall. Pick up here next time - still need to get them to join neatly into one dataframe that I can then stick in the app folder and try the shiny app on.  