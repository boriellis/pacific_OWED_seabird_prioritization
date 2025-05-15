
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
    ~ paste0("prop", .),
    .cols = -c(species, estimate, alpha_code)
  )

#make a df to then paste back into the original dataframe

joined_df <- ex_sums %>%
  left_join(
    orig_data %>% filter(alpha_code %in% unique(ex_sums$alpha_code)),
    by = "alpha_code"
  ) %>% 
  select(-ends_with(".y"),
         -prop...1) %>% 
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) %>% 
  mutate(species = paste0(species, " (", estimate, ")"),
         exposure_model = "elicited") %>% 
  select(alpha_code,
         taxonomy,
         species,
         scientific_name,
         exposure_model,
         iucn_status,
         starts_with("prop"),
         CV,
         DV) %>% 
  relocate(propOR, .before = propCA) %>% 
  rename(common_name = species)
  
orig_data <- orig_data %>% 
    select(-...1) 
  
alldat <- bind_rows(orig_data, joined_df)

write.csv(alldat, file = "data/processed_data/all_cleaned_data.csv")
