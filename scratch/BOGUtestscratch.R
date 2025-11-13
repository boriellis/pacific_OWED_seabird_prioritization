



STAL1000 <- exposure1000 %>%
  filter(region == "CA", alpha_code == "STAL")

summary(STAL1000$raw_overlap[[1]])
summary(STAL1000$scaled_overlap[[1]])


PFSH1000 <- exposure1000 %>%
  filter(region == "CA", alpha_code == "PFSH")

summary(PFSH1000$raw_overlap[[1]])
summary(PFSH1000$scaled_overlap[[1]])

BOGU20 <- exposure20 %>%
  filter(region == "CA", alpha_code == "BOGU")
summary(BOGU20$raw_overlap[[1]])
summary(BOGU20$scaled_overlap[[1]])

BOGU1000 <- exposure1000 %>%
  filter(region == "CA", alpha_code == "BOGU")
summary(BOGU1000$raw_overlap[[1]])
summary(BOGU1000$scaled_overlap[[1]])




exposure1000 %>%
  mutate(max_raw_overlap = map_dbl(raw_overlap, ~max(.x, na.rm = TRUE))) %>%
  select(region, alpha_code, max_raw_overlap) %>% 
  print(n = 60)




calculate_bogu_exposure <- function(modeled_path, elicited_path, v, sp) {
  # identify species for exposure
  d <- rast(modeled_path, pattern = "BOGU", full_names = TRUE)
  
  # Extract WEA overlaps for each species/region
  map(exposure_sp$alpha_code, \(s) {
    message("Processing species: ", s)
    # Pull out distribution raster
    d <- if (s %in% c("HAPE", "TOSP", "STAL")) {
      rast(dir(elicited_path, 
               pattern = s, 
               full.names = TRUE))
    } else {
      model <- exposure_sp$exposure_model[exposure_sp$alpha_code == s]
      rast(dir(modeled_path, 
               pattern = model, 
               full.names = TRUE))
    }
    # Extract by WEA
    extracted_density <- terra::extract(d, v, exact = TRUE, touches = TRUE)
    
    # Proportion overlap
    prop_overlap <- as_tibble(extracted_density) %>%
      mutate(across(-c(ID, fraction), \(x) x * fraction)) %>% 
      group_by(ID) %>% 
      summarize(across(-fraction, sum)) %>% 
      rename(region = ID) %>% 
      mutate(region = v$name)
    density_POCS <- global(d, sum, na.rm = TRUE)$sum
    for (i in 1:length(density_POCS)) {
      prop_overlap[, i + 1] <- prop_overlap[, i + 1] / density_POCS[i]
    }
    
    # Pivot to long format
    pivot_longer(prop_overlap, 
                 -region, 
                 names_to = "simulation", 
                 values_to = "prop_overlap") %>% 
      mutate(alpha_code = s,
             simulation = as.integer(str_extract(simulation, "sim_(.*)", 1)))
  }) %>% 
    list_rbind() %>% 
    # Nest proportion overlaps by region and species
    group_by(region, alpha_code) %>% 
    summarize(raw_overlap = list(prop_overlap),
              .groups = "drop") %>% 
    # Rescale overlaps within each region
    group_by(region) %>%
    mutate(scaled_overlap = list(rescale_overlap(raw_overlap))) %>%
    ungroup()
}





bogu_vals <- calculate_exposure("/Volumes/seagate/bogutest", "/Volumes/seagate/test2", weas, sp)