



STAL1000 <- exposure_1000sims %>%
  filter(region == "CA", alpha_code == "STAL")
summary(STAL1000$raw_overlap[[1]])
summary(STAL1000$scaled_overlap[[1]])

HAPE1000 <- exposure_1000sims %>%
  filter(region == "CA", alpha_code == "HAPE")
summary(HAPE1000$raw_overlap[[1]])

PFSH1000 <- exposure_1000sims %>%
  filter(region == "CA", alpha_code == "PFSH")
summary(PFSH1000$raw_overlap[[1]])
summary(PFSH1000$scaled_overlap[[1]])

PFSH20 <- exposure_20sims %>%
  filter(region == "CA", alpha_code == "PFSH")
summary(PFSH20$raw_overlap[[1]])
summary(PFSH20$scaled_overlap[[1]])

BOGU20 <- exposure_20sims %>%
  filter(region == "CA", alpha_code == "BOGU")
summary(BOGU20$raw_overlap[[1]])
summary(BOGU20$scaled_overlap[[1]])

HAPE20 <- exposure_20sims %>%
  filter(region == "CA", alpha_code == "HAPE")
summary(HAPE20$raw_overlap[[1]])

BOGU1000 <- exposure_1000sims %>%
  filter(region == "CA", alpha_code == "BOGU")
summary(BOGU1000$raw_overlap[[1]])
summary(BOGU1000$scaled_overlap[[1]])

quantile(BOGU1000$raw_overlap[[1]], probs = c(0.025, 0.975), na.rm = TRUE)

BOGU1000_outliersrm <- winsorize(BOGU1000$raw_overlap[[1]])

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




bogu_exposure <- function(d, v){

  #extract raw proportion overlap values
  extracted_density <- terra::extract(d, v, exact = TRUE, touches = TRUE)
  
  #normalize by total density 
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
  
  # Associate prop overlaps with species/region info
  result <- cross_join(exposure_sp, select(as_tibble(v), region = name)) %>% 
    mutate(raw_overlap = map2(exposure_model, region, \(em, r) {
      as.numeric(prop_overlap[
        prop_overlap$region == r,
        str_detect(names(prop_overlap), em)
      ])
    })) %>% 
    group_by(region) %>% 
    mutate(scaled_overlap = rescale_overlap(raw_overlap)) %>% 
    ungroup()
}

rescale_overlap <- function(overlap_list) {
  all_overlaps <- unlist(overlap_list)
  min_overlap <- min(all_overlaps)
  max_overlap <- max(all_overlaps)
  log_rescale <- function(x) {
    log_y_rng <- log(c(0.5, 2.0))
    log_y <- log_y_rng[1] + 
      (log_y_rng[2] - log_y_rng[1]) * 
      (x - min_overlap) / (max_overlap - min_overlap)
    y <- exp(log_y)
    return(y)
  }
  map(overlap_list, log_rescale)
}

BOGU_1000 <- rast("/Volumes/seagate/bogutest/BOGU_1000.tiff/")
                     


bogu_vals <- bogu_exposure(BOGU_1000, weas)



library(terra)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

bogu_exposure <- function(d, v) {
  
  # 1. Extract raw overlap values (weighted by polygon fraction)
  extracted_density <- terra::extract(d, v, exact = TRUE, touches = TRUE)
  
  # 2. Normalize by total density across the whole raster stack
  prop_overlap <- as_tibble(extracted_density) %>%
    mutate(across(-c(ID, fraction), \(x) x * fraction)) %>%
    group_by(ID) %>%
    summarize(across(-fraction, sum), .groups = "drop") %>%
    rename(region = ID) %>%
    mutate(region = v$name)
  
  total_density <- global(d, sum, na.rm = TRUE)$sum
  
  for (i in seq_along(total_density)) {
    prop_overlap[, i + 1] <- prop_overlap[, i + 1] / total_density[i]
  }
  
  # 3. Combine all 1000 simulation values into a clean numeric vector per region
  prop_overlap <- prop_overlap %>%
    mutate(
      raw_overlap = pmap(prop_overlap[, -1], \(...) unname(c(...)))  # <-- remove names here
    ) %>%
    select(region, raw_overlap)
  
  # 4. Apply log rescaling to each region’s overlaps
  prop_overlap <- prop_overlap %>%
    mutate(scaled_overlap = rescale_overlap(raw_overlap))
  
  return(prop_overlap)
}

# Log rescaling helper function
rescale_overlap <- function(overlap_list) {
  all_overlaps <- unlist(overlap_list)
  min_overlap <- min(all_overlaps)
  max_overlap <- max(all_overlaps)
  
  log_rescale <- function(x) {
    log_y_rng <- log(c(0.5, 2.0))
    log_y <- log_y_rng[1] +
      (log_y_rng[2] - log_y_rng[1]) *
      (x - min_overlap) / (max_overlap - min_overlap)
    exp(log_y)
  }
  
  map(overlap_list, log_rescale)
}

bogu_vals <- bogu_exposure(BOGU_1000, weas)

BOGUtest <- bogu_vals %>%
  filter(region == "CA")
summary(bogu_vals$raw_overlap[[1]])

