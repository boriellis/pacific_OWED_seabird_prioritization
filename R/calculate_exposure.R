#these are the functions in the exposure branch of the work flow


# FUNCTION TO CLEAN RAW EXPERT QUALTRICS WEIGHTS --------------------------


#this function takes the raw data from qualtrics and cleans it into a longform dataframe that can be used in the calculate_exposure function

clean_exweights <- function(csv_file_path) {
  # Define rare species codes lookup
  rare_codes <- tibble(common_name = c("Short-tailed Albatross", "Townsend's Storm-Petrel", "Hawaiian Petrel"), 
                       alpha_code = c("STAL", "TOSP", "HAPE"))
  
  # Define model names to replace with
  model_names <- c("SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", "MAMU", "PIGU", 
                   "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", "BOGU", "HEEG", "WEGU-WGWH-GWGU", 
                   "CAGU", "HERG-ICGU", "CATE", "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", 
                   "LOON", "LAAL", "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", 
                   "BULS", "STTS-SOSH-FFSH", "BVSH", "BRAC", "PECO", "DCCO", "BRPE")
  
  # Read header from row 2 (skip 1, read 1 row)
  header <- read_csv(csv_file_path, skip = 1, n_max = 1, show_col_types = FALSE)
  
  # Read the actual data starting from row 4, using proper column names
  raw_dataframe <- read_csv(csv_file_path,
                            skip = 3,
                            col_names = colnames(header),
                            show_col_types = FALSE)
  
  # Clean the data
  cleaned_weights <- raw_dataframe %>% 
    mutate(expert = row_number()) %>% 
    slice(-9, -17, -18) %>%  # Remove incomplete submissions
    select(expert,
           starts_with("Short-tailed Albatross"),
           starts_with("Townsend's Storm-Petrel"),
           starts_with("Hawaiian Petrel")) %>% 
    pivot_longer(-expert, 
                 names_to = c("species", "model"),
                 names_sep = " - ",
                 values_to = "weight") %>% 
    mutate(weight = weight / 100) %>%  # Convert to percentages
    left_join(rare_codes, by = c(species = "common_name"))
  
  # Add model names
  cleaned_weights$model_name <- rep(model_names, nrow(cleaned_weights) / length(model_names))
  
  return(cleaned_weights)
}





# DISTRIBUTION MC FUNCTION & SUB FUNCTIONS  -------------------------------



distribution_mc <- function(n_sims, densityrasts, cvrasts, weas, region, exweights) {
  # Use Monte Carlo to incorporate uncertainty at the seasonal level
  seasonal_density_mc <- run_dist_mc(n_sims, densityrasts, cvrasts)
  
  # Combine seasonal densities per species 
  annual_density_mc <- combine_seasons(seasonal_density_mc)
  
  #make raster stacks of elicited species based on expert weights
  elicited_spp <- weight_maps_by_exp(exweights, annual_density_mc)
  
  #add the two stacks together
  all_annual_rasts <- c(annual_density_mc, elicited_spp)
  
}


run_dist_mc <- function(n_sims, densityrasts, cvrasts) {
  species_season_mc <- map(1:nlyr(densityrasts), \(i) {
    mu <- values(densityrasts[[i]])
    cv <- values(cvrasts[[i]])  
    sd <- mu * cv
    by_sp_season <- map(1:n_sims, \(j) {
      result <- densityrasts[[i]]
      values(result) <- suppressWarnings(
        rlnorm(length(mu), 
               meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
               sdlog = sqrt(log(1 + cv^2)))
      )
      # Add simulation number to the layer name
      names(result) <- paste0(names(densityrasts[[i]]), "_", j)
      result
    }) %>% 
      rast()
  })
  return(rast(species_season_mc)) 
}



combine_seasons <- function(x) {
  layer_names <- names(x)
  species_sim_info <- str_extract(layer_names, "^[^_]+")
  numsims <- max(as.numeric(str_extract(layer_names, "\\d+$")))
  unique_species <- unique(species_sim_info)
  # For each species, sum across seasons for each simulation
  annual_rasters <- map(unique_species, \(sp) {
    # Group by simulation (using the detected max)
    sim_rasters <- map(1:numsims, \(sim_num) {
      pattern <- paste0("^", sp, "_.+_", sim_num, "$")
      matching_layers <- which(str_detect(layer_names, pattern))
      if (length(matching_layers) > 0) {
        sum(x[[matching_layers]])   # Sum all seasonal layers for this species and simulation
      }
    }) %>% 
      rast()  # Stack the simulation layers
    names(sim_rasters) <- paste0(sp, "_annual_sim_", 1:numsims)
    return(sim_rasters)
  })
  return(rast(annual_rasters))
}




#MAKE RASTER STACK OF EXPERT ELICITED SPP, 1 PER EXPERT PER SPECIES PER SIMULATION
weight_maps_by_exp <- function(n_sims, r, w){ 
  elicited_rasters <- cross_join(w, tibble(sim = 1:n_sims)) %>% 
    group_by(expert, alpha_code, sim) %>% 
    summarize(density = list(weighted.mean2(r, weight, model_name, sim)), 
              .groups = "drop") 
  result <- rast(elicited_rasters$density)
  names(result) <- str_glue("{elicited_rasters$alpha_code}_annual_sim_{elicited_rasters$sim}_expert_{elicited_rasters$expert}")
  return(result)
}

#this function works within the above
weighted.mean2 <- function(r, w, m, i) { 
  sim_names <- str_glue("{m}_annual_sim_{i}") 
  r2 <- r[[sim_names]]
  r2_nonmissing <- r2[[w > 0]] #this makes a new stacked raster with only the rasters that have nonzero weights
  terra::weighted.mean(r2_nonmissing, w[w > 0]) #for the nonzero raster layers, sum together the layers according to their expert weights
}  

