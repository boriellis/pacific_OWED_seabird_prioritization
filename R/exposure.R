#this script is the function used to calculate exposure. the output should be a df that contains the mean, upper, and lower proportion values for all species in any given region called by the function. 






calculate_exposure <- function(n_sims, densityrasts, cvrasts, weas, region, exweights) {
  # Use Monte Carlo to incorporate uncertainty at the seasonal level
  seasonal_density_mc <- run_dist_mc(n_sims, densityrasts, cvrasts)
  
  # Combine seasonal densities per species 
  annual_density_mc <- combine_seasons(seasonal_density_mc)
  
  #make raster stacks of elicited species based on expert weights
  elicited_spp <- weight_maps_by_exp(exweights, annual_density_mc)
  
  #calculate overlap with given region
    #use region parameter to run terra::extract for appropriate WEAs
    #divide that number by sum of all cells for each raster
  
  #return mean prop and 95% CI
  
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
  return(rast(species_season_mc)) #I'm currently not sure what this is returning and if it's what we want - is it a raster stack as long as #densityrasts * #cvrasts? is that what we want? 
}




combine_seasons <- function(seasonmaps) { #seasonmaps is the output of run_dist_dc 
  layer_names <- names(seasonmaps)
  
  # Extract species and simulation info from layer names
  # Assuming format: "SPECIES_SEASON_predicted_density_SIMULATION" or similar
  species_sim_info <- str_extract(layer_names, "^[^_]+") # Gets species code (everything before first underscore)
  
  # Get unique species
  unique_species <- unique(species_sim_info)
  
  # For each species, sum across seasons for each simulation
  annual_rasters <- map(unique_species, \(sp) {
    # Find all layers for this species
    sp_layers <- which(str_detect(layer_names, paste0("^", sp, "_")))
    
    # Group by simulation (assuming simulations are numbered 1:10)
    sim_rasters <- map(1:10, \(sim_num) {
      # Find layers for this species and simulation
      pattern <- paste0("^", sp, "_.+_", sim_num, "$")
      matching_layers <- which(str_detect(layer_names, pattern))
      
      if (length(matching_layers) > 0) {
        # Sum all seasonal layers for this species and simulation
        sum(raster_stack[[matching_layers]])
      }
    }) %>% 
      rast()  # Stack the 10 simulation layers
    
    # Name the layers
    names(sim_rasters) <- paste0(sp, "_annual_sim_", 1:10)
    return(sim_rasters)
  })
  
  # Combine all species into one big raster stack
  final_stack <- rast(annual_rasters)
  return(final_stack)
}



weight_maps_by_exp <- function(exweights, annualmaps){
  
}

