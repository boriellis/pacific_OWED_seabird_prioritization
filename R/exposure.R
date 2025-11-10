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





# DISTRIBUTION MC FUNCTIONS  -------------------------------


#FOR EACH MODEL

#' Make a raster stack of simulations for a given model summed annually
#'
#' @param n_sims is the number of simulations you want (typically should be 1000)
#' @param densityrasts is the raster stack of mean densities
#' @param cvrasts is the raster stack of coefficients of variation
#' @param model is the name of the model you're running this for (i.e., "PAJA-LTJA")
#'
#' @returns a raster stack with layers named model_annual_sim_x
#' 
distribution_mc_1 <- function(n_sims, densityrasts, cvrasts, model) {
  # Use Monte Carlo to incorporate uncertainty at the seasonal level
  print(str_glue("run dist mcs for {model}"))
  seasonal_density_mc <- run_dist_mc(n_sims, densityrasts, cvrasts, model)
  
  # Combine seasonal densities per species 
  print("combine seasons")
  annual_density_mc <- combine_seasons(seasonal_density_mc)
  
  return(annual_density_mc)
}

#' Output a raster stack with n simulations of each season of a given model 
#'
#' @param n_sims is the number of simulations you want (typically should be 1000)
#' @param densityrasts is the raster stack of mean densities
#' @param cvrasts is the raster stack of coefficients of variation
#' @param model is the name of the model to do this for
#'
#' @returns a raster stack with a for each season and simulation for the model in question
#' @export
#'
#' @examples
run_dist_mc <- function(n_sims, densityrasts, cvrasts, model) {
  model_seasons <- str_subset(names(densityrasts), pattern = model)
  model_season_mc <- map(model_seasons, \(l) {
    mu <- values(densityrasts[[l]])
    cv <- values(cvrasts[[paste0(l, "_CV")]])  
    sd <- mu * cv
    by_sp_season <- map(1:n_sims, \(j) {
      if(j %% 100 == 0) print(j)
      result <- densityrasts[[l]]
      values(result) <- suppressWarnings(
        rlnorm(length(mu), 
               meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
               sdlog = sqrt(log(1 + cv^2)))
      )
      # Add simulation number to the layer name
      names(result) <- paste0(names(densityrasts[[l]]), "_", j)
      result
    }) %>% 
      rast()
  })
  return(rast(model_season_mc)) 
}



#' Sum together seasonal rasters into annual raster
#'
#' @param x is the raster stack of seasons and simulation for a model 
#'
#' @returns a raster stack for the model summed annually by simulation (PHAL_sim_1)

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



#FOR EACH ELICITED SPECIES

#' Make a raster stack for each elicited species and expert with n_sims layers by weighting and recombining annual models 
#'
#' @param n_sims number of simulations (typically 1000)
#' @param species alpha code for the elicited species (HAPE, TOSP, or STAL)
#' @param expert the unique identifier for each expert
#' @param dist_path the file path to where the annual raster stacks of simulations for each model are stored
#' @param exweights the df of cleaned expert weights
#'
#' @returns a raster with n_sims layers for a given species and expert combo 
#' @export
#'
#' @examples
distribution_mc_2 <- function(n_sims, species, expert, dist_path, exweights) {
  # Isolate the species of interest
  exweights2 <- filter(exweights, 
                       expert == !!expert, 
                       alpha_code == !!species, 
                       weight > 0)
  
  # Input species and their weights
  input_species <- exweights2$model_name
  species_weights <- exweights2$weight
  
  # Weight and combine all the input species
  input_rasters <- map(
    input_species,
    \(s) rast(dir(dist_path, pattern = s, full.names = TRUE))
  )
  
  # Rename layers
  result <- Reduce(`+`, Map(`*`, input_rasters, species_weights))
  names(result) <- str_glue("{species}_expert{expert}_{1:n_sims}")
  
  return(result)
}









# CLEAN WEAs --------------------------------------------------------------
#takes leases from the one file and calls from the other and pulls out the ones we want, and makes summed versions for state and region
#' Clean WEA polygons
#'
#' @param l leases from Wind_Lease_Outlines
#' @param c calls from Wing_Planning_Areas
#'
#' @returns just the polygons of the OR and CA WEAs, plus summed versions of polygons at the state and regional levels
clean_weas <- function(l, c){
  crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data
  c <- project(c, crs)
  l <- project(l, crs)
  local_weas <- rbind(
    l %>% 
      filter(str_detect(LEASE_NUMB, "OCS-P")) %>% 
      select(name = LEASE_NUMB) %>% 
      mutate(state = "CA", spatial_scale = "lease"),
    c %>% 
      filter(str_detect(ADDITIONAL, "OCS-P")) %>% 
      select(name = ADDITIONAL) %>% 
      mutate(state = "OR", spatial_scale = "lease")
  )
  #state and all-level
  state_weas <- local_weas %>% 
    group_by(state) %>% 
    summarize() %>% 
    mutate(name = state, spatial_scale = "state")
  all_weas <- aggregate(local_weas)
  all_weas$name <- "all"
  all_weas$state <- NA
  all_weas$spatial_scale <- "all"
  weas <- rbind(local_weas, state_weas, all_weas)
}






# CALCULATE AND RESCALE EXPOSURE ------------------------------------------


#' calculate exposure
#'
#' @param modeled_path file path to a folder with a raster stack with n_sims layers for each leirness model
#' @param elicited_path file path to a folder with a raster stack with n_sims layers for each elicited species and expert combo
#' @param v cleaned wind energy vectors
#' @param sp species information dataframe
#'
#' @returns output should be a dataframe object where each row is a species and
#'   a spatial scale, and each cell contains a list of the distribution of
#'   proportional overlaps for that species and spatial scale that's been
#'   rescaled so the highest value for any possible proportion at that spatial
#'   scale is 2 and the lowest is 0.5. we're going to do leases, states, and
#'   overall region.
calculate_exposure <- function(modeled_path, elicited_path, v, sp) {
  # identify species for exposure
  exposure_sp <- sp %>% 
    filter(!is.na(exposure_model), 
           regional == "Y") %>% 
    select(alpha_code, exposure_model) %>% 
    rbind(tibble(alpha_code = c("HAPE", "TOSP", "STAL"),
                 exposure_model = c("HAPE", "TOSP", "STAL")))
  
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


#' Title
#'
#' @param overlap_list 
#'
#' @returns
#' @export
#'
#' @examples
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


# foo <- result %>% 
#   unnest(scaled_overlap) %>% 
#   filter(region == "CA")
# bar <- filter(foo, alpha_code %in% c("HAPE", "TOSP", "STAL"))
# ggplot(foo, aes(scaled_overlap, color = alpha_code)) + 
#   geom_density() + 
#   geom_density(aes(fill = alpha_code), bar, alpha = 0.5) +
#   scale_y_continuous(transform = "log1p") +
#   theme(legend.position = "none")
# ggplot(bar, aes(scaled_overlap, fill = alpha_code)) + 
#   geom_density(alpha = 0.5) +
#   xlim(0, 1)
