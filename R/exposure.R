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



distribution_mc <- function(n_sims, densityrasts, cvrasts, exweights) {
  # Use Monte Carlo to incorporate uncertainty at the seasonal level
  print("run dist mcs")
  seasonal_density_mc <- run_dist_mc(n_sims, densityrasts, cvrasts)
  
  # Combine seasonal densities per species 
  print("combine seasons")
  annual_density_mc <- combine_seasons(seasonal_density_mc)
  
  print("weight maps by expert")
  #make raster stacks of elicited species based on expert weights
  elicited_spp <- weight_maps_by_exp(n_sims, annual_density_mc, exweights)
  
  print("add expert rasts into main raster stack")
  #add the two stacks together
  all_annual_rasts <- c(annual_density_mc, elicited_spp)
  
}


run_dist_mc <- function(n_sims, densityrasts, cvrasts) {
  species_season_mc <- map(1:nlyr(densityrasts), \(i) {
    print(names(densityrasts[[i]]))
    mu <- values(densityrasts[[i]])
    cv <- values(cvrasts[[i]])  
    sd <- mu * cv
    by_sp_season <- map(1:n_sims, \(j) {
      if(j %% 100 == 0) print(j)
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
#' Weight Maps By Expert
#'
#' @param n_sims is the number of sims set in the distribution_mc function
#' @param r 
#' @param w 
#'
#' @returns a raster stack of expert elicited species, 1 per species per expert per simulation
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
#' Title
#'
#' @param r raster stack of distributions
#' @param w expert weight
#' @param m leirness model
#' @param i iteration
weighted.mean2 <- function(r, w, m, i) { 
  sim_names <- str_glue("{m}_annual_sim_{i}") 
  r2 <- r[[sim_names]]
  r2_nonmissing <- r2[[w > 0]] #this makes a new stacked raster with only the rasters that have nonzero weights
  terra::weighted.mean(r2_nonmissing, w[w > 0]) #for the nonzero raster layers, sum together the layers according to their expert weights
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
#' @param d raster stack, 1 per species/group per simulation (and per expert)
#' @param v cleaned wind energy vectors
#' @param sp species information dataframe
#'
#' @returns output should be a dataframe object where each row is a species and
#'   a spatial scale, and each cell contains a list of the distribution of
#'   proportional overlaps for that species and spatial scale that's been
#'   rescaled so the highest value for any possible proportion at that spatial
#'   scale is 2 and the lowest is 0.5. we're going to do leases, states, and
#'   overall region.
calculate_exposure <- function(d, v, sp){
  #identify species for exposure
  exposure_sp <- sp %>% 
    filter(!is.na(exposure_model), 
           regional == "Y") %>% 
    select(alpha_code, exposure_model) %>% 
    rbind(tibble(alpha_code = c("HAPE", "TOSP", "STAL"),
                 exposure_model = c("HAPE", "TOSP", "STAL")))
  
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
