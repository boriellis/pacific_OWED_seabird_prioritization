## dummy script tryna monte carlo them densities
library(tidyverse)
library(terra)

ncei_dir <- "~/Downloads/0242882/1.1/data/0-data/model_output_predictions"
density_crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data
calls <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp") %>%  #this contains the two oregon areas
  project(density_crs)
leases <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp") %>% 
  project(density_crs)
weas <- rbind(
  leases %>% 
    filter(str_detect(LEASE_NUMB, "OCS-P")) %>% 
    select(name = LEASE_NUMB) %>% 
    mutate(state = "CA"),
  calls %>% 
    filter(str_detect(ADDITIONAL, "OCS-P")) %>% 
    select(name = ADDITIONAL) %>% 
    mutate(state = "OR")
)

# Load up mean and CV of densities
density_mean <- rast(dir(ncei_dir, pattern = ".*density.tif$", full.names = TRUE))
density_cv <- rast(dir(ncei_dir, pattern = ".*density_CV.tif$", full.names = TRUE))

# Monte Carlo ye olde ancient murrellettey
anmu_mean <- density_mean[[1]]
anmu_cv <- density_cv[[1]]

n_sim <- 10
anmu_mc <- map(1:n_sim, \(i) {
  result <- anmu_mean
  mu <- values(anmu_mean)
  cv <- values(anmu_cv)
  sd <- mu * cv
  # This will produce warning messages because of the NAs
  density_vals <- rlnorm(length(mean_vals), 
                         meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
                         sdlog = sqrt(log(1 + cv^2)))
  values(result) <- density_vals
  result
}) %>% 
  rast()
plot(anmu_mc)

# Do it for all them bad babies
n_sim <- 10
density_mc <- map(1:nlyr(density_mean), \(i) {
  mu <- values(density_mean[[i]])
  cv <- values(density_cv[[i]])
  sd <- mu * cv
  by_sp_season <- map(1:n_sim, \(j) {
    result <- density_mean[[i]]
    # This will produce warning messages because of the NAs
    values(result) <- rlnorm(length(mu), 
                             meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
                             sdlog = sqrt(log(1 + cv^2)))
    result
  }) %>% 
    rast()
  names(by_sp_season) <-  paste0(names(density_mean[[i]]), "_", 1:n_sim)
  by_sp_season
})

# Next, we'll do the summing total and summing lease within each monte carlo
# That way you're not saving a billion rasters, but rather a few numbers

# Pink-footed Shearwater (high overlap, multiple seasons, single model)
is_pfsh <- str_detect(names(density_mean), "^PFSH")
pfsh_means <- density_mean[[is_pfsh]]
pfsh_cvs <- density_cv[[is_pfsh]]
n_sim <- 10
pfsh_mc_by_season <- map(1:nlyr(pfsh_means), \(i) {
  mu <- values(pfsh_means[[i]])
  cv <- values(pfsh_cvs[[i]])
  sd <- mu * cv
  by_sp_season <- map(1:n_sim, \(j) {
    result <- pfsh_means[[i]]
    # This will produce warning messages because of the NAs
    values(result) <- suppressWarnings(
      rlnorm(length(mu), 
             meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
             sdlog = sqrt(log(1 + cv^2)))
    )
    result
  }) %>% 
    rast()
  
  # Sum density across POCS
  density_pocs <- global(by_sp_season, 'sum', na.rm = TRUE) %>% 
    mutate(sim = 1:n_sim) %>% 
    rename(density_pocs = "sum")
  
  # Extract and sum densities in all the lease areas
  extract_density <- extract(by_sp_season, weas, exact = TRUE, touches = TRUE) %>% 
    as_tibble(.name_repair = "unique_quiet") %>% 
    group_by(ID) %>% 
    summarize(across(-fraction, ~ sum(.x * fraction))) %>% 
    set_names(c("ID", 1:n_sim)) %>% 
    pivot_longer(-ID, names_to = "sim", values_to = "density_wea") %>% 
    mutate(sim = as.integer(sim),
           wea = weas$name[ID],
           species = str_extract(names(by_sp_season)[1], 
                                 "^([^_]+)_([^_]+)_",
                                 group = 1),
           season = str_extract(names(by_sp_season)[1], 
                                "^([^_]+)_([^_]+)_",
                                group = 2)) %>% 
    left_join(density_pocs, by = "sim")
  
  extract_density
}) %>% 
  list_rbind()

pfsh_mc <- pfsh_mc_by_season %>% 
  group_by(species, wea, sim) %>% 
  summarize(across(c(density_wea, density_pocs), sum),
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)

states <- weas %>% 
  as_tibble() %>% 
  select(wea = name, state)
pfsh_mc_states <- pfsh_mc %>% 
  left_join(states, by = "wea") %>% 
  group_by(sim, state) %>% 
  summarize(density_wea = sum(density_wea), 
            density_pocs = density_pocs[1],
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)
pfsh_mc_all <- pfsh_mc %>% 
  group_by(sim) %>% 
  summarize(density_wea = sum(density_wea), 
            density_pocs = density_pocs[1],
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)

ggplot(pfsh_mc, aes(prop_overlap)) +
  geom_histogram() +
  facet_wrap(~wea, scales = "free")
ggplot(pfsh_mc_states, aes(prop_overlap)) +
  geom_histogram() +
  facet_wrap(~state, scales = "free")
ggplot(pfsh_mc_all, aes(prop_overlap)) +
  geom_histogram()
  
  

# All species, seasons ----------------------------------------------------

n_sim <- 1000
species_season_mc <- map(1:nlyr(density_mean), \(i) {
  mu <- values(density_mean[[i]])
  cv <- values(density_cv[[i]])
  sd <- mu * cv
  by_sp_season <- map(1:n_sim, \(j) {
    result <- density_mean[[i]]
    # This will produce warning messages because of the NAs
    values(result) <- suppressWarnings(
      rlnorm(length(mu), 
             meanlog = log(mu^2 / sqrt(sd^2 + mu^2)),
             sdlog = sqrt(log(1 + cv^2)))
    )
    result
  }) %>% 
    rast()
  
  # Sum density across POCS
  density_pocs <- global(by_sp_season, 'sum', na.rm = TRUE) %>% 
    mutate(sim = 1:n_sim) %>% 
    rename(density_pocs = "sum")
  
  # Extract and sum densities in all the lease areas
  extract_density <- extract(by_sp_season, weas, exact = TRUE, touches = TRUE) %>% 
    as_tibble(.name_repair = "unique_quiet") %>% 
    group_by(ID) %>% 
    summarize(across(-fraction, ~ sum(.x * fraction))) %>% 
    set_names(c("ID", 1:n_sim)) %>% 
    pivot_longer(-ID, names_to = "sim", values_to = "density_wea") %>% 
    mutate(sim = as.integer(sim),
           wea = weas$name[ID],
           species = str_extract(names(by_sp_season)[1], 
                                 "^([^_]+)_([^_]+)_",
                                 group = 1),
           season = str_extract(names(by_sp_season)[1], 
                                "^([^_]+)_([^_]+)_",
                                group = 2)) %>% 
    left_join(density_pocs, by = "sim")
  
  extract_density
}) %>% 
  list_rbind()

exposure_mc <- species_season_mc %>% 
  group_by(species, wea, sim) %>% 
  summarize(across(c(density_wea, density_pocs), sum),
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)

states <- weas %>% 
  as_tibble() %>% 
  select(wea = name, state)
exposure_mc_states <- exposure_mc %>% 
  left_join(states, by = "wea") %>% 
  group_by(sim, state) %>% 
  summarize(density_wea = sum(density_wea), 
            density_pocs = density_pocs[1],
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)
exposure_mc_all <- exposure_mc %>% 
  group_by(sim) %>% 
  summarize(density_wea = sum(density_wea), 
            density_pocs = density_pocs[1],
            .groups = "drop") %>% 
  mutate(prop_overlap = density_wea / density_pocs)








