## dummy script tryna monte carlo them densities
library(tidyverse)
library(terra)

ncei_dir <- "~/Downloads/0242882/1.1/data/0-data/model_output_predictions"

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
