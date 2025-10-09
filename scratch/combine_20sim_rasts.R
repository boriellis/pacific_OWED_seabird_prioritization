#this is the (ugly) rough outline of how to do this if I was just combining two, but I'm not sure how to make it work with all 50 without giving R a small meltdown. Let's ask max



library(terra)
library(tidyverse)

dist_rast_paths <- dir(
  "/Volumes/seagate",
  full.names = TRUE
)

# Reduce is like map, but it accumulates instead of running in parallel
dist_rasts <- reduce(
  # Collection to iteratate over (paths to the rasters)
  dist_rast_paths[-1],
  # Initial point for accumulation (i.e., first raster stack)
  .init = rast(dist_rast_paths[1]),
  # Function that accumulates the next item 
  \(running_stack, next_path) {
    print(next_path)
    # Read the next stack
    next_stack <- rast(next_path)
    # Pull raster index from file path
    rast_idx <- as.integer(str_extract(next_path, "rasts_([0-9]+).tif", 1))
    # Offset for simulation indices
    sim_offset <- (rast_idx - 1) * 20
    # Update layer names
    for (i in 1:20) {
      # Handle regular ones
      names(next_stack) <- str_replace(names(next_stack),
                                       str_glue("sim_{i}$"),
                                       str_glue("sim_{i + sim_offset}"))
      # Handle expert ones
      names(next_stack) <- str_replace(names(next_stack),
                                       str_glue("sim_{i}_"),
                                       str_glue("sim_{i + sim_offset}_"))
    }
    # Return corrected stack
    c(running_stack, next_stack)
  }
)

writeRaster(dist_rasts, "/Volumes/seagate/1000_distribution_rasts.nc")


