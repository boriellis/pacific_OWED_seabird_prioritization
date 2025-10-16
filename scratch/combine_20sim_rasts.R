#this is the (ugly) rough outline of how to do this if I was just combining two, but I'm not sure how to make it work with all 50 without giving R a small meltdown. Let's ask max



library(terra)
library(tidyverse)


#This chunk of code fixes the names on the simulations so that when I combine them using the command line each has a unique simulation number from 1-1000

dist_rast_paths <- dir("/Volumes/seagate", pattern = "rasts_.*\\.tif$", full.names = TRUE)

# Just fix names in each file separately
walk(dist_rast_paths, \(path) {
  print(path)
  r <- rast(path)
  rast_idx <- as.integer(str_extract(path, "rasts_([0-9]+).tif", 1))
  sim_offset <- (rast_idx - 1) * 20
  
  for (i in 1:20) {
    names(r) <- str_replace(names(r), str_glue("sim_{i}$"), str_glue("sim_{i + sim_offset}"))
    names(r) <- str_replace(names(r), str_glue("sim_{i}_"), str_glue("sim_{i + sim_offset}_"))
  }
  
  # Write to temporary file
  temp_path <- str_replace(path, "\\.tif$", "_temp.tif")
  writeRaster(r, temp_path, overwrite = TRUE)
  
  # Clean up memory
  rm(r)
  gc()
  
  # Replace original with renamed version
  file.rename(temp_path, path)
})

#convert them all to NetCDF files to help with the memory issue?
walk(dist_rast_paths, \(path) {
  print(path)
  r <- rast(path)
  nc_path <- str_replace(path, "\\.tif$", ".nc")
  writeRaster(r, nc_path, overwrite = TRUE, filetype = "netCDF")
  rm(r)
  gc()
})




#this is all old code, delete if not needed:

#max's code to rename and combine everything within R - issue was, the writeRaster command couldn't handle such a massive file

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

test_50 <- rast("/Volumes/seagate/20_distribution_rasts_50.tif")
names(test_50)


writeRaster(dist_rasts, "/Volumes/seagate/1000_distribution_rasts.nc")


