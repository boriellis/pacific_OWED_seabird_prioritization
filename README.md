# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current 

This repository contains the data and code necessary to produce the exposure data for seabird species in the wind energy planning areas off of the California and Oregon coasts. The repository is currently in progress, and this document will be updated frequently.

## Directory Information + Metadata

#### Folder "data" houses the following files

The **Data** folder contains shapefiles of BOEM wind lease outlines and planning areas, accessed on August 9, 2024 from the [BOEM website](https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data). These files were last updated on April 29, 2024, for the planning areas, and June 6, 2024, for the lease areas.

It also contains a subfolder, "**densities**", that the .tif files of seabird densities in the Pacific Outer Continental Shelf region by season produced by Leirness et al for NOAA NCCOS in 2021. We only use the density data without the paired coefficient of variation maps, so only these are uploaded here, however, the complete data set can be accessed [here](https://coastalscience.noaa.gov/project/marine-bird-distributions-pacific-outer-continental-shelf/).

#### Folder "scripts" houses the following files

**density_script.R** imports and selects the outlines of the five California lease areas and the two Oregon call areas as vectors, and then uses a loop to combine the seasonal rasters of density by species into a single raster of summed annual density for each species and to use the `extract` function to calculate the density of predicted birds within each wind area and print it into a dataframe along with the total summed density for the region overall.

**risktrace_example_script.R** is currently in progress - it will be used to test different approaches to combine the values for exposure, sensitivity, and threat down the line.
