###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current  
#expert elicitation data cleaning scratch script
######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################

# Part 1: Load Packages -------------------------------------------------------
packages<- c("sf", "terra", "dplyr", "tidyverse")

pacman::p_load(packages, character.only = TRUE); rm(packages)

# Part 2: Import data and make parcels  ---------------------------------------

##WIND ENERGY AREAS
calls <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Planning_Area_Outlines_04_29_2024.shp") #this contains the two oregon areas
leases <- vect("data/raw_data/BOEM_shapefiles/BOEM_Wind_Lease_Outlines_06_06_2024.shp") #this contains the five CA leases

#change projection
crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data
calls <- project(calls, crs)
leases <- project(leases, crs)

#trim to individual parcels
#OR
OCSP0566 <- calls[calls$ADDITIONAL == "Oregon PSN - OCS-P 0566"] 
OCSP0567 <- calls[calls$ADDITIONAL == "Oregon PSN - OCS-P 0567"]
#CA
OCSP0561 <- leases[leases$LEASE_NUMB == "OCS-P 0561"] #humboldt 
OCSP0562 <- leases[leases$LEASE_NUMB == "OCS-P 0562"] #humboldt 
OCSP0563 <- leases[leases$LEASE_NUMB == "OCS-P 0563"] #morro 
OCSP0564 <- leases[leases$LEASE_NUMB == "OCS-P 0564"] #morro 
OCSP0565 <- leases[leases$LEASE_NUMB == "OCS-P 0565"] #morro 

# Part 3: Loop to combine seasonal rasters into  annual rasters to use for rest of analysis ---------
#make a vector of just the species codes/groups
birdcodes <- list.files("data/raw_data/densities") #make a list of all the file names
birdcodes <- regmatches(birdcodes, regexpr("[^_]+", birdcodes)) #Getting just the species codes - that operator is saying to extract everything up to the first underscore 
cleancodes <- birdcodes[!duplicated(birdcodes)] #make a list without duplicates

for(i in cleancodes){ #save the annual rasters to a new folder 
  print(i)    #tracker to show progress
  ##load in and combine the seasons for each species
  rastlist <- list.files("data/raw_data/densities", pattern = i, all.files = TRUE, full.names = FALSE) #make a list of the file names for a single species 
  allrasters <- rast(paste("data/raw_data/densities", rastlist, sep = "/")) #load in all the rasters for the species 
  annualrast <- app(allrasters, sum) #sum the seasons to create a total raster
  annualrast <- annualrast / global(annualrast, "max", na.rm = TRUE)[1,1] #rescale from 0-1 so that they can be combined
  writeRaster(annualrast, filename = file.path("data/raw_data/annual_densities", paste0(i, ".tif")), overwrite = TRUE)
} #this worked, I tested it

# Part 4: load in and clean weights from experts --------------------------
raw_exweights_header <- read_csv(here::here("data/raw_data/expert_weights_may12_2025.csv"), 
                                 skip = 1,
                                 n_max = 1) #get the new header line
raw_exweights <- read_csv(here::here("data/raw_data/expert_weights_may12_2025.csv"),
                          skip = 3,
                          col_names = colnames(raw_exweights_header)) %>% #clean up header
  mutate(expert = row_number()) %>% #add an ID for expert
  slice(-9,-17,-18) %>% #cut the three incomplete submissions
  select(expert,
         starts_with("Short-tailed Albatross"),
         starts_with("Townsend's Storm-Petrel"),
         starts_with("Hawaiian Petrel")) %>% #picking only the columns I want - cut out comments, metadata
  pivot_longer(-expert, 
               names_to = c("species", "model"),
               names_sep = " - ",
               values_to = "weight") %>% #pivot longer so that each expert/species/model weight has its own row 
  mutate(weight = weight / 100) #make the weights percentages
model_names <- c("SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", "MAMU", "PIGU", "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", "BOGU", "HEEG", "WEGU-WGWH-GWGU", "CAGU", "HERG-ICGU", "CATE", "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", "LOON", "LAAL", "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", "BULS", "STTS-SOSH-FFSH", "BVSH", "BRAC", "PECO", "DCCO", "BRPE") 
raw_exweights$model_name <- rep(model_names, nrow(raw_exweights) / length(model_names)) #change the model names to match density files


# Part 5: make a new weighted raster per each expert/species combo --------

# Stack all your SDM rasters
allrasters <- rast(file.path("data/raw_data/annual_densities", 
                             paste0(model_names, ".tif")))

#make a function that outputs a new, single-layer raster where each pixel is the weighted average of that pixel across the input layers
weighted.mean2 <- function(r, w) { #input is a stack of rasters and a vector of weights, one for each raster layer
  r_nonmissing <- r[[w > 0]] #this makes a new stacked raster with only the rasters that have nonzero weights
  terra::weighted.mean(r_nonmissing, w[w > 0]) #for the nonzero raster layers, sum together the layers according to their expert weights
} 

#make the new rasters, store them in a dataframe (3 per expert, one for each species)
ex_rasts <- raw_exweights %>% 
  group_by(expert, species) %>% 
  summarize(weighted_sdm = list(weighted.mean2(allrasters, weight)),
            .groups = "drop") #run the function for each combination of expert/species and store the raster in  weighted_sdm column




# Part 6: calculate prop per region per ex/sp combo -----------------------


#load up the seven vectors as one stacked spatvector
library(tidyterra)
lease_areas <- rbind(
  calls %>% 
    filter(str_detect(ADDITIONAL, "Oregon PSN - OCS-P ")) %>% 
    transmute(region = str_remove(ADDITIONAL, ".*Oregon PSN - OCS-P ")), #tidy the names
  leases %>% 
    filter(str_detect(LEASE_NUMB, "OCS-P ")) %>% 
    transmute(region = str_remove(LEASE_NUMB, ".*OCS-P ")) #tidy the names
) 

#make a function that takes a raster and a lease area and returns a total number of birds in that lease area
count_birds_in_lease <- function(r, l) {
  l_vect <- filter(lease_areas, region == l)
  terra::extract(r, l_vect, exact = TRUE, fun = sum, ID = FALSE)[1, 1]
} 

#calculate birds/lease, birds/region, and prop in lease for each lease
ex_sumdens_overlaps_per_lease <- ex_rasts %>% 
  cross_join(as_tibble(lease_areas)) %>% #makes a row for every expert, species, and lease area combo
  mutate(lease_birds = map2_dbl(weighted_sdm, region, count_birds_in_lease), #use count_birds_in_lease on every raster/lease combo
         total_birds = map_dbl(weighted_sdm, \(r) global(r, sum, na.rm = TRUE)[1,1]), #get the total #birds projected across the whole region
         prop_overlap = lease_birds / total_birds) #get the proportion


#ADD THE COMBINED AREAS INTO THE DATAFRAME
# Summarize OR region (0566, 0567)
or_summary <- ex_sumdens_overlaps_per_lease %>%
  filter(region %in% c("0566", "0567")) %>%
  group_by(expert, species) %>%
  summarize(
    region = "OR",
    lease_birds = sum(lease_birds),
    total_birds = first(total_birds),
    prop_overlap = lease_birds / total_birds,
    .groups = "drop"
  )

# Summarize CA region (0561–0565)
ca_summary <- ex_sumdens_overlaps_per_lease %>%
  filter(region %in% c("0561", "0562", "0563", "0564", "0565")) %>%
  group_by(expert, species) %>%
  summarize(
    region = "CA",
    lease_birds = sum(lease_birds),
    total_birds = first(total_birds),
    prop_overlap = lease_birds / total_birds,
    .groups = "drop"
  )

# Summarize ALL region (CA + OR)
all_summary <- bind_rows(or_summary, ca_summary) %>%
  group_by(expert, species) %>%
  summarize(
    region = "ALL",
    lease_birds = sum(lease_birds),
    total_birds = first(total_birds),
    prop_overlap = lease_birds / total_birds,
    .groups = "drop"
  )

# Combine all summaries and original per-lease data
ex_sumdens_overlaps_per_region <- bind_rows(
  ex_sumdens_overlaps_per_lease,
  or_summary,
  ca_summary,
  all_summary
)

# Define the desired region order
region_order <- c("0561", "0562", "0563", "0564", "0565", "0566", "0567", "CA", "OR", "ALL")

# Arrange by expert, species, and ordered region
ex_sumdens_overlaps_per_region <- ex_sumdens_overlaps_per_region %>%
  mutate(region = factor(region, levels = region_order)) %>%
  arrange(expert, species, region)









#code to plot the distributions for all to check it out

ex_sumdens_overlaps_per_region %>%
  filter(species == "Hawaiian Petrel", region == "ALL") %>%
  ggplot(aes(x = prop_overlap)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Proportion Overlap in Region 'ALL' for Hawaiian Petrel",
       x = "Proportion Overlap", y = "Count") +
  theme_minimal()

ex_sumdens_overlaps_per_region %>%
  filter(species == "Short-tailed Albatross", region == "ALL") %>%
  ggplot(aes(x = prop_overlap)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Proportion Overlap in Region 'ALL' for Short-tailed Albatross",
       x = "Proportion Overlap", y = "Count") +
  theme_minimal()

ex_sumdens_overlaps_per_region %>%
  filter(species == "Townsend's Storm-Petrel", region == "ALL") %>%
  ggplot(aes(x = prop_overlap)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Proportion Overlap in Region 'ALL' for Townsend's Storm-Petrel",
       x = "Proportion Overlap", y = "Count") +
  theme_minimal()




rast_lo <- function(r_list, lease_area) {
  r <- rast(r_list)
  mean(r) - 1.96 * stdev(r)
}


rast_hi <- function(r_list) {
  r <- rast(r_list)
  mean(r) + 1.96 * stdev(r)
}


ex_rast_agg <- ex_rasts %>% #this isn't right - makes an overall "low" sdm instead of a number
  group_by(species) %>% 
  summarize(sdm_mean = list(mean(rast(weighted_sdm))),
            sdm_lo = list(rast_lo(weighted_sdm)),
            sdm_hi = list(rast_hi(weighted_sdm)))











###MY OLD CODE
#intial clean
exweights <- raw_exweights %>% 
  select(-(1:17)) %>% 
  slice(-2) %>% 
  mutate(ex_id = if_else(row_number() == 1,
                         "id",
                         paste0("ex_", row_number() -1)))
  
head(exweights)

#make vector to rename model name row to match map names
model_names <- c("id", "SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", "MAMU", "PIGU", "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", "BOGU", "HEEG", "WEGU-WGWH-GWGU", "CAGU", "HERG-ICGU", "CATE", "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", "LOON", "LAAL", "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", "BULS", "STTS-SOSH-FFSH", "BVSH", "BRAC", "PECO", "DCCO", "BRPE")

#make STAL df 
STAL_weights<- exweights %>% 
  select(ex_id, starts_with("Q3_"))
  
model_row <- as_tibble_row(setNames(model_names, names(STAL_weights)))
STAL_weights <- bind_rows(model_row, STAL_weights)
  
STAL_weights<- STAL_weights %>% 
  slice(-2)

#make TOSP df 
TOSP_weights<- exweights %>% 
  select(ex_id, starts_with("Q6_"))

model_row <- as_tibble_row(setNames(model_names, names(TOSP_weights)))
TOSP_weights <- bind_rows(model_row, TOSP_weights)

TOSP_weights<- TOSP_weights %>% 
  slice(-2)
  
#make HAPE df 
HAPE_weights<- exweights %>% 
  select(ex_id, starts_with("Q9_"))

model_row <- as_tibble_row(setNames(model_names, names(HAPE_weights)))
HAPE_weights <- bind_rows(model_row, HAPE_weights)

HAPE_weights<- HAPE_weights %>% 
  slice(-2)

#CONSIDER EXPORTING THESE EVENTUALLY



# doing it manually to test --------------------------------------

#HAPE
for(i in 1:5){
  print(i)
  
  ex_id <- paste0("ex_", i)  # construct the ID you're looking for
  
  # Find the row in HAPE_weights that matches the current ex_id
  row_index <- which(HAPE_weights[[1]] == ex_id)
  
  # Skip if no match is found (just in case)
  if(length(row_index) == 0) next
  
  # Get raster names (model names) from the first row
  model_names <- as.character(HAPE_weights[1, -1])  # skip ID column
  rastlist2 <- paste0(model_names, ".tif")
  
  # Get corresponding weights from the matching row
  rastweight <- as.numeric(HAPE_weights[row_index, -1]) / 100
  
  # Load rasters
  allrasters <- rast(file.path("data/raw_data/annual_densities", rastlist2))
  
  # Apply weights
  weighted_rasters <- allrasters * rastweight
  
  # Sum and rescale
  weighted_raster <- sum(weighted_rasters)
  weighted_raster <- weighted_raster / global(weighted_raster, "max", na.rm = TRUE)[1,1]
  
  # Plot
  plot(weighted_raster)
  
  #save
  assign(paste0("HAPEmap_", i), weighted_raster) 
}

#TOSP
for(i in 1:5){
  print(i)
  
  ex_id <- paste0("ex_", i)  # construct the ID you're looking for
  
  # Find the row in HAPE_weights that matches the current ex_id
  row_index <- which(TOSP_weights[[1]] == ex_id)
  
  # Skip if no match is found (just in case)
  if(length(row_index) == 0) next
  
  # Get raster names (model names) from the first row
  model_names <- as.character(TOSP_weights[1, -1])  # skip ID column
  rastlist2 <- paste0(model_names, ".tif")
  
  # Get corresponding weights from the matching row
  rastweight <- as.numeric(TOSP_weights[row_index, -1]) / 100
  
  # Load rasters
  allrasters <- rast(file.path("data/raw_data/annual_densities", rastlist2))
  
  # Apply weights
  weighted_rasters <- allrasters * rastweight
  
  # Sum and rescale
  weighted_raster <- sum(weighted_rasters)
  weighted_raster <- weighted_raster / global(weighted_raster, "max", na.rm = TRUE)[1,1]
  
  # Plot
  plot(weighted_raster)
  assign(paste0("TOSPmap_", i), weighted_raster) 
}

#STAL

for(i in 1:5){
  print(i)
  
  ex_id <- paste0("ex_", i)  # construct the ID you're looking for
  
  # Find the row in HAPE_weights that matches the current ex_id
  row_index <- which(STAL_weights[[1]] == ex_id)
  
  # Skip if no match is found (just in case)
  if(length(row_index) == 0) next
  
  # Get raster names (model names) from the first row
  model_names <- as.character(STAL_weights[1, -1])  # skip ID column
  rastlist2 <- paste0(model_names, ".tif")
  
  # Get corresponding weights from the matching row
  rastweight <- as.numeric(STAL_weights[row_index, -1]) / 100
  
  # Load rasters
  allrasters <- rast(file.path("data/raw_data/annual_densities", rastlist2))
  
  # Apply weights
  weighted_rasters <- allrasters * rastweight
  
  # Sum and rescale
  weighted_raster <- sum(weighted_rasters)
  weighted_raster <- weighted_raster / global(weighted_raster, "max", na.rm = TRUE)[1,1]
  
  # Plot
  plot(weighted_raster)
  assign(paste0("STALmap_", i), weighted_raster) 
}





#a loop that makes all the maps and saves them to folders somewhere 
#so, a loop to take each of the maps and calculate each of the proportions and save them to the dataframe (like the loop i wrote in 01_density_script)
#do that once per species
#to get mean low and high for each species - need each of those columns for each of the expert maps 
#eventual goal is making nine lines of data - columns are each lease area, propCA, propOR, propALL, rows are mean low and high for each unk species
#that would then get combined with the cleaned_data file in the app folder and a new version will get put in there







# scratch -----------------------------------------------------------------



#the below code works and is the way to do it from a df column:


# Grab both model names and weights from the same columns
model_names <- as.character(HAPE_weights[1, -1])    # skip ID column
rastlist2 <- paste0(model_names, ".tif")

rastweight <- as.numeric(HAPE_weights[2, -1]) / 100  # also skip ID column

# Load the rasters using file.path
allrasters <- rast(file.path("data/raw_data/annual_densities", rastlist2))

# Multiply each raster by its weight
weighted_rasters <- allrasters * rastweight

# Sum and rescale to 0–1
weighted_raster <- sum(weighted_rasters)
weighted_raster <- weighted_raster / global(weighted_raster, "max", na.rm = TRUE)[1,1]

plot(weighted_raster)


#testing one row 

rastlist2 <- c("LAAL.tif", "BFAL.tif", "LESP.tif", "MUPE.tif") #make a list of the file names for a single species 
rastweight <- c(.25, .25, .25, .25)
allrasters <- rast(file.path("data/raw_data/annual_densities", rastlist2)) #load in all the rasters for the species 
# Apply the weights
weighted_rasters <- allrasters * rastweight
# Sum the weighted rasters
weighted_raster <- sum(weighted_rasters)
#rescale to 1 again
weighted_raster <- weighted_raster / global(weighted_raster, "max", na.rm = TRUE)[1,1] #rescale from 0-1 


plot(weighted_raster)
#ok that all worked 

