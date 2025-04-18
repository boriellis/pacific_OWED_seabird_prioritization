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

for(i in cleancodes){
  print(i)    #tracker to show progress
  ##load in and combine the seasons for each species
  rastlist <- list.files("data/raw_data/densities", pattern = i, all.files = TRUE, full.names = FALSE) #make a list of the file names for a single species 
  allrasters <- rast(paste("data/raw_data/densities", rastlist, sep = "/")) #load in all the rasters for the species 
  annualrast <- app(allrasters, sum) #sum the seasons to create a total raster
  writeRaster(annualrast, filename = file.path("data/raw_data/annual_densities", paste0(i, ".tif")), overwrite = TRUE)
} #this worked, I tested it

# Part 4: load in and clean weights from experts --------------------------

raw_exweights <- densities <- read.csv(here::here("data/raw_data/expert_scratch_data.csv")) #replace this with final file later

#intial clean
exweights <- raw_exweights %>% 
  select(-(1:17)) %>% 
  slice(-2) %>% 
  mutate(ex_id = if_else(row_number() == 1,
                         "id",
                         paste0("ex_", row_number() -1)))
  
head(exweights)

#make vector to rename model name row to match map names
model_names <- c("id", "SCOT", "PHAL", "PAJA-LTJA", "POJA", "SPSK", "RHAU", "TUPU", "CAAU", "MAMU", "PIGU", "COMU", "ANMU", "SCMU-GUMU-CRMU", "BLKI", "SAGU", "BOGU", "HEGU", "WEGU-WGWH-GWGU", "CAGU", "HERG-ICGU", "CATE", "COTE-ARTE", "ROYT-ELTE", "WEGR-CLGR", "RTLO", "COLO", "LOON", "LAAL", "BFAL", "FTSP", "LESP", "ASSP", "BLSP", "NOFU", "MUPE", "COPE", "PFSH", "BULS", "STTS-SOSH-FFSH", "BVSH", "BRCO", "PECO", "DCCO", "BRPE")

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






