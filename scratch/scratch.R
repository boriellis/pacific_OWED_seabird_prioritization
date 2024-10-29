###############################################################################
# Seabird species prioritization framework for offsetting offshore wind energy impacts in the California Current   ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
# Manuscript Authors:  #
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------
packages<- c("sf", "terra", "dplyr")

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


# Part 3: Set up for the loop ---------------------------------------------


#load up the two CORM models

cormf <- rast("data/raw_data/densities/CORM_fall_predicted_density.tif")
cormw <- rast("data/raw_data/densities/CORM_winter_predicted_density.tif")
cormall <- cormf+cormw

propsdf2 <- data.frame(matrix(ncol = 8, nrow = length(1))) #make the empty dataframe
colnames(propsdf2) <- c("totdens_OCSP0561", "totdens_OCSP0562", "totdens_OCSP0563", "totdens_OCSP0564", "totdens_OCSP0565", "totdens_OCSP0566", "totdens_OCSP0567", "totdens_region") #making colnames to house the densities for each area

propsdf2[1, "totdens_OCSP0561"] <- terra::extract(cormall, OCSP0561, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0562"] <- terra::extract(cormall, OCSP0562, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0563"] <- terra::extract(cormall, OCSP0563, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0564"] <- terra::extract(cormall, OCSP0564, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0565"] <- terra::extract(cormall, OCSP0565, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0566"] <- terra::extract(cormall, OCSP0566, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_OCSP0567"] <- terra::extract(cormall, OCSP0567, exact = TRUE, touches = TRUE, ID = FALSE) |>
  dplyr::summarise(dplyr::across(!fraction, ~ sum(.x * fraction)))
propsdf2[1, "totdens_region"] <- (sum(values(cormall), na.rm = TRUE)) 

propsdf3 <- propsdf2 %>% 
  mutate(propALL = ((totdens_OCSP0561 + totdens_OCSP0562 + totdens_OCSP0563 + totdens_OCSP0564 + totdens_OCSP0565 + totdens_OCSP0566 + totdens_OCSP0567)/totdens_region)
) 

