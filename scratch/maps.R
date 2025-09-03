###############################################################################
# Map making script for annual maps for expert elicitation process  ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------

packages<- c("sf", "terra", "tidyr", "tidyterra", "ggplot2", "dplyr")

pacman::p_load(packages, character.only = TRUE); rm(packages)


# Load state outlines -----------------------------------------------------------

#loading in the states outlines and making the projection match the density data
states1 <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
crds(states1, df = FALSE)

e <- ext(-130,-117, 30, 50)
west <- crop(states1, e)
plot(west)

crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data  
states <- project(west, crs)
plot(states)


# test LAAL map -----------------------------------------------------------

LAALw <- rast("data/raw_data/densities/LAAL_winter_predicted_density.tif")
LAALsp <- rast("data/raw_data/densities/LAAL_spring_predicted_density.tif")

#combining the seasons
x <- c(LAALw, LAALsp)
LAAL_annual <- app(x, sum)

#this is making a new layer of the proportion of total density
LAAL <- LAAL_annual %>%
  mutate(newcol = sum/(minmax(LAAL_annual)[2])) %>% #each density/max value
  rename(proportion = newcol)
LAAL

max(LAAL_annual) #this is how to get the value that goes into the legend 

#plot
max_val <- global(LAAL_annual, "max", na.rm = TRUE)[1,1]  # Extracts the actual max value

p <- ggplot()+
  geom_spatraster(data = LAAL, na.rm = TRUE, aes(fill = proportion))+
  geom_spatvector(data=states, color = "#ffffff", fill = "#8290AB")+
  scale_fill_continuous(na.value = "transparent") +  # Make NA values transparent
  theme(axis.text = element_text(size = 20, color = "#ffffff")) +
  theme_minimal()+
  labs(
    title = "Annual Laysan Albatross \n Predicted Density",
    fill = paste0("Proportion of max density\n(", round(max_val, 3), " individuals/km^2)")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Center & style title
    legend.title = element_text(hjust = 0.5, size = 10) 
  )

p +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )


# Loop through to make all maps -------------------------------------------

#set up a list to loop through just the species codes/groups
birdcodes <- list.files("data/raw_data/densities") #make a list of all the file names
birdcodes <- regmatches(birdcodes, regexpr("[^_]+", birdcodes)) #Getting just the species codes - that operator is saying to extract everything up to the first underscore 
cleancodes <- birdcodes[!duplicated(birdcodes)] #make a list without duplicates

#loop!
for(i in cleancodes){
  print(i)    #tracker to show progress
  ##load in and combine the seasons for each species
  rastlist <- list.files("data/raw_data/densities", pattern = i, all.files = TRUE, full.names = FALSE) #make a list of the file names for a single species 
  allrasters <- rast(paste("data/raw_data/densities", rastlist, sep = "/")) #load in all the rasters for the species 
  annualrast <- app(allrasters, sum) #sum the seasons to create a total raster
  bin_labels <- c("<1%", "1-10%", "10-25%", "25-50%", ">50%")
  propannual <- annualrast %>%
    mutate(proportion = sum/(minmax(annualrast)[2]), #each density/max value
           prop_bin = cut(proportion, 
                          c(0, 0.01, 0.1, 0.25, 0.5, 1),
                          labels = bin_labels ))
  #plot
  dens_pal <- rev(RColorBrewer::brewer.pal(n=7, "RdYlBu"))[-(1:2)]
  names(dens_pal) <- bin_labels
 # max_val <- global(annualrast, "max", na.rm = TRUE)[1,1]  # Extracts the actual max value
  p <- ggplot()+
    geom_spatraster(data = propannual, na.rm = TRUE, aes(fill = prop_bin))+
    geom_spatvector(data=states, color = "#ffffff", fill = "#8290AB")+
   # scale_fill_continuous(na.value = "transparent") +  # Make NA values transparent
    scale_fill_manual(values = dens_pal, na.value = "transparent", na.translate = FALSE) +
    theme(axis.text = element_text(size = 20, color = "#ffffff")) +
    theme_minimal()+
    labs(
      title = paste0("Annual ", i, " Predicted Density"),
      fill = paste0("Percentage of maximum \n density/km^2")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Center & style title
      legend.title = element_text(hjust = 0.5, size = 10) 
    )
  
  p <- p +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  # Save the plot
  ggsave(
    filename = paste0("reports/images/", i, "_density_map_binned.png"),  # Save one level up
    plot = p,
    width = 10.5, height = 8, dpi = 400  # Adjust size and resolution as needed
  )
}


