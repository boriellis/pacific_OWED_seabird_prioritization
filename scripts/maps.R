###############################################################################
# Map making script for annual maps for expert elicitation process  ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------

packages<- c("sf", "terra", "tidyr", "tidyterra", "ggpolot2", "dplyr")

pacman::p_load(packages, character.only = TRUE); rm(packages)


# Load state outlines -----------------------------------------------------------

#loading in the states outlines and making the projection match the density data
states1 <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
crds(states1, df = FALSE)

e <- ext(-130,-115, 30, 50)
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

#plot
p <- ggplot()+
  geom_spatraster(data = LAAL, na.rm = TRUE, aes(fill = proportion))+
  geom_spatvector(data=states, color = "#ffffff", fill = "#8290AB")+
  theme(axis.text = element_text(size = 20, color = "#ffffff")) +
  theme_minimal()+
  labs(fill = "Proportion of max density\n(0.588 individuals/km^2)")

p +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
p












