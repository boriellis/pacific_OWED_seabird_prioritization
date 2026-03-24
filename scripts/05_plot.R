
packages<- c("tidyverse", "sf", "terra", "tidyr", "tidyterra", "ggridges", "scico")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/visualizations.R"))



###########################################################################
#####################                           ###########################
#####################       RESULTS TABLES      ###########################
#####################                           ###########################
###########################################################################


#load data
raw_scores <- read_rds(here::here("output/priority_scores_1000_321.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

#CA
formatted_results_table_CA <- clean_priority_vals(raw_scores, se, st, "CA")
write_csv(formatted_results_table_CA, "paper/CA_results_table.csv")

#all lease areas
formatted_results_table_all <- clean_priority_vals(raw_scores, se, st, "all")
write_csv(formatted_results_table_all, "paper/POCS_results_table.csv")


###########################################################################
#####################                           ###########################
#####################       RESULTS BOXPLOTS    ###########################
#####################                           ###########################
###########################################################################

#load in data
sp_list <- read_csv("data/raw_data/total_sp_list.csv")
tax <- read_csv("data/raw_data/Clements-v2024-October-2024-rev.csv")
priority_dists <- read_rds("output/priority_scores_1000_321.rds")


#CA
boxplot_CA <- make_boxplot(sp_list, tax, priority_dists, "CA")
ggsave(here::here("paper/boxplot_CA.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)

#max's suggestion
boxplot_CA_alt <- make_boxplot2(sp_list, tax, priority_dists, "CA")
ggsave(here::here("paper/boxplot_CA_alt.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)


#all lease areas
boxplot_all <- make_boxplot(sp_list, tax, priority_dists, "all")
ggsave(here::here("paper/boxplot_all.png"), plot = last_plot(), width = 12, height = 10, units = "in", dpi = 300)




###########################################################################
#####################                           ###########################
#####################      UNCERTAINTY PLOTS    ###########################
#####################                           ###########################
###########################################################################


# Generate rank output dataframes DON'T RERUN  -----------------------------------------
#generate the outputs for plots - each of 321, 111, 211, 121, and 112 is saved in paper folder as an RDS. don't re-run unless you re-run all

#load data
e <- read_rds(here::here("output/cleaned_exposure_1000sims.rds"))
se <- read_rds(here::here("output/sensitivity_sum.rds"))
st <- read_rds(here::here("output/status.rds"))

# #3, 2, 1
# priority_ranks321 <- priority_mc(e, se, st, w = c(3, 2, 1))
# saveRDS(priority_ranks321, "paper/321priority_ranks_1000_for_plots.rds")
# 
# 
# #1, 1, 1
# priority_ranks111 <- priority_mc(e, se, st, w = c(1, 1, 1))
# saveRDS(priority_ranks111, "paper/111priority_ranks_1000_for_plots.rds")
# 
# #2, 1, 1
# priority_ranks211 <- priority_mc(e, se, st, w = c(2, 1, 1))
# saveRDS(priority_ranks211, "paper/211priority_ranks_1000_for_plots.rds")
# 
# 
# #1, 2, 1
# priority_ranks121 <- priority_mc(e, se, st, w = c(1, 2, 1))
# saveRDS(priority_ranks121, "paper/121priority_ranks_1000_for_plots.rds")
# 
# #1, 1, 2
# priority_ranks112 <- priority_mc(e, se, st, w = c(1, 1, 2))
# saveRDS(priority_ranks112, "paper/112priority_ranks_1000_for_plots.rds")
# 





# set up ------------------------------------------------------------------

# set colors
topsps_CA <- c("Pink-footed Shearwater", "Cassin's Auklet", "Red Phalarope", "Red-necked Phalarope", "Ashy Storm-Petrel", "Guadalupe Murrelet", "Pomarine Jaeger",  "Rhinoceros Auklet", "Craveri's Murrelet", "Sooty Shearwater", "Scripps's Murrelet", "Townsend's Storm-Petrel", "Buller's Shearwater", "Sabine's Gull", "South Polar Skua", "Marbled Murrelet", "Bonaparte's Gull", "Black Scoter", "Short-tailed Albatross","Northern Fulmar", "Hawaiian Petrel", "California Gull")

# if you want colors to be ordered by priority
spcolors_pri <- c("#F9DD8B", "#FBD588", "#F7C98D", "#F3BB84", "#E5A67C", "#E9946F", "#E88164", "#DB705F", "#C5655F", "#AE6363", "#996169", "#86606E", "#755F72", "#655E76", "#565B7A", "#405578", "#25486D", "#103657", "#07243E", "#021326", "#020F1C",  "#01080F")


# load differently weighted simulations

ranks321 <- readRDS(here::here("paper/321priority_ranks_1000_for_plots.rds"))
ranks111 <- readRDS(here::here("paper/111priority_ranks_1000_for_plots.rds"))
ranks211 <- readRDS(here::here("paper/211priority_ranks_1000_for_plots.rds"))
ranks121 <- readRDS(here::here("paper/121priority_ranks_1000_for_plots.rds"))
ranks112 <- readRDS(here::here("paper/112priority_ranks_1000_for_plots.rds"))


# ridge plots -------------------------------------------------------------

#main plot
p1 <- ridgeplot(ranks321, topsps_CA, spcolors_pri, "CA", 47)
ggsave(here::here("paper/3_2_1_ridgeplot.png"), plot = p1, width = 10, height = 8, units = "in", dpi = 300)

#main plot to wrap with others
p_wrap <- ridgeplot3(ranks321, topsps_CA, spcolors_pri, "CA", 47)
ggsave(here::here("paper/3_2_1_ridgeplot_wrap.png"), plot = p_wrap, width = 10, height = 7, units = "in", dpi = 300)

#sensitivity plots to combine in illustrator 
p2 <- ridgeplot2(ranks111, topsps_CA, spcolors_pri, "CA", 37)
ggsave(here::here("paper/1_1_1_ridgeplot.png"), plot = p2, width = 8, height = 6, units = "in", dpi = 300)

p3 <- ridgeplot2(ranks211, topsps_CA, spcolors_pri, "CA", 43)
ggsave(here::here("paper/2_1_1_ridgeplot.png"), plot = p3, width = 8, height = 6, units = "in", dpi = 300)

p4 <- ridgeplot2(ranks121, topsps_CA, spcolors_pri, "CA", 25)
ggsave(here::here("paper/1_2_1_ridgeplot.png"), plot = p4, width = 8, height = 6, units = "in", dpi = 300)

p5 <- ridgeplot2(ranks112, topsps_CA, spcolors_pri, "CA", 20)
ggsave(here::here("paper/1_1_2_ridgeplot.png"), plot = p5, width = 8, height = 6, units = "in", dpi = 300)



# stacked histograms -------------------------------------------------------

#main plot

h1 <- stackedhist(ranks321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_stackedhist.png"), plot = h1, width = 10, height = 8, units = "in", dpi = 300)

#for illustrator combo
h_wrap <- stackedhist3(ranks321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_stackedhist_wrap.png"), plot = h_wrap, width = 8, height = 6 , units = "in", dpi = 300)

#for legend screenshot
legend <- stackedhist4(ranks321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_legend.png"), plot = legend, width = 10, height = 12 , units = "in", dpi = 300)

#sensitivity plots to combine in illustrator 

h2 <- stackedhist2(ranks111, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_1_stackedhist.png"), plot = h2, width = 8, height = 6, units = "in", dpi = 300)

h3 <- stackedhist2(ranks211, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/2_1_1_stackedhist.png"), plot = h3, width = 8, height = 6, units = "in", dpi = 300)

h4 <- stackedhist2(ranks121, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_2_1_stackedhist.png"), plot = h4, width = 8, height = 6, units = "in", dpi = 300)

h5 <- stackedhist2(ranks112, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_2_stackedhist.png"), plot = h5, width = 8, height = 6, units = "in", dpi = 300)


# ESS plots ---------------------------------------------------------------


#load data
results321 <- read_rds(here::here("output/priority_scores_1000_321.rds"))
results111 <- read_rds(here::here("output/priority_scores_1000_111.rds"))
results211 <- read_rds(here::here("output/priority_scores_1000_211.rds"))
results121 <- read_rds(here::here("output/priority_scores_1000_121.rds"))
results112 <- read_rds(here::here("output/priority_scores_1000_112.rds"))

#main solo plot

e1 <- ess(results321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_ess.png"), plot = e1, width = 12, height = 10, units = "in", dpi = 300)

#main plot for wrapping
e_wrap <- ess3(results321, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/3_2_1_ess_wrap.png"), plot = e_wrap, width = 8, height = 13, units = "in", dpi = 300)

#sensitivity plots to combine in illustrator

e2 <- ess2(results111, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_1_ess.png"), plot = e2, width = 8, height = 6, units = "in", dpi = 300)

e3 <- ess2(results211, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/2_1_1_ess.png"), plot = e3, width = 8, height = 6, units = "in", dpi = 300)

e4 <- ess2(results121, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_2_1_ess.png"), plot = e3, width = 8, height = 6, units = "in", dpi = 300)

e5 <- ess2(results112, topsps_CA, spcolors_pri, "CA")
ggsave(here::here("paper/1_1_2_ess.png"), plot = e5, width = 8, height = 6, units = "in", dpi = 300)



# study area map ----------------------------------------------------------



#loading in the states outlines and making the projection match the density data
states1 <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
crds(states1, df = FALSE)

e <- ext(-130,-117, 30, 50)
west <- crop(states1, e)
plot(west)

crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data  
states <- project(west, crs)

plot(states)


#Get study area
LAALw <- rast("data/raw_data/leirness_model_outputs/LAAL_winter_predicted_density.tif")

# create binary mask of non-NA cells
mask <- !is.na(LAALw)

# convert to polygons
poly <- as.polygons(mask, dissolve = TRUE)

# convert to sf
poly_sf <- st_as_sf(poly)

# keep only the TRUE area
study_area <- poly_sf[poly_sf$LAAL_winter_predicted_density == 1, ]

sf::st_write(study_area, "data/raw_data/study_area.shp")



library(ggspatial)

ggplot() +
  annotation_map_tile(type = "cartolight")



p <- ggplot() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#BFD6E6", color = NA),
    plot.background = element_rect(fill = "#BFD6E6", color = NA),
    axis.text = element_text(size = 20, color = "#ffffff")
  ) +
  geom_spatvector(data = states, color = "#ffffff", fill = "#8290AB") +
  geom_spatvector(data = study_area, color = "#D73027", fill = NA)


library(rnaturalearth)
library(sf)

land <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
  
  # ocean background
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#CFE8F3", color = NA),
    plot.background = element_rect(fill = "#CFE8F3", color = NA)
  ) +
  
  # land
  geom_sf(data = land, fill = "#8290AB", color = "white", linewidth = 0.3) +
  
  # study area
  geom_sf(data = study_area, fill = NA, color = "#D73027", linewidth = 0.5) +
  
  # crop map
  coord_sf(
    xlim = c(-132, -116),
    ylim = c(29, 50),
    expand = FALSE
  )







