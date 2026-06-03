###############################################################################
# Figure 3 Inset Maps - Pink-footed Shearwater
# Outputs 10 maps to paper/fig3_inset_maps/ (no titles, no legends)
# Each raster shown as proportion-of-max on a log10 scale
###############################################################################

# --- Packages ----------------------------------------------------------------
packages <- c("sf", "terra", "tidyterra", "ggplot2", "dplyr", "stringr")
pacman::p_load(packages, character.only = TRUE); rm(packages)

# --- Output directory --------------------------------------------------------
out_dir <- here::here("paper/fig3_inset_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Shared map elements -----------------------------------------------------
crs_omrc <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

states <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp") %>%
  crop(ext(-130, -117, 30, 50)) %>%
  project(crs_omrc)

map_theme <- theme_void() +
  theme(
    legend.position = "none",
    plot.margin     = margin(5, 5, 5, 5)
  )

# low_clip = the "pop" knob. Cells below this fraction of the max all get the
# bottom color, dedicating the ramp to the high-density areas.
#   lower it (e.g. 0.001) to reveal more faint area
#   raise it (e.g. 0.05) to make hotspots pop harder
low_clip <- 0.01

# --- Helper: proportion-of-max, log10 scale ----------------------------------
prop_of_max <- function(r) r / minmax(r)[2]   # each cell / that map's max

save_map_viridis <- function(rast_layer, filename, viridis_option = "E") {
  
  rast_prop <- prop_of_max(rast_layer)
  
  p <- ggplot() +
    geom_spatraster(data = rast_prop) +
    geom_spatvector(data = states, fill = "grey80", color = "white",
                    linewidth = 0.3) +
    scale_fill_viridis_c(
      option   = viridis_option,
      trans    = "log10",
      limits   = c(low_clip, 1),
      oob      = scales::squish,
      na.value = "transparent"
    ) +
    map_theme
  
  ggsave(file.path(out_dir, filename), plot = p,
         width = 5, height = 7, dpi = 300, bg = "white")
  message("Saved: ", filename)
}

# --- Helper: YlOrBr maps (CV) — native scale, no rescaling -------------------
save_map_ylorbr <- function(rast_layer, filename) {
  
  p <- ggplot() +
    geom_spatraster(data = rast_layer) +              # raw CV values
    geom_spatvector(data = states, fill = "grey80", color = "white",
                    linewidth = 0.3) +
    scale_fill_distiller(
      palette   = "YlOrBr",
      direction = 1,
      na.value  = "transparent"
    ) +
    map_theme
  
  ggsave(file.path(out_dir, filename), plot = p,
         width = 5, height = 7, dpi = 300, bg = "white")
  message("Saved: ", filename)
}

###############################################################################
# 1. Mean density — cividis (option "E")
###############################################################################
save_map_viridis(PFSHsp, "01_PFSH_spring_mean_density.png")
save_map_viridis(PFSHsu, "02_PFSH_summer_mean_density.png")
save_map_viridis(PFSHf,  "03_PFSH_fall_mean_density.png")

###############################################################################
# 2. CV maps — YlOrBr
###############################################################################
save_map_ylorbr(PFSHsp_cv, "04_PFSH_spring_cv.png")
save_map_ylorbr(PFSHsu_cv, "05_PFSH_summer_cv.png")
save_map_ylorbr(PFSHf_cv,  "06_PFSH_fall_cv.png")

###############################################################################
# 3. Simulated seasonal maps (sim #1) — cividis
###############################################################################
pfsh_sp_sim1 <- pfsh_seasonal_mc[[str_detect(names(pfsh_seasonal_mc), "spring") &
                                    str_detect(names(pfsh_seasonal_mc), "_1$")]]
pfsh_su_sim1 <- pfsh_seasonal_mc[[str_detect(names(pfsh_seasonal_mc), "summer") &
                                    str_detect(names(pfsh_seasonal_mc), "_1$")]]
pfsh_f_sim1  <- pfsh_seasonal_mc[[str_detect(names(pfsh_seasonal_mc), "fall") &
                                    str_detect(names(pfsh_seasonal_mc), "_1$")]]

save_map_viridis(pfsh_sp_sim1, "07_PFSH_spring_sim1.png")
save_map_viridis(pfsh_su_sim1, "08_PFSH_summer_sim1.png")
save_map_viridis(pfsh_f_sim1,  "09_PFSH_fall_sim1.png")

###############################################################################
# 4. Annual simulated map (sim #1 seasons summed) — cividis
###############################################################################
annual_sim1 <- pfsh_annual_mc[[1]]
save_map_viridis(annual_sim1, "10_PFSH_annual_sim1.png")

message("All 10 maps saved to ", out_dir)