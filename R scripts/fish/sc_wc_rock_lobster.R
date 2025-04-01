source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

sc_rl_intensity <- "../../Spatial Data/fishing_effort_data/South_Coast_Rock_Lobster_Intensity/South_Coast_Rock_Lobster_Intensity/South_Coast_Rock_Lobster_Intensity.tif"
wc_rl_intensity <- "../../Spatial Data/fishing_effort_data/West_Coast_Rock_Lobster_Intensity/West_Coast_Rock_Lobster_Intensity/West_Coast_Rock_Lobster_Intensity.tif"

rl_intensity <- trim(rast(rl_intensity))
rl_intensity <- as.numeric(rl_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(rl_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = rl_intensity, na.rm = TRUE) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
