source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

rec_shore_intensity <- "../../Spatial Data/fishing_effort_data/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity.tif"
rec_shore_intensity <- trim(rast(rec_shore_intensity))
rec_shore_intensity <- as.numeric(rec_shore_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(rec_shore_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = rec_shore_intensity, na.rm = TRUE) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
