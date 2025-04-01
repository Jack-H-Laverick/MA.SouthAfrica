source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

pt_intensity <- "../../Spatial Data/fishing_effort_data/Prawn_Trawl_Footprint/Prawn_Trawl/Prawn_Trawl_footprint.tif"
pt_intensity <- trim(rast(pt_intensity))
pt_intensity <- as.numeric(pt_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(pt_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = pt_intensity, na.rm = TRUE) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
# Data only covers section of North-East coast (Out of bounds of implementation)
