source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

gn_areas <- "../../Spatial Data/fishing_effort_data/Gill_Net_Areas/Gill_net_areas/Gill_net_areas.shp"
gn_areas <- st_read(gn_areas)

gn_intensity <- "../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity.tif"
gn_intensity <- trim(rast(gn_intensity))
gn_intensity <- as.numeric(gn_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(gn_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_sf(data = gn_areas, alpha = 0.2) +
    geom_spatraster(data = gn_intensity, na.rm = TRUE) +
    # geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
