source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

small_scale_intensity <- "../../Spatial Data/fishing_effort_data/Small_Scale_Harvest_Intensity/Small_Scale_Harvest_Intensity/Small_Scale_Harvest_Intensity.tif"
small_scale_intensity <- trim(rast(small_scale_intensity))
small_scale_intensity <- as.numeric(small_scale_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(small_scale_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = small_scale_intensity, na.rm = TRUE) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
