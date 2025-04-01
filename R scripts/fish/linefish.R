source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

lf_intensity <- "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity.tif"
lf_intensity <- trim(rast(lf_intensity))
lf_intensity <- as.numeric(lf_intensity)

# Check if terra is interpretting the values as continuous or categorical
max(values(lf_intensity, na.rm = TRUE))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = lf_intensity, na.rm = TRUE) +
    # geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
