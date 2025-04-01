source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

pll_intensity <- "../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity.tif"
pll_intensity <- trim(rast(pll_intensity))
pll_intensity <- as.numeric(pll_intensity)

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = pll_intensity, na.rm = TRUE) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
