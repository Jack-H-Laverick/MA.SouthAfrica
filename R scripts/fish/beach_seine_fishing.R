source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

bs_areas <- "../../Spatial Data/fishing_effort_data/Beach_Seine_Areas/Beach_seine_areas/Beach_Seine_Areas.shp"
bs_areas <- st_read(bs_areas)

bs_intensity <- "../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_intensity/Beach_Seine_Intensity.tif"
bs_intensity <- trim(rast(bs_intensity))
# See the true range of values for beach seine intensity
hist(bs_intensity) # Values have been loaded categorically by terra
bs_intensity <- as.numeric(bs_intensity)


sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_sf(data = bs_areas, alpha = 0.2) +
    geom_spatraster(data = bs_intensity, na.rm = TRUE) +
    # geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()
