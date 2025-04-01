source("./R scripts/fish/fisheries_data_functions.R")
library(terra)
library(rnaturalearth)
library(tidyterra)

oyster_fn <- "../../Spatial Data/fishing_effort_data/Approximate_Oyster_Harvesting/Approximate_Oyster_Harvesting/Approximate_Oyster_Harvesting.tif"
oyster <- as.numeric(trim(rast(oyster_fn)))

sa_outline <- ne_countries(country = "south africa")
ggplot() +
    geom_spatraster(data = oyster, aes(fill = OID)) +
    geom_sf(data = sa_outline, alpha = 0.3) +
    scale_fill_viridis()

# Plot actual values
hist(oyster)
