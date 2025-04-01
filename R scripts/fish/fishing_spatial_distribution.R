library(ggplot2)
library(terra)
library(sf)

# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

# Load Spatial fishing data files
