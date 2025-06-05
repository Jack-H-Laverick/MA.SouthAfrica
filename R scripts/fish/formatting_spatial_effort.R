library(terra)
library(sf)
library(glue)
library(stringr)
library(dplyr)

source("./R Scripts/@_Region file.R")

# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

directories <- list.dirs("../../Spatial Data/fishing_effort_data/")

for (directory in directories) {
    files <- list.files(directory, full.names = TRUE)
    if (!any(str_detect(files, ".tif$"))) next
    if (any(str_detect(files, glue("{crs}.tif")))) next

    var_name <- last(str_split(directory, "(/)")[[1]])

    print(glue("Reprojecting spatial effort data for {var_name}"))

    raster <- rast(files[str_detect(files, ".tif$")])
    raster <- project(raster, glue("epsg:{crs}"), method = "near")
    writeRaster(raster, glue("{directory}/{var_name}_{crs}.tif"))
}
