# Script to combine all downloaded global fishing watch apparent fishing data into accumulated parquet files
# that are space and time efficient compared to csvs.

source("./R scripts/fish/fishing_spatial_functions.R")

sa_eez <- read_sf("./Data/spatial/SA_EEZ.json")
sau_fao_area <- read_sf("./Data/spatial/FAO_Major_Fishing_Areas.geojson") %>%
    filter(F_CODE == "47")

# Retrieve just the area that Seas Around Us uses for the Atlantic and Cape region, which is the intersection between the SA EEZ and the FAO zone 47
sau_west_area <- st_intersection(sa_eez, sau_fao_area)
st_write(sau_west_area, "./Objects/sau_atlantic_cape_region.gpkg")

csv_dirs <- list.dirs("../../Spatial Data/fishing_effort_data/Global_fishing_watch/")[-1]
walk(csv_dirs, combine_csv_parquet)

file_types <- c("fleet-daily", "fleet-monthly", "mmsi-daily")
patterns <- c("[fleet][-][daily]", "[fleet][-][monthly]", "[mmsi][-][daily]")
combine_parqs <- function(file_type, pattern) combine_year_parquets(dir = "../../Spatial Data/fishing_effort_data/Global_fishing_watch/", file_base = file_type, pattern = pattern)
walk2(file_types, patterns, combine_parqs, .progress = TRUE)
