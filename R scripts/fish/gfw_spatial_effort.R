library(tidyverse)
library(ggplot2)
library(arrow)
library(sf)

# Perform spatial filtering of fishing event points in Domains in Julia for extra performance ----
system(paste('Julia --project=@. --threads=auto "R Scripts/fish/gfw_spatial_effort.jl"'))

# library(gfwr)
# key <- gfw_auth()
# sa_eez <- get_region_id(region_name = "South Africa", region_source = "EEZ", key = key)
# sa_domain <- readRDS("./Objects/Domains.rds")

# sa_fishing_events <- get_event(
#     "FISHING",
#     start_date = "2017-01-01",
#     end_date = "2019-12-31",
#     region_source = "EEZ",
#     region = sa_eez$id,
#     duration = 30,
#     key = key
# )

# sa_gap_events <- get_event(
#     "GAP",
#     start_date = "2010-01-01",
#     end_date = "2019-12-31",
#     region_source = "EEZ",
#     region = sa_eez$id,
#     duration = 30,
#     key = key
# )

# sa_fishing_raster <- get_raster(
#     spatial_resolution = "LOW",
#     temporal_resolution = "YEARLY",
#     group_by = "FLAG",
#     start_date = "2019-01-01",
#     end_date = "2019-12-31",
#     region_source = "USER_SHAPEFILE",
#     region = sa_domain,
#     key = key
# )

fleet_daily <- read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily.parq")

str(fleet_daily)
unique(fleet_daily$flag)
cumulative_fishing_hours <- fleet_daily %>%
    group_by(flag) %>%
    summarise(total_fishing_hours = sum(fishing_hours)) %>%
    mutate(prop_of_total = total_fishing_hours / sum(total_fishing_hours))
cumulative_fishing_hours <- arrange(cumulative_fishing_hours, desc(total_fishing_hours))
flags <- cumulative_fishing_hours$flag
ggplot() +
    geom_bar(
        data = cumulative_fishing_hours[cumulative_fishing_hours$prop_of_total > 0.0001, ],
        aes(x = factor(flag, flags), y = total_fishing_hours, fill = flag),
        stat = "identity"
    )

fleet_domain <- read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-domain.parq")

mmsi_daily <- read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily.parq")
