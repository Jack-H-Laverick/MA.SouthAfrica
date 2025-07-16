# Script to extract spatial effort data from GFW for the model domain/SAU areas and create nc files.

library(furrr)
library(tictoc)

source("./R scripts/fish/fishing_spatial_functions.R")

# Perform spatial filtering of fishing event points in Domain and SAU area in Julia for extra performance ----
domain <- readRDS("./Objects/Domains.rds")
system(paste('Julia --project=@. --threads=auto "R scripts/fish/gfw_2_1_spatial_effort.jl"'))

# Process data for domain area.
bbox <- st_bbox(domain)
res <- 0.01
empty_domain_raster <- rast(
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"],
    resolution = res,
    vals = NA,
    crs = "EPSG:4326"
)

fleet_domain <- read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-domain.parq")
unique(fleet_domain$geartype)
fleet_domain <- fleet_domain %>%
    mutate(
        gear_type_reclass = case_when(
            geartype %in% c("purse_seines", "tuna_purse_seines", "other_purse_seines") ~ "purse_seine",
            geartype == "drifting_longlines" ~ "pelagic_longline",
            geartype == "set_longlines" ~ "demersal_longline",
            geartype == "pole_and_line" ~ "pole_and_line",
            geartype == "trawlers" ~ "trawl",
            geartype == "squid_jigger" ~ "squid_jig"
        )
    )

fleet_domain <- fleet_domain[!is.na(fleet_domain$gear_type_reclass), ]
fleet_domain_wider <- fleet_domain %>%
    mutate(year = year(as.Date(date))) %>% # Calculate year from date
    group_by(cell_ll_lon, cell_ll_lat, flag, gear_type_reclass, year) %>% # group each cell by flag, gear type and year
    summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>% # calculate sum
    ungroup() %>%
    pivot_wider(names_from = gear_type_reclass, values_from = fishing_hours)

cumulative_fishing_hours <- fleet_domain %>%
    group_by(flag) %>%
    summarise(total_fishing_hours = sum(fishing_hours)) %>%
    mutate(prop_of_total = total_fishing_hours / sum(total_fishing_hours))
cumulative_fishing_hours <- arrange(cumulative_fishing_hours, desc(total_fishing_hours))

vars <- unique(fleet_domain$gear_type_reclass)
flags <- unique(cumulative_fishing_hours[cumulative_fishing_hours$prop_of_total >= 0.01, ]$flag)

walk(flags, function(x) create_effort_raster(fleet_domain_wider, vars, target, x, "domain"), .progress = TRUE)

# Process data for SAU area.
bbox <- st_bbox(sau_west_area)
res <- 0.01
empty_sau_raster <- rast(
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"],
    resolution = res,
    vals = NA,
    crs = "EPSG:4326"
)

fleet_sau_area <- read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-sau_area.parq")
unique(fleet_sau_area$geartype)
fleet_sau_area <- fleet_sau_area %>%
    mutate(
        gear_type_reclass = case_when(
            geartype %in% c("purse_seines", "tuna_purse_seines", "other_purse_seines") ~ "purse_seine",
            geartype == "drifting_longlines" ~ "pelagic_longline",
            geartype == "set_longlines" ~ "demersal_longline",
            geartype == "pole_and_line" ~ "pole_and_line",
            geartype == "trawlers" ~ "trawl",
            geartype == "squid_jigger" ~ "squid_jig"
        )
    )

fleet_sau_area <- fleet_sau_area[!is.na(fleet_sau_area$gear_type_reclass), ]
fleet_sau_area_wider <- fleet_sau_area %>%
    mutate(year = year(as.Date(date))) %>% # Calculate year from date
    group_by(cell_ll_lon, cell_ll_lat, flag, gear_type_reclass, year) %>% # group each cell by flag, gear type and year
    summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>% # calculate sum
    ungroup() %>%
    pivot_wider(names_from = gear_type_reclass, values_from = fishing_hours)

cumulative_fishing_hours <- fleet_sau_area %>%
    group_by(flag) %>%
    summarise(total_fishing_hours = sum(fishing_hours)) %>%
    mutate(prop_of_total = total_fishing_hours / sum(total_fishing_hours))
cumulative_fishing_hours <- arrange(cumulative_fishing_hours, desc(total_fishing_hours))

vars <- unique(fleet_sau_area$gear_type_reclass)
flags <- unique(cumulative_fishing_hours[cumulative_fishing_hours$prop_of_total >= 0.01, ]$flag)

walk(flags, function(x) create_effort_raster(fleet_sau_area_wider, vars, empty_sau_raster, x, "sau_area"), .progress = TRUE)
