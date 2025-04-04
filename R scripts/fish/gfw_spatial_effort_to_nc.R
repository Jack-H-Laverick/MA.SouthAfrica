library(tidyverse)
library(ggplot2)
library(arrow)
library(sf)
library(terra)
library(furrr)
library(tictoc)

# Perform spatial filtering of fishing event points in Domains in Julia for extra performance ----
domain <- readRDS("./Objects/Domains.rds")
st_write(domain, "./Objects/Domains.gpkg")
system(paste('Julia --project=@. --threads=auto "R Scripts/fish/gfw_spatial_effort.jl"'))

bbox <- st_bbox(domain)
res <- 0.01
empty_raster <- rast(
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
            geartype == "pole_and_line" ~ "linefishery",
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

create_variable_raster <- function(flag_data, x_var, target) {
    # Then for each fishing variable
    var_raster <- dplyr::select(flag_data, c("cell_ll_lon", "cell_ll_lat", "year", all_of(x_var))) %>% # Select it along with spatial and temporal variables
        split(f = .$year) %>% # Split by year so we get one raster per time step
        map(
            function(x, target, x_var) { # For each year

                raster <- target # Take the target grid
                cells <- terra::cellFromXY(target, cbind(x[, c("cell_ll_lon", "cell_ll_lat")])) # Find which cells on the grid our observations fall on
                raster[cells] <- x[, x_var] # Copy over the data

                return(raster)
            }, # Return an updated raster
            target = target, x_var = x_var
        ) %>% # Specify the target raster
        rast() # And bind each year into a raster brick of one variable for one flag

    return(var_raster)
}

create_effort_raster <- function(dataframe, vars, target, target_flag) {
    tic()
    flag_data <- filter(dataframe, flag == target_flag) # Limit to one in turn
    vars <- names(select(flag_data, !c(cell_ll_lon, cell_ll_lat, flag, year)))
    for (var in vars) {
        if (sum(!is.na(flag_data[, var])) == 0) {
            vars <- vars[vars != var]
        }
    }

    print(glue::glue("Now processing {target_flag}")) # Keeping track

    flag_rasters <- map(vars, function(x) create_variable_raster(flag_data, x, target))
    flag_rasters <- lapply(flag_rasters, function(flag_raster) {
        time(flag_raster) <- as.numeric(names(flag_raster))

        return(flag_raster)
    })

    print(glue::glue("Now saving {target_flag}")) # Keeping track
    future_map2(flag_rasters, paste0(target_flag, "-", vars), ~ { # Then save to netcdf
        writeCDF(.x,
            filename = paste0("./Data/fleet_fishing_", .y, ".nc"), overwrite = TRUE, # Building a name from flag and variable
            varname = .y, unit = "Hours", zname = "year",
            atts = c("x=Longitude", "y=Latitude")
        )
    }, .progress = TRUE)
    toc()
}

walk(flags, function(x) create_effort_raster(fleet_domain_wider, vars, target, x), .progress = TRUE)
