library(ggplot2)
library(terra)
library(sf)
library(glue)
library(docstring)
library(tidyterra)
library(exactextractr)
source("./R scripts/fish/fisheries_data_functions.R")
source("./R scripts/@_Region file.R")

# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

# Load GFW effort data (hours)
gfw_polygon_extraction <- function(file, polygons) {
    #' Extracting GFW data for habitat polygons from GFW netcdf files.
    #'
    #' File takes a file name containing flag and GFW variable information.
    #' Calculates the total hours for each year and habitat/shore type.
    flag <- file %>%
        str_split_i(pattern = "(?=[-])", i = 1) %>%
        str_split_i(pattern = "(?<=[_])", i = 3)
    variable <- file %>%
        str_split_i(pattern = "([-])", i = 2) %>% # Split to get last section of file name
        str_split_i(pattern = "([.])", i = 1)

    extracted_data <- rast(glue("./Data/{file}")) %>% # Import a brick of all years
        exact_extract(polygons, fun = "sum", append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        pivot_longer(
            cols = matches("\\d"),
            names_to = "year",
            values_to = "hours"
        ) %>%
        mutate(year = as.numeric(str_split_i(year, "(?<=[=])", i = 2))) %>%
        mutate(variable = glue("{flag}_{variable}")) %>%
        select(c(Habitat, Shore, year, variable, hours))

    return(extracted_data)
}

nc_files <- list.files("./Data/", pattern = ".nc")
# Extract the total yearly effort in hours for each habitat/shore area and variable
extracted_data <- map(
    nc_files,
    function(x) gfw_polygon_extraction(x, habitats)
)
# For each variable, calculate the proportion of effort distributed to each habitat/shore type
gfw_data <- rbindlist(extracted_data) %>%
    group_by(variable, Habitat, Shore) %>%
    summarise(hours = mean(hours)) # %>%
# group_by(variable) %>%
# mutate(proportion = hours / sum(hours))

extract_habitat_data <- function(raster, habitats, fun, name) {
    #' Use exactextractr::exact_extract() function to extract raster data from each zone and rename resulting column.
    extracted <- raster %>%
        exact_extract(habitats, fun = fun, append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        rename(!!name := fun)

    return(extracted)
}

format_sanbi_raster <- function(raster_fn, habitats) {
    #' Format a SANBI 'cumulative stress index' raster into usable values that contain just positive effort.
    raster <- rast(raster_fn)
    raster <- crop(raster, habitats)
    raster <- as.numeric(raster)
    raster <- subst(raster, 0, NA) # Replace 0 values with NA because there is no fishing in that area.

    return(raster)
}

sanbi_proportion_effort <- function(raster, habitats, fun, name) {
    #' Extract the effort data for each zone from a SANBI raster object and calculate the proportional effort for each zone.
    extracted <- extract_habitat_data(raster, habitats, fun, name)
    extracted[, name] <- ifelse(is.na(extracted[, name]), 0, extracted[, name])
    extracted <- extracted %>% mutate("proportion_{name}" := .[, name] / sum(.[, name]))

    return(extracted)
}

# prop_covered <- function(x) {
#     if (class(x) == "data.frame") {
#         x <- x$value
#         return(sum(!is.na(x)) / length(x))
#     } else {
#         return(sum(!is.na(x)) / length(x))
#     }
# }

# domain_rough_bound <- ext(lims)

# Load Midwater Trawl intensity data - need to acquire demersal trawl data - hours of trawling
mw_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif",
    habitats
)
mw_data <- sanbi_proportion_effort(
    mw_intensity,
    habitats,
    "sum",
    "midwater_trawl"
)

ggplot() +
    geom_spatraster(data = mw_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Load spatial data for nets including small scale
## Load Gillnets
gn_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity_{crs}.tif",
    habitats
)
gn_intensity <- subst(gn_intensity, NA, 0) # Set NA values to 0 for addition of second layer

## Load beach seine nets
bs_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity_{crs}.tif",
    habitats
)
bs_intensity <- subst(bs_intensity, NA, 0) # Set NA values to 0 for addition of second layer

net_intensity <- gn_intensity + bs_intensity
net_intensity <- subst(net_intensity, 0, NA) # Set 0 values to NA to just extract from regions with effort.
net_data <- sanbi_proportion_effort(net_intensity, habitats, "sum", "nets")

ggplot() +
    geom_spatraster(data = net_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract Pole and Line data


# Extract Squid Jig data
sj_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Fishery_Intensity_{crs}.tif",
    habitats
)
sj_data <- sanbi_proportion_effort(
    sj_intensity,
    habitats,
    "sum",
    "squid_jig"
)
ggplot() +
    geom_spatraster(data = sj_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Collate pelagic and demersal longline data from GFW
longline_data <- gfw_data %>%
    filter(variable %in% c("ZAF_pelagic_longline", "ZAF_demersal_longline")) %>%
    group_by(Habitat, Shore) %>%
    summarise(hours = sum(hours)) %>%
    mutate(proportion_longline = hours / sum(hours))

# Collate purse seine data from GFW
purse_seine_data <- gfw_data %>%
    filter(variable == "ZAF_purse_seine") %>%
    group_by(Habitat, Shore) %>%
    summarise(hours = sum(hours)) %>%
    mutate(proportion_purse_seine = hours / sum(hours))

# Extract Demersal Trawl data (not available from SANBI yet)
# Need to confirm the format and values of the data are similar to the other SANBI layers
dt_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Demersal_Trawl_Intensity_{crs}.tif",
    habitats
)
dt_data <- sanbi_proportion_effort(dt_intensity, habitats, "sum", "demersal_trawl")

ggplot() +
    geom_spatraster(data = dt_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract West Coast Rock Lobster data (catch - kg/km^2).
# Commercial West Coast Rock Lobster trapping mainly occurs at depths greater than 100m over rocky habitats.
# We distribute the effort evenly across rocky habitat areas at depths greater than 100m. This is then used to calculate the proportion of effort across the domain habitat types.
GEBCO <- rast("../Shared data/GEBCO_2020.nc")
GEBCO <- crop(GEBCO, round(ext(wcrl_intensity), digits = 3))
GEBCO[GEBCO > -100] <- NA
GEBCO[] <- 1

rocky_greater_100 <- mask(GEBCO, habitats[habitats$Habitat == "rock", ])
wcrl_data <- sanbi_proportion_effort(
    rocky_greater_100,
    habitats,
    "sum",
    "West_Coast_Rock_Lobster_traps"
)
# To avoid any errors where the raster and habitat data don't perfectly line up, any effort data for other habitat types has been removed before recalculating the proportions.
wcrl_data[wcrl_data$Habitat != "rock", ]$West_Coast_Rock_Lobster_traps <- 0
wcrl_data <- mutate(wcrl_data, proportion_West_Coast_Rock_Lobster_traps = West_Coast_Rock_Lobster_traps / sum(West_Coast_Rock_Lobster_traps))

# Extract recreational fishing gear spatial effort data
# Combination of linefishery spatial effort and recreational-shore based effort
rlf_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif",
    habitats
)
rlf_intensity <- subst(rlf_intensity, NA, 0)

rsb_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity_{crs}.tif",
    habitats
)
rsb_intensity <- subst(rsb_intensity, NA, 0)

rec_intensity <- rlf_intensity + rsb_intensity
rec_intensity <- subst(rec_intensity, 0, NA)
rec_data <- sanbi_proportion_effort(
    rec_intensity, habitats,
    "sum",
    "recreational_fishing_gears"
)

# Extract small scale lines spatial effort data
ssl_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif",
    habitats
)
ssl_data <- sanbi_proportion_effort(
    ssl_intensity,
    habitats,
    "sum",
    "small_scale_lines"
)
ggplot() +
    geom_spatraster(data = ssl_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract subsistence fishing gear spatial effort data
ssf_intensity <- format_sanbi_raster(
    "../../Spatial Data/fishing_effort_data/Subsistence_Fishing_Intensity/Subsistence_Fishing_Intensity_{crs}.tif",
    habitats
)
ssf_data <- sanbi_proportion_effort(
    ssf_intensity,
    habitats,
    "sum",
    "subsistence_fishing_gear"
)



# Combine spatial activity datasets
habitat_activity <- habitats %>%
    # Insert purse seine data
    left_join(., mw_data, by = c("Habitat", "Shore")) %>%
    left_join(., lf_data, by = c("Habitat", "Shore")) %>%
    left_join(., sj_data, by = c("Habitat", "Shore")) %>%
    left_join(., pll_data, by = c("Habitat", "Shore"))
# Insert demersal longline data

habitat_activity <- habitat_activity %>%
    pivot_longer(cols = !c(Habitat, Shore, geometry), names_to = "gear_type", values_to = "pressure") %>%
    mutate(pressure = if_else(is.na(pressure), 0, pressure)) %>% # missing data = 0 fishing
    group_by(gear_type) %>% # Have to group by gear type to sum each row to 1
    mutate(proportion = pressure / sum(pressure)) %>%
    ungroup()

ggplot() +
    geom_col(data = habitat_activity, aes(x = Habitat, y = proportion, fill = gear_type)) +
    facet_wrap(~Shore) +
    ylab("Proportion of domain-wide effort per year") +
    theme_minimal()

ggplot() +
    geom_spatraster(data = mw_intensity, aes(fill = OID), na.rm = TRUE) +
    scale_fill_viridis_c() +
    new_scale_fill() +
    geom_sf(data = habitats, aes(fill = Habitat), alpha = 0.5) +
    scale_fill_viridis_d(option = "B")
