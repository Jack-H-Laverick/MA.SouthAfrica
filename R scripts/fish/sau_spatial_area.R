library(sf)
library(terra)
library(glue)
library(exactextractr)

domain <- readRDS("./Objects/Domains.rds")
domain <- st_union(domain)

# Retrieve just the area that SAU uses for the Atlantic and Cape region, which is the intersection between the SA EEZ and the FAO zone 47
sau_west_area <- read_sf("./Objects/sau_atlantic_cape_region.gpkg")

# ggplot() +
#     geom_sf(data = sa_eez) +
#     geom_sf(data = sau_west_area) +
#     geom_sf(data = habitats)

gfw_polygon_effort <- function(file, polygons) {
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
        exact_extract(polygons, fun = "sum") %>% # Sum fishing hours within habitat types
        pivot_longer(
            cols = matches("\\d"),
            names_to = "year",
            values_to = "hours"
        ) %>%
        mutate(year = as.numeric(str_split_i(year, "(?<=[=])", i = 2))) %>%
        mutate(variable = glue("{flag}_{variable}")) %>%
        select(c(year, variable, hours))

    return(extracted_data)
}

format_sanbi_raster <- function(raster_fn, habitats) {
    #' Format a SANBI 'cumulative stress index' raster into usable values that contain just positive effort.
    raster <- rast(raster_fn)
    raster <- crop(raster, habitats)
    raster <- as.numeric(raster)
    raster <- subst(raster, 0, NA) # Replace 0 values with NA because there is no fishing in that area.

    return(raster)
}

# Determine the proportion of activity within the sau_west_area that is within the domain for each gear type to scale SAU catch data
#
# Midwater trawls
mw_intensity_fn <- glue("../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif")

mw_intensity_domain <- format_sanbi_raster(mw_intensity_fn, domain)
mw_activity_domain <- global(mw_intensity_domain, fun = "sum", na.rm = TRUE)
mw_intensity_sau_area <- format_sanbi_raster(mw_intensity_fn, sau_west_area)
mw_activity_sau_area <- global(mw_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_mw <- mw_activity_domain / mw_activity_sau_area

#
# Nets including small scale
gn_fn <- glue("../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity_{crs}.tif")
bs_fn <- glue("../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity_{crs}.tif")

## Combining gillnet and beach seine activity data for domain region
gn_intensity_domain <- format_sanbi_raster(gn_fn, domain)
gn_intensity_domain <- subst(gn_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
bs_intensity_domain <- format_sanbi_raster(bs_fn, domain)
bs_intensity_domain <- subst(bs_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
net_intensity_domain <- gn_intensity_domain + bs_intensity_domain
net_activity_domain <- global(net_intensity_domain, fun = "sum", na.rm = TRUE)

gn_intensity_sau_area <- format_sanbi_raster(gn_fn, sau_west_area)
gn_intensity_sau_area <- subst(gn_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
bs_intensity_sau_area <- format_sanbi_raster(bs_fn, sau_west_area)
bs_intensity_sau_area <- subst(bs_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
net_intensity_sau_area <- gn_intensity_sau_area + bs_intensity_sau_area
net_activity_sau_area <- global(net_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_nets <- net_activity_domain / net_activity_sau_area

#
# Squid jig
sj_fn <- glue("../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Fishery_Intensity_{crs}.tif")
sj_intensity_domain <- format_sanbi_raster(sj_fn, domain)
sj_activity_domain <- global(sj_intensity_domain, fun = "sum", na.rm = TRUE)
sj_intensity_sau_area <- format_sanbi_raster(sj_fn, sau_west_area)
sj_activity_sau_area <- global(sj_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_sj <- sj_activity_domain / sj_activity_sau_area

#
# Longline
domain_longline_data <- data.table::rbindlist(list(
    gfw_polygon_effort("fleet_fishing_ZAF-demersal_longline_domain.nc", domain),
    gfw_polygon_effort("fleet_fishing_ZAF-pelagic_longline_domain.nc", domain)
)) %>%
    group_by(year) %>%
    summarise(hours = sum(hours)) %>%
    ungroup() %>%
    summarise(hours = mean(hours))

sau_longline_data <- data.table::rbindlist(list(
    gfw_polygon_effort("fleet_fishing_ZAF-demersal_longline_sau_area.nc", sau_west_area),
    gfw_polygon_effort("fleet_fishing_ZAF-pelagic_longline_sau_area.nc", sau_west_area)
)) %>%
    group_by(year) %>%
    summarise(hours = sum(hours)) %>%
    ungroup() %>%
    summarise(hours = mean(hours))

prop_sau_longline <- as.numeric(domain_longline_data / sau_longline_data)

#
# Purse seine
domain_purse_seine_data <- gfw_polygon_effort("fleet_fishing_ZAF-purse_seine_domain.nc", domain) %>%
    group_by(year) %>%
    summarise(hours = sum(hours)) %>%
    ungroup() %>%
    summarise(hours = mean(hours))

sau_purse_seine_data <- gfw_polygon_effort("fleet_fishing_ZAF-purse_seine_sau_area.nc", sau_west_area) %>%
    group_by(year) %>%
    summarise(hours = sum(hours)) %>%
    ungroup() %>%
    summarise(hours = mean(hours))

prop_sau_purse_seine <- as.numeric(domain_purse_seine_data / sau_purse_seine_data)

# West Coast Rock Lobster traps
# Assume that all West Coast Rock Lobster traps occurs within the domain as the South Coast Rock Lobster dominates the southern coast of South Africa
prop_sau_wcrl <- 1

#
# Recreational fishing gear
rlf_fn <- glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif")
rsb_fn <- glue("../../Spatial Data/fishing_effort_data/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity_{crs}.tif")

## Combining recreational boat-based linefishing and shore-based activity data for domain region
rlf_intensity_domain <- format_sanbi_raster(rlf_fn, domain)
rlf_intensity_domain <- subst(rlf_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
rsb_intensity_domain <- format_sanbi_raster(rsb_fn, domain)
rsb_intensity_domain <- subst(rsb_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
rec_intensity_domain <- rlf_intensity_domain + rsb_intensity_domain
rec_activity_domain <- global(rec_intensity_domain, fun = "sum", na.rm = TRUE)

rlf_intensity_sau_area <- format_sanbi_raster(rlf_fn, sau_west_area)
rlf_intensity_sau_area <- subst(rlf_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
rsb_intensity_sau_area <- format_sanbi_raster(rsb_fn, sau_west_area)
rsb_intensity_sau_area <- subst(rsb_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
rec_intensity_sau_area <- rlf_intensity_sau_area + rsb_intensity_sau_area
rec_activity_sau_area <- global(rec_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_rec <- rec_activity_domain / rec_activity_sau_area

#
# Small scale lines
ssl_fn <- glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif")
ssl_intensity_domain <- format_sanbi_raster(ssl_fn, domain)
ssl_activity_domain <- global(ssl_intensity_domain, fun = "sum", na.rm = TRUE)
ssl_intensity_sau_area <- format_sanbi_raster(ssl_fn, sau_west_area)
ssl_activity_sau_area <- global(ssl_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_ssl <- ssl_activity_domain / ssl_activity_sau_area

#
# Subsistence fishing gear
ssf_fn <- glue("../../Spatial Data/fishing_effort_data/Subsistence_Fishing_Intensity/Subsistence_Fishing_Intensity_{crs}.tif")
ssf_intensity_domain <- format_sanbi_raster(ssf_fn, domain)
ssf_activity_domain <- global(ssf_intensity_domain, fun = "sum", na.rm = TRUE)
ssf_intensity_sau_area <- format_sanbi_raster(ssf_fn, sau_west_area)
ssf_activity_sau_area <- global(ssf_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_ssf <- ssf_activity_domain / ssf_activity_sau_area
