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
    extracted <- raster %>%
        exact_extract(habitats, fun = fun, append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        rename(!!name := fun)

    return(extracted)
}

sanbi_proportion_effort <- function(raster_fn, habitats, fun, name) {
    raster <- project(rast(raster_fn), "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
    raster <- crop(raster, habitats)
    raster <- as.numeric(raster)
    raster <- subst(raster, 0, NA) # Replace NA values with 0 because there is no fishing in that area.

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

domain_rough_bound <- ext(lims)

# Load Midwater Trawl intensity data - need to acquire demersal trawl data - hours of trawling
mw_data <- sanbi_proportion_effort(
    "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity.tif",
    habitats,
    "sum",
    "midwater_trawl"
)

ggplot() +
    geom_spatraster(data = mw_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4)
scale_fill_viridis_c()

# Load spatial data for nets including small scale
## Load Gillnets
gn_intensity <- "../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity.tif"
gn_intensity <- project(rast(gn_intensity), "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
gn_intensity <- crop(gn_intensity, habitats)
gn_intensity <- as.numeric(gn_intensity)
gn_intensity <- subst(gn_intensity, NA, 0) # Set missing values to 0 intermediately to add beach seine data

## Load beach seine nets
bs_intensity <- "../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity.tif"
bs_intensity <- project(rast(bs_intensity), "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
bs_intensity <- crop(bs_intensity, habitats)
bs_intensity <- as.numeric(bs_intensity)
bs_intensity <- subst(bs_intensity, NA, 0) # Set missing values to 0 intermediately to add gillnet data

net_intensity <- gn_intensity + bs_intensity
net_intensity <- subst(net_intensity, 0, NA) # Set 0 values to NA to just extract from regions with effort.
net_data <- extract_habitat_data(net_intensity, habitats, "sum", "nets")
net_data$nets <- ifelse(is.na(net_data$nets), 0, net_data$nets)
net_data <- net_data %>% mutate(proportion_nets := nets / sum(nets))

ggplot() +
    geom_spatraster(data = net_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract Squid Jig data
sj_data <- sanbi_proportion_effort(
    "../../Spatial Data/fishing_effort_data/Squid_Jig_Intensity/Squid_Jig_Intensity/Squid_Jig_Intensity.tif",
    habitats,
    "sum",
    "squid_jig"
)

# Extract Linefishery data
lf_data <- sanbi_proportion_effort(
    "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linfish_Intensity.tif",
    habitats,
    "sum",
    "linefishery"
)

# Collate pelagic and demersal longline data from GFW

# Collate purse seine data from GFW

# Extract Demersal Trawl data (not available from SANBI yet)
# Need to confirm the format and values of the data are similar to the other SANBI layers
dt_data <- sanbi_proportion_effort(
    "../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Demersal_Trawl_Intensity.tif",
    habitats,
    "sum",
    "demersal_trawl"
)

# Load pelagic longline - hooks/area
pll_intensity <- "../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity.tif"
pll_intensity <- trim(rast(pll_intensity))
pll_intensity <- as.numeric(pll_intensity)
pll_intensity <- project(pll_intensity, "epsg:4326", method = "near")
pll_intensity <- subst(pll_intensity, NA, 0)
pll_data <- extract_habitat_data(pll_intensity, habitats, "mean", "pelagic_longline")
# pll_coverage <- exact_extract(pll_intensity, habitats)
# pll_data$coverage <- sapply(pll_coverage, prop_covered)

# Can include recreational fishing map - recreational fishers per km^2, beach seine - rights / km^2, gillnets - rights / km^2

# Load demersal longline data - need to acquire

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
