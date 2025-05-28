library(ggplot2)
library(terra)
library(sf)
library(tidyterra)
library(exactextractr)
source("./R scripts/fish/fisheries_data_functions.R")


# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

extract_habitat_data <- function(raster, habitats, fun, name) {
    extracted <- raster %>%
        exact_extract(habitats, fun = fun, append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        rename(!!name := fun)

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

# Load Purse Seine fishing intensity data - need to acquire

# Load Midwater Trawl intensity data - need to acquire demersal trawl data - hours of trawling
mw_intensity <- "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity.tif"
mw_intensity <- trim(rast(mw_intensity))
mw_intensity <- as.numeric(mw_intensity)
mw_intensity <- project(mw_intensity, "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
mw_intensity <- subst(mw_intensity, NA, 0) # Replace NA values with 0 because there is no fishing in that area.
mw_data <- extract_habitat_data(mw_intensity, habitats, "mean", "midwater_trawl")
# mw_data$coverage <- terra::extract(mw_intensity, habitats, fun = prop_covered)$OID

# Load linefishery intensity data - catch in kg/area ()
lf_intensity <- "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity.tif"
lf_intensity <- trim(rast(lf_intensity))
lf_intensity <- as.numeric(lf_intensity)
lf_intensity <- project(lf_intensity, "epsg:4326", method = "near")
lf_intensity <- subst(lf_intensity, NA, 0)
lf_data <- extract_habitat_data(lf_intensity, habitats, "mean", "linefishery")
# lf_coverage <- exact_extract()

# Load squidjig intensity data - catch in kg/area
sj_intensity <- "../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Fishery_Intensity.tif"
sj_intensity <- trim(rast(sj_intensity))
sj_intensity <- as.numeric(sj_intensity)
sj_intensity <- project(sj_intensity, "epsg:4326", method = "near")
sj_intensity <- subst(sj_intensity, NA, 0)
sj_data <- extract_habitat_data(sj_intensity, habitats, "mean", "squid_jig")
# sj_data$coverage <- terra::extract(sj_intensity, habitats, fun = prop_covered)$OID

# Load pelagic longline - hooks/area
pll_intensity <- "../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity.tif"
pll_intensity <- trim(rast(pll_intensity))
pll_intensity <- as.numeric(pll_intensity)
pll_intensity <- project(pll_intensity, "epsg:4326", method = "near")
pll_intensity <- subst(pll_intensity, NA, 0)
pll_data <- extract_habitat_data(pll_intensity, habitats, "mean", "pelagic_longline")
# pll_coverage <- exact_extract(pll_intensity, habitats)
# pll_data$coverage <- sapply(pll_coverage, prop_covered)


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
