library(ggplot2)
library(terra)
library(sf)
library(tidyterra)
library(exactextractr)
source("./R scripts/fish/fisheries_data_functions.R")


# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

extract_habitat_proportion <- function(raster, habitats, fun, name) {
    extracted <- raster %>%
        exact_extract(habitats, fun = fun, append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        rename(!!name := fun)

    return(extracted)
}

# Load Purse Seine fishing intensity data - need to acquire

# Load Midwater Trawl intensity data - need to acquire demersal trawl data
mw_intensity <- "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity.tif"
mw_intensity <- trim(rast(mw_intensity))
mw_intensity <- as.numeric(mw_intensity)
mw_intensity <- project(mw_intensity, "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
mw_data <- extract_habitat_proportion(mw_intensity, habitats, "mean", "midwater_trawl")

# Load linefishery intensity data
lf_intensity <- "../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity.tif"
lf_intensity <- trim(rast(lf_intensity))
lf_intensity <- as.numeric(lf_intensity)
lf_intensity <- project(lf_intensity, "epsg:4326", method = "near")
lf_data <- extract_habitat_proportion(lf_intensity, habitats, "mean", "linefishery")

# Load squidjig intensity data
sj_intensity <- "../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Fishery_Intensity.tif"
sj_intensity <- trim(rast(sj_intensity))
sj_intensity <- as.numeric(sj_intensity)
sj_intensity <- project(sj_intensity, "epsg:4326", method = "near")
sj_data <- extract_habitat_proportion(sj_intensity, habitats, "mean", "squid_jig")

# Load pelagic longline
pll_intensity <- "../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity.tif"
pll_intensity <- trim(rast(pll_intensity))
pll_intensity <- as.numeric(pll_intensity)
pll_intensity <- project(pll_intensity, "epsg:4326", method = "near")
pll_data <- extract_habitat_proportion(pll_intensity, habitats, "mean", "pelagic_longline")

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
