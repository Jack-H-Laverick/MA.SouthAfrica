library(ggplot2)
library(terra)
library(sf)
library(tidyterra)
library(exactextractr)
source("./R scripts/fish/fisheries_data_functions.R")
source("./R scripts/@_Region file.R")


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

domain_rough_bound <- ext(lims)


# Load Midwater Trawl intensity data - need to acquire demersal trawl data - hours of trawling
mw_intensity <- "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity.tif"
mw_intensity <- crop(rast(mw_intensity), domain_rough_bound)
mw_intensity <- project(mw_intensity, "epsg:4326", method = "near") # Using nearest neighbour method because values are somewhat discrete
mw_intensity <- crop(mw_intensity, habitats)
mw_intensity <- as.numeric(mw_intensity)
mw_intensity <- subst(mw_intensity, NA, 0) # Replace NA values with 0 because there is no fishing in that area.
mw_data <- extract_habitat_data(mw_intensity, habitats, "mean", "midwater_trawl")
mw_data$midwater_trawl <- ifelse(is.na(mw_data$midwater_trawl), 0, mw_data$midwater_trawl)
mw_data <- mw_data %>% mutate(proportion = midwater_trawl / sum(midwater_trawl))

ggplot() +
    geom_spatraster(data = mw_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Load spatial data for nets including small scale
## Load Gillnets
gn_intensity <- "../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity.tif"
gn_intensity <- trim(rast(gn_intensity))
gn_intensity <- as.numeric(gn_intensity)
gn_intensity <- project(gn_intensity, "epsg:4326", method = "near")
gn_intensity <- subst(gn_intensity, NA, 0)
gn_data <- extract_habitat_data(gn_intensity, habitats, "mean", "gill_nets")

## Load beach seine nets
bs_intensity <- "../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity.tif"
bs_intensity <- trim(rast(bs_intensity))
bs_intensity <- as.numeric(bs_intensity)
bs_intensity <- project(bs_intensity, "epsg:4326", method = "near")
bs_intensity <- subst(bs_intensity, NA, 0)
bs_data <- extract_habitat_data(bs_intensity, habitats, "mean", "beach_seine")



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
