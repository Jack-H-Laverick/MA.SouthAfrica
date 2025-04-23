# Extract variable data from world ocean atlas for nitrate and ammonia for winter and summer
# Concentrations from WOA are in micromol/kg of seawater

library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(tidyverse)

source("./R scripts/@_Region file.R")

domain <- readRDS("./Objects/Domains.rds") %>% st_transform(crs = 4326)

winter_months <- c("Nov", "Dec", "Jan", "Feb")
summer_months <- c("May", "Jun", "Jul", "Aug")
variable <- c("nitrate", "ammonia")
depth_label <- c("shallow", "deep")
depth_ranges <- c("shallow" = paste0("0:", SDepth), "deep" = paste0(SDepth, ":", DDepth))

results <- expand.grid(c(winter_months, summer_months), variable, depth_label, stringsAsFactors = FALSE)
colnames(results) <- c("month", "variable", "depth")

results <- results %>%
    mutate(
        season = case_when(
            month %in% winter_months ~ "winter",
            month %in% summer_months ~ "summer"
        )
    ) %>%
    mutate(
        depth_range = case_when(
            depth == "shallow" ~ depth_ranges["shallow"],
            depth == "deep" ~ depth_ranges["deep"]
        )
    ) %>%
    select(c(season, month, variable, depth, depth_range))

not_all_na <- \(r) {
    terra::global(r, fun = "notNA") > 0
}

extract_domain_woa <- function(domain, directory, depth_ranges, month, variable, depth, nc_var = "mn") {
    if (depth == "deep") {
        domain <- domain[domain$Shore == "Offshore", ]
    }
    depth_range <- eval(parse(text = depth_ranges[depth]))

    fn <- list.files(directory) %>%
        .[str_detect(., str_sub(variable, 1, 1))] %>%
        .[str_detect(., month)] %>%
        paste0(directory, .)
    terr <- rast(fn)
    terr_var <- varnames(terr)
    terr <- project(terr, "epsg:4326")
    terr <- crop(terr, domain, mask = TRUE)

    terr_var <- terr_var[str_detect(terr_var, nc_var)]
    terr <- terr[terr_var]

    # Remove depth layers that contain no data
    terr <- select(terr, names(terr)[not_all_na(terr)])

    depths <- names(terr) %>%
        str_split_i(., "(=)", 2) %>%
        as.numeric()
    upper_depth <- head(depth_range, n = 1)
    lower_depth <- tail(depth_range, n = 1)

    target_depths <- names(terr)[depths > upper_depth & depths < lower_depth]

    target_layer_mean <- select(terr, target_depths) %>%
        values(., na.rm = TRUE) %>%
        mean()

    domain_vol <- sum(domain$area) * (lower_depth - upper_depth)

    volume_mean <- target_layer_mean / 1000 / domain_vol # Convert from micromolar to millimolar then divide by target domain volume

    return(volume_mean)
}

pmap(results[, c("month", "variable", "depth")], function(month, variable, depth) {
    extract_domain_woa(
        domain = domain,
        directory = "../../Spatial Data/world_ocean_atlas/",
        depth_ranges = depth_ranges,
        month = month,
        variable = variable,
        depth = depth
    )
})

# nitrate_terr <- rast("../../Spatial Data/world_ocean_atlas/woa23_all_n00_01.nc") %>%
#     project(., "epsg:4326") # Update to use crs from region file
# nitrate_terr <- crop(nitrate_terr, domain, mask = TRUE) # Remove areas of nitrate_terr that don't touch domain polygons

# nitrate_mn <- nitrate_terr["n_mn"]

# depths <- names(nitrate_mn) %>%
#     str_split_i(., "(=)", 2) %>%
#     as.numeric()
# inshore_depths <- names(nitrate_mn)[depths < 50]
# offshore_depths <- names(nitrate_mn)[depths > 50 & depths < 800]

# inshore_nitrate <- select(nitrate_mn, inshore_depths) %>% mean()
# offshore_nitrate <- select(nitrate_mn, offshore_depths) %>% mean()

# ggplot() +
#     geom_spatraster(data = mean(inshore_nitrate)) +
#     geom_sf(data = domain, alpha = 0.3)

# ggplot() +
#     geom_spatraster(data = offshore_nitrate) +
#     geom_sf(data = domain, alpha = 0.3)

# inshore_nitrate <- mean(values(inshore_nitrate), na.rm = TRUE)
# offshore_nitrate <- mean(values(offshore_nitrate), na.rm = TRUE)

# # Calculate concentrations as mM/m^3
# # Assuming that micromol/kg concentrations are uM we divide concentrations by 1000 to get mM
# # Thickness of inshore volume = 50m, thickness of offshore volume = 750m

# inshore_nitrate <- inshore_nitrate / 1000
# inshore_volume <- filter(domain, Shore == "Inshore")$area * SDepth
# inshore_nitrate_m3 <- inshore_nitrate / inshore_volume

# offshore_nitrate <- offshore_nitrate / 1000
# offshore_volume <- filter(domain, Shore == "Offshore")$area * (DDepth - SDepth)
# offshore_nitrate_m3 <- offshore_nitrate / offshore_volume
