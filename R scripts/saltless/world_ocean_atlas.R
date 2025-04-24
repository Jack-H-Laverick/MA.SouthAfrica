# Extract variable data from world ocean atlas for nitrate for winter and summer
# Concentrations from WOA are in micromol/kg of seawater

library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(tidyverse)
library(glue)

source("./R scripts/@_Region file.R")

domain <- readRDS("./Objects/Domains.rds") %>% st_transform(crs = 4326)

winter_months <- c("Nov", "Dec", "Jan", "Feb")
summer_months <- c("May", "Jun", "Jul", "Aug")
variable <- c("nitrate")
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
    mutate(
        month_num = sprintf("%02d", match(month, month.abb))
    ) %>%
    select(c(season, month, month_num, variable, depth, depth_range))

not_all_na <- \(r) {
    terra::global(r, fun = "notNA") > 0
}

extract_domain_woa <- function(domain, directory, depth_ranges, month, variable, depth, nc_var = "mn") {
    message(glue("Extracting WOA data from depth {depth} for month {month}"))
    if (depth == "deep") {
        domain <- domain[domain$Shore == "Offshore", ]
    } else {
        domain$Elevation <- rep(domain[domain$Shore == "Inshore", ]$Elevation, length(domain$Elevation))
    }
    depth_range <- eval(parse(text = depth_ranges[depth]))

    fn <- list.files(directory) %>%
        .[str_detect(., str_sub(variable, 1, 1))] %>%
        .[str_detect(., glue("n{month}"))] %>%
        paste0(directory, .)
    terr <- rast(fn)
    terr_var <- varnames(terr)
    terr <- project(terr, "epsg:4326")
    terr <- crop(terr, domain, mask = TRUE) # Mask raster cells not touching the domain

    # Find mean variable
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

    # Extract all values from the target_depth layers of the cropped raster and take the mean of the entire water volume
    target_layer_mean <- select(terr, target_depths) %>%
        values(.) %>%
        mean(., na.rm = TRUE)

    target_layer_std <- select(terr, target_depths) %>%
        values(.) %>%
        sd(., na.rm = TRUE)

    domain_vol <- sum(domain$area) * abs(mean(domain$Elevation))
    volume_mean <- target_layer_mean / 1000 / domain_vol # Convert from micromolar to millimolar then divide by target domain volume
    volume_stdev <- target_layer_std / 1000 / domain_vol

    return(c(volume_mean, volume_stdev))
}

results[, c("mean_conc", "sd_conc")] <- pmap(results[, c("month_num", "variable", "depth")], function(month_num, variable, depth) {
    extract_domain_woa(
        domain = domain,
        directory = "../../Spatial Data/world_ocean_atlas/",
        depth_ranges = depth_ranges,
        month = month_num,
        variable = variable,
        depth = depth
    )
}) %>%
    unlist()

season_mean <- results %>%
    group_by(season, depth) %>%
    summarise(mean_conc = mean(mean_conc), std_conc = mean(sd_conc))
