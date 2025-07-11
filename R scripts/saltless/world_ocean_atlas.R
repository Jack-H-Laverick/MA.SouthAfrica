# Extract variable data from world ocean atlas for nitrate for winter and summer
# Concentrations from WOA are in micromol/kg of seawater

library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(glue)
library(docstring)

source("./R scripts/@_Region file.R")

domain <- readRDS("./Objects/Domains.rds") %>%
    st_transform(crs = 4326) %>%
    mutate(area_proportion = area / sum(area))

winter_months <- c("Nov", "Dec", "Jan", "Feb") # Winter months are Nov-Feb despite South Africa being in southern hemisphere to align with existing StrathE2E setup.
summer_months <- c("May", "Jun", "Jul", "Aug") # Summer months are May-Aug despite South Africa being in southern hemisphere to align with existing StrathE2E setup.
variable <- c("nitrate")
depth_label <- c("shallow", "deep")
depth_ranges <- c(
    "shallow_inshore" = paste0("0:", abs(domain[domain$Shore == "Inshore", ]$Elevation)),
    "shallow_offshore" = paste0("0:", SDepth),
    "deep" = paste0(SDepth, ":", abs(domain[domain$Shore == "Offshore", ]$Elevation))
)

# Need to extract data for each month and depth level
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
        month_num = sprintf("%02d", match(month, month.abb))
    ) %>%
    select(c(season, month, month_num, variable, depth))

not_all_na <- \(r) {
    terra::global(r, fun = "notNA") > 0
}

extract_domain_woa <- function(domain, directory, depth_ranges, month, variable, depth, nc_var = "mn") {
    message(glue("Extracting WOA data from depth {depth} for month {month}"))
    fn <- list.files(directory) %>%
        .[str_detect(., str_sub(variable, 1, 1))] %>%
        .[str_detect(., glue("n{month}"))] %>%
        paste0(directory, .)
    terr <- rast(fn)
    terr_var <- varnames(terr)
    terr <- project(terr, "epsg:4326")

    # Find mean variable
    terr_var <- terr_var[str_detect(terr_var, nc_var)]
    terr <- terr[terr_var]

    # Remove depth layers that contain no data
    terr <- select(terr, names(terr)[not_all_na(terr)])

    # For deep layer only the offshore area data is used and a single mean value is calculated.
    if (depth == "deep") {
        domain_l <- domain[domain$Shore == "Offshore", ]
        terr <- crop(terr, domain_l, mask = TRUE) # Mask raster cells not touching the domain

        depth_range <- depth_ranges[depth]
        depth_range <- eval(parse(text = depth_range))

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

        # domain_vol <- sum(domain_l$area) * (depth_range[2] - depth_range[1])
        volume_mean <- target_layer_mean # / 1000 / domain_vol # Convert from micromolar to millimolar then divide by target domain volume
        volume_stdev <- target_layer_std # / 1000 / domain_vol

        # For shallow layer the mean is calculated for the offshore and inshore shallow layers separately as they have different depths
    } else if (depth == "shallow") {
        domain_vols <- c("inshore" = NA, "offshore" = NA)
        volume_means <- c("inshore" = NA, "offshore" = NA)
        volume_stds <- c("inshore" = NA, "offshore" = NA)

        for (i in seq_along(volume_means)) {
            zone <- names(volume_means[i])
            domain_l <- domain[str_detect(domain$Shore, regex(zone, ignore_case = TRUE)), ]
            terr <- crop(terr, domain_l, mask = TRUE) # Mask raster cells not touching the domain

            depth_range <- depth_ranges[names(depth_ranges)[str_detect(names(depth_ranges), zone)]]
            depth_range <- eval(parse(text = depth_range))

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

            domain_vols[i] <- sum(domain_l$area) * (depth_range[2] - depth_range[1])
            volume_means[i] <- target_layer_mean # / domain_vol  / 1000 Convert from micromolar to millimolar then divide by target domain volume
            volume_stds[i] <- target_layer_std # / domain_vol / 1000
        }

        # # Combine inshore and offshore statistics using area-weighting
        volume_mean <- weighted.mean(volume_means, domain_vols)
        volume_stdev <- weighted.mean(volume_stds, domain_vols)
    } else {
        stop(glue("Depth {depth} not found, only shallow or deep accepted."))
    }

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

# Calculate single season statistics
season_mean <- results %>%
    group_by(season, depth) %>%
    summarise(mean_conc = mean(mean_conc), std_conc = mean(sd_conc))

write.csv(season_mean, "./Objects/woa23_nitrate_concentrations.csv", row.names = FALSE)
