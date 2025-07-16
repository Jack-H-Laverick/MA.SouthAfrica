# Script for calculating spatial distribution of fishing across the model habitat types,
# mainly using SANBI relative effort raster data.

source("./R scripts/@_Region file.R")
source("./R scripts/@_model_config.R")
source("./R scripts/fish/fishing_spatial_functions.R")

# Load habitat map
habitats <- readRDS("./Objects/Habitats.rds")

# Load Midwater Trawl intensity data - hours of trawling
mw_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif"),
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

# Load spatial data for nets - number of rights
## Load Gillnets
gn_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity_{crs}.tif"),
    habitats
)
gn_intensity <- subst(gn_intensity, NA, 0) # Set NA values to 0 for addition of second layer

## Load beach seine nets
bs_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity_{crs}.tif"),
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
tpl_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Tuna_Pole_Intensity/Tuna_Pole_Intensity_{crs}.tif"),
    habitats
)
tpl_intensity <- tpl_intensity["Tuna_Pole_Intensity_1"] # Select first band of data containing continuous values
tpl_data <- sanbi_proportion_effort(
    tpl_intensity,
    habitats,
    "sum",
    "pole_and_line"
)
ggplot() +
    geom_spatraster(data = tpl_intensity, aes(fill = Tuna_Pole_Intensity_1)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract Squid Jig data
sj_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Intensity_{crs}.tif"),
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

# Collate pelagic and demersal longline data from SANBI layers
# Load inshore demersal trawl activity
pll_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity_{crs}.tif"),
    habitats
)
pll_intensity <- subst(pll_intensity, NA, 0) # Set NA values to 0 for addition of second layer

## Load offshore demersal trawl activity
dmll_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Hake_Longline_Intensity/Hake_Longline_Intensity_{crs}.tif"),
    habitats
)
dmll_intensity <- subst(dmll_intensity, NA, 0) # Set NA values to 0 for addition of second layer
dmll_intensity <- dmll_intensity["Hake_Longline_Intensity_1"]

longline_intensity <- pll_intensity + dmll_intensity
longline_intensity <- subst(longline_intensity, 0, NA) # Set 0 values to NA to just extract from regions with effort.
longline_data <- sanbi_proportion_effort(longline_intensity, habitats, "sum", "longline")

ggplot() +
    geom_spatraster(data = longline_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Collate purse seine data from GFW
smpl_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Small_Pelagic_Intensity/Small_Pelagic_Intensity_{crs}.tif"),
    habitats
)
smpl_intensity <- smpl_intensity["Small_Pelagic_Intensity_1"]
smpl_data <- sanbi_proportion_effort(
    smpl_intensity,
    habitats,
    "sum",
    "purse_seine"
)
ggplot() +
    geom_spatraster(data = smpl_intensity, aes(fill = Small_Pelagic_Intensity_1)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract Demersal Trawl data (data separated into inshore and offshore regions)
# Need to confirm the format and values of the data are similar to the other SANBI layers
# Load inshore demersal trawl activity
intr_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Inshore_Intensity/Trawl_Inshore_Intensity_{crs}.tif"),
    habitats
)
intr_intensity <- subst(intr_intensity, NA, 0) # Set NA values to 0 for addition of second layer
intr_intensity <- intr_intensity["Trawl_Inshore_Intensity_1"]

## Load offshore demersal trawl activity
oftr_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Offshore_Intensity/Trawl_Offshore_Intensity_{crs}.tif"),
    habitats
)
oftr_intensity <- subst(oftr_intensity, NA, 0) # Set NA values to 0 for addition of second layer
oftr_intensity <- oftr_intensity["Trawl_Offshore_Intensity_1"]

dmtrwl_intensity <- intr_intensity + oftr_intensity
dmtrwl_intensity <- subst(dmtrwl_intensity, 0, NA) # Set 0 values to NA to just extract from regions with effort.
dmtrwl_data <- sanbi_proportion_effort(dmtrwl_intensity, habitats, "sum", "demersal_trawl")

ggplot() +
    geom_spatraster(data = dmtrwl_intensity, aes(fill = Trawl_Inshore_Intensity_1)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract West Coast Rock Lobster data (catch - kg/km^2).
# Commercial West Coast Rock Lobster trapping mainly occurs at depths greater than 100m over rocky habitats.
# We distribute the effort evenly across rocky habitat areas at depths greater than 100m. This is then used to calculate the proportion of effort across the domain habitat types.
GEBCO <- rast("../Shared data/GEBCO_2020.nc")
GEBCO <- crop(GEBCO, round(ext(habitats), digits = 3))
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
    glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif"),
    habitats
)
rlf_intensity <- subst(rlf_intensity, NA, 0)

rsb_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity_{crs}.tif"),
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

ggplot() +
    geom_spatraster(data = rec_intensity, aes(fill = OID)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Extract small scale lines spatial effort data
ssl_intensity <- format_sanbi_raster(
    glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif"),
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
    glue("../../Spatial Data/fishing_effort_data/Subsistence_Harvest_Intensity/Subsistence_Harvest_Intensity_{crs}.tif"),
    habitats
)
ssf_intensity <- ssf_intensity["Subsistence_Harvest_Intensity_1"]
ssf_data <- sanbi_proportion_effort(
    ssf_intensity,
    habitats,
    "sum",
    "subsistence_fishing_gear"
)
ggplot() +
    geom_spatraster(data = ssf_intensity, aes(fill = Subsistence_Harvest_Intensity_1)) +
    geom_sf(data = habitats, aes(color = Habitat), alpha = 0.4) +
    scale_fill_viridis_c()

# Combine spatial activity datasets
habitat_activity <- habitats %>%
    left_join(., mw_data, by = c("Habitat", "Shore")) %>%
    left_join(., net_data, by = c("Habitat", "Shore")) %>%
    left_join(., tpl_data, by = c("Habitat", "Shore")) %>%
    left_join(., sj_data, by = c("Habitat", "Shore")) %>%
    left_join(., longline_data, by = c("Habitat", "Shore")) %>%
    left_join(., smpl_data, by = c("Habitat", "Shore")) %>%
    left_join(., dmtrwl_data, by = c("Habitat", "Shore")) %>%
    left_join(., wcrl_data, by = c("Habitat", "Shore")) %>%
    left_join(., rec_data, by = c("Habitat", "Shore")) %>%
    left_join(., ssl_data, by = c("Habitat", "Shore")) %>%
    left_join(., ssf_data, by = c("Habitat", "Shore"))

habitat_activity <- habitat_activity %>%
    select(c(Habitat, Shore, contains("proportion"))) %>%
    pivot_longer(
        cols = contains("proportion"),
        names_to = "gear_type",
        values_to = "proportion"
    ) %>%
    mutate(habitat_shore = paste0(Habitat, "_", Shore)) %>%
    st_drop_geometry()

ggplot() +
    geom_tile(data = habitat_activity, aes(x = habitat_shore, y = gear_type, fill = proportion)) +
    theme_minimal() +
    scale_fill_viridis_c()

habitat_activity_final <- habitat_activity %>%
    mutate(
        strathe2e_habitat = case_when(
            habitat_shore == "rock_Inshore" ~ "Habitat_s0",
            habitat_shore == "gravel_Inshore" ~ "Habitat_s3",
            habitat_shore == "sand_Inshore" ~ "Habitat_s2",
            habitat_shore == "mud_Inshore" ~ "Habitat_s1",
            habitat_shore == "rock_Offshore" ~ "Habitat_d0",
            habitat_shore == "gravel_Offshore" ~ "Habitat_d3",
            habitat_shore == "sand_Offshore" ~ "Habitat_d2",
            habitat_shore == "mud_Offshore" ~ "Habitat_d1"
        )
    ) %>%
    pivot_wider(
        id_cols = c(gear_type),
        names_from = strathe2e_habitat,
        values_from = proportion
    ) %>%
    mutate(
        Gear_code = case_when(
            gear_type == "proportion_midwater_trawl" ~ "MWT",
            gear_type == "proportion_nets" ~ "NTS",
            gear_type == "proportion_pole_and_line" ~ "TPL",
            gear_type == "proportion_squid_jig" ~ "SJ",
            gear_type == "proportion_longline" ~ "LL",
            gear_type == "proportion_purse_seine" ~ "PS",
            gear_type == "proportion_demersal_trawl" ~ "DMT",
            gear_type == "proportion_West_Coast_Rock_Lobster_traps" ~ "WCRLT",
            gear_type == "proportion_recreational_fishing_gears" ~ "RFG",
            gear_type == "proportion_small_scale_lines" ~ "SSL",
            gear_type == "proportion_subsistence_fishing_gear" ~ "SBF"
        )
    )

habitat_activity_final$Gear_name <- strathe2e_gear_types[habitat_activity_final$Gear_code]
habitat_activity_final <- habitat_activity_final %>%
    select(c(Gear_name, Gear_code, Habitat_s0, Habitat_s1, Habitat_s2, Habitat_s3, Habitat_d0, Habitat_d1, Habitat_d2, Habitat_d3))

write.csv(habitat_activity_final, glue("./Objects/fishing_distribution_{implementation}_{start_year}-{end_year}.csv"), row.names = FALSE)
