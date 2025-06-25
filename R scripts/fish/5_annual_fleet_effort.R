# Compile annual effort data for each gear type.

packages <- c("tidyverse", "glue", "terra", "sf", "exactextractr")
sapply(packages, library, character.only = TRUE)

source("./R Scripts/@_Region file.R")
source("./R Scripts/@_model_config.R")

domain <- readRDS("./Objects/Domains.rds")
domain_size <- readRDS("./Objects/Domains.rds") %>% # We need landings as tonnes per m^2
    st_transform(crs = 9822) %>%
    sf::st_union() %>%
    sf::st_area() %>%
    as.numeric()

habitats <- readRDS("./Objects/Habitats.rds")

# Midwater Trawl
# Effort data for the Midwater Trawl fishing fleet comes from Reed et al. (2017). This fleet mainly consists of one large trawler vessel performing the majority of midwater trawls in South Africa.
# The midwater trawl fishery extends east from 22 degrees East. This means that most of the fleet's effort likely doesn't occur within the Southern Benguela model habitat domain. To get a more accurate estimate of effort we need to scale the total effort estimate by the proportion of effort occurring within the domain area.

# Reed J, Kerwath S.E., Attwood C. (2017). Analysis of bycatch in the South African midwater trawl fishery for horse mackerel *Trachurus capensis* based on observer data.
mw_intensity <- glue("../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif")
mw_intensity <- rast(mw_intensity)
mw_intensity <- as.numeric(mw_intensity)

total_spatial_mw_intensity <- global(mw_intensity, fun = "sum", na.rm = TRUE)
prop_mw_in_domain <- exact_extract(mw_intensity, domain, fun = "sum")
prop_mw_in_domain <- sum(prop_mw_in_domain) / total_spatial_mw_intensity
# Number of trawls estimated from Reed et al. 2007. (5092.3)
annual_mw_effort <- (2.2 * 0.85 * 5092.3) + (2.3 * 0.15 * 5092.3) # Average hours of nighttime and daytime trawls and the proportion of total trawls (5092) that are conducted at night/day
annual_mw_effort <- annual_mw_effort * prop_mw_in_domain[1, 1] # Scaled by the proportion of effort in habitat domain.

# Nets including small scale
# Activity data for the Nets fishing fleet comes from Hutchings and Lamberth (2010). This fleet mainly consists of Beach Seine and Gillnet gear types, with some additional catch coming from small scale net types such as encircling nets. Hutchings and Lamberth (2010) carried out questionaire and phone surveys to estimate the annual beach seine activity as 3200 beach-seine hauls, and gillnet activity as 25000 gillnet days per year.
# If we assume that a fisher performs 1 beach seine haul per day, we can estimate the beach-seine activity as 3200 beach-seine days per year, and estimate the proportion of total fleet activity from both net types.
# These effort proportions can be used to weight the two net types when estimating spatial proportions of effort.

# Hutchings K. and Lamberth S.J. (2010). Catch-and-effort estimates for the gillnet and beach-seine fisheries in the Western Cape, South Africa.

beach_seine_days <- 3200
gillnet_days <- 25000
annual_net_effort <- beach_seine_days + gillnet_days # Fisher net days
annual_net_effort <- annual_net_effort * 12 * 60 * 60 # Assume that fishers spend an average of 12h fishing each fishing day and convert to seconds

beach_seine_proportion <- beach_seine_days / annual_net_effort
gillnet_proportion <- gillnet_days / annual_net_effort

# Pole and line
# Activity data for the pole and line (migratory) fishery is taken from West et al. (2024). This report indicates that 165 vessel rights were allocated to fishers for the 2013-2020 period.

# West W., Mketsu Q., Kerwath C., da Silva C. and Meyer M. (2024). South African national report to the Extended Scientific Committee of the Commission for the Conservation of Southern Bluefin Tuna (CCSBT), 2019-2023.
tpl_intensity <- glue("../../Spatial Data/fishing_effort_data/Tuna_Pole_Intensity/Tuna_Pole_Intensity_{crs}.tif")
tpl_intensity <- rast(tpl_intensity)
tpl_intensity <- as.numeric(tpl_intensity)
tpl_intensity <- tpl_intensity["Tuna_Pole_Intensity_1"]

total_spatial_tpl_intensity <- global(tpl_intensity, fun = "sum", na.rm = TRUE)
prop_tpl_in_domain <- exact_extract(tpl_intensity, domain, fun = "sum")
prop_tpl_in_domain <- sum(prop_tpl_in_domain) / total_spatial_tpl_intensity

annual_pole_and_line_effort <- 165 * prop_tpl_in_domain[1, 1] # Vessels, scaled by total tuna pole proportion of effort within the domain
annual_pole_and_line_effort <- annual_pole_and_line_effort * (182 * 12 * 60 * 60) # Assume boats fish 2/4 weeks per month (182 days/y) (West et al. 2024), for 12h per day and convert to seconds.

# Squid jig
# Activity data for squid jig fishery is taken from Cochrane et al. (2014). This study estimates the total number of boats in the fishery to be 138 based on questionaire surveys from 2012-2013.
# Most of the Chokka Squid fishing occurs along the southern coast, so this activity level needs to be scaled by the proportion of fishing ocurring in the domain.
sj_intensity <- glue("../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Intensity_{crs}.tif")
sj_intensity <- rast(sj_intensity)
sj_intensity <- as.numeric(sj_intensity)

total_spatial_sj_intensity <- global(sj_intensity, fun = "sum", na.rm = TRUE)
prop_sj_in_domain <- exact_extract(sj_intensity, domain, fun = "sum")
prop_sj_in_domain <- sum(prop_sj_in_domain) / total_spatial_sj_intensity

annual_sj_effort <- 3000000 / 21 # Number of man-hours divided by average crew per vessel (21). Cochrane et al. 2014.
annual_sj_effort <- annual_sj_effort * 60 * 60 * prop_sj_in_domain[1, 1] # Convert vessel-hours to seconds and scale by proportion of effort within domain.

# Longlines
# Activity data for the pelagic longline fishery are taken from Parker et al. (2021). This report contains data on the number of hooks used in the pelagic longline sector from 2010 to 2019. These data are used to calculate an annual average activity, which is combined with spatial data to get an activity estimate for the Southern Benguela domain.
pll_intensity <- glue("../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity_{crs}.tif")
pll_intensity <- rast(pll_intensity)
pll_intensity <- as.numeric(pll_intensity)

total_spatial_pll_intensity <- global(pll_intensity, fun = "sum", na.rm = TRUE)
prop_pll_in_domain <- exact_extract(pll_intensity, domain, fun = "sum")
prop_pll_in_domain <- sum(prop_pll_in_domain) / total_spatial_pll_intensity

annual_pll_effort <- mean(c(23, 25, 16, 19, 19, 15, 22, 24, 29, 21)) * 182 * 12 * 60 * 60 # Calculate mean number of boats used for 2010-2019 period and knowing average trip duration is 2 weeks (Petersen and Honig 2006.), we assume 2 weeks on 2 weeks off like tuna pole and line (182 days), and for 12 hours per day.
# annual_pll_effort <- mean(c(4452420, 5235123, 3816271, 3872846, 1828671, 1614724, 1284756, 1284160, 1325446, 1355677))
annual_pll_effort <- annual_pll_effort * prop_pll_in_domain[1, 1] # Scale by proportion of effort in domain

# Activity data for the demersal longline fishery are taken from Bergh (2022). This study contains data on the number of demersal longline sets (for the west coast ~17-18 degE), which can be converted to a number of hooks (assuming 14,000 hooks per demersal longline set (Nyengera and Angel. 2019)).
dll_intensity <- glue("../../Spatial Data/fishing_effort_data/Hake_Longline_Intensity/Hake_Longline_Intensity_{crs}.tif")
dll_intensity <- rast(dll_intensity)
dll_intensity <- as.numeric(dll_intensity)
dll_intensity <- dll_intensity["Hake_Longline_Intensity_1"]

total_spatial_dll_intensity <- global(dll_intensity, fun = "sum", na.rm = TRUE)
prop_dll_in_domain <- exact_extract(dll_intensity, domain, fun = "sum")
prop_dll_in_domain <- sum(prop_dll_in_domain) / total_spatial_dll_intensity

annual_dll_effort <- mean(c(1176, 1311, 1751, 2153, 2860, 3136, 3113, 2885, 2761, 2700)) * 14000 # Annual demersal longline effort in number of hooks (assuming each set has 14,000 hooks).
annual_dll_effort <- annual_dll_effort / 14995 # Assuming that an average of 14,995 hooks are set per day we calculate the total number of fishing-days per year (Nyengera and Angel. 2019)
annual_dll_effort <- annual_dll_effort * 17 * 60 * 60 # Setting begins at 2am mostly, with hauling starting at 11 and continuing for 8 to 10 hours. Total 17 hours per day wet-time assumed (Nyengera and Angel. 2019.)
annual_dll_effort <- annual_dll_effort * prop_dll_in_domain[1, 1] # Scale by proportion of effort in domain

annual_longline_effort <- annual_pll_effort + annual_dll_effort
annual_longline_effort <- annual_longline_effort

# Purse Seine
# Activity data for the purse seine fishery are taken from Global Fishing Watch data. These data have been collated to the number of total hours per fleet per year. This data is then extracted for the habitat domain and the average annual hours for each habitat are summed to the domain level.
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

# Extract the total yearly effort in hours for each habitat/shore area and variable
gfw_data <- gfw_polygon_extraction("fleet_fishing_ZAF-purse_seine_domain.nc", habitats)
# For each variable, calculate the proportion of effort distributed to each habitat/shore type
gfw_data <- group_by(gfw_data, year) %>%
    summarise(hours = sum(hours)) # %>%

annual_purse_seine_effort <- mean(gfw_data$hours) # Hours of purse seine effort annual average
annual_purse_seine_effort <- annual_purse_seine_effort * 60 * 60 # Convert to seconds

# Demersal Trawl
# Activity data for the demersal trawl fleet are obtained from Barry Baker et al (2007). This data originates from the Department for Fisheries, Forestry and Environment South Africa. The data from this study covers the 1996-2004 period. We calculate the annual activity value as the average of 2000-2004 activity data, as there is a substantial shift in number of hours from 1990s to 2000s.
# Barry Baker G., Double M.C., Gales R., Tuck G.N., Abbott C., Ryan P.G., Petersen S., Robertson C.J.R, Alderman R. (2007). A global assessment of the impact of fisheries-related mortality on shy and white-capped albatrosses: Conservation implications

# Combine inshore and offshore demersal trawl layers:
# Load inshore demersal trawl activity
intr_intensity <- glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Inshore_Intensity/Trawl_Inshore_Intensity_{crs}.tif")
intr_intensity <- rast(intr_intensity)
intr_intensity <- as.numeric(intr_intensity)
intr_intensity <- subst(intr_intensity, NA, 0) # Set NA values to 0 for addition of second layer
intr_intensity <- intr_intensity["Trawl_Inshore_Intensity_1"]

## Load offshore demersal trawl activity
oftr_intensity <- glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Offshore_Intensity/Trawl_Offshore_Intensity_{crs}.tif")
oftr_intensity <- rast(oftr_intensity)
oftr_intensity <- as.numeric(oftr_intensity)
oftr_intensity <- subst(oftr_intensity, NA, 0) # Set NA values to 0 for addition of second layer
oftr_intensity <- oftr_intensity["Trawl_Offshore_Intensity_1"]

# Get proportion of total demersal trawl effort in domain
dmtrwl_intensity <- intr_intensity + oftr_intensity
total_spatial_dmtrwl_intensity <- global(dmtrwl_intensity, fun = "sum", na.rm = TRUE)
prop_dmtrwl_in_domain <- exact_extract(dmtrwl_intensity, domain, fun = "sum")
prop_dmtrwl_in_domain <- sum(prop_dmtrwl_in_domain) / total_spatial_dmtrwl_intensity

annual_demt_effort <- mean(c(80000, 75000, 81000, 75000, 70000)) # Number of trawling hours reported in the South African offshore demersal trawl fishery.
annual_demt_effort <- annual_demt_effort * 60 * 60 # Convert to seconds

# West Coast Rock Lobster traps
# Activity data for the West Coast Rock Lobster fishery are obtained as the total number of trips per year from Eggers (2021) for the offshore sector. Data
# from this study are available for the 2016-2019 period and were obtained using department of fisheries data. Note that only the offshore sector is considered in activity to avoid double counting, because WCRL landings for the nearshore sector may be incorporated into small scale nets / pots or traps.
annual_wcrl_effort <- mean(c(2696, 2171, 1114)) # Number of trips in the offshore sector.
annual_wcrl_effort <- annual_wcrl_effort * 2 * (0.75 * 24) * 60 * 60 # Assume that trips last around 2 days (SAFE Lobster presentation 2020.) and that traps are underwater for 75% of the trip duration to convert from trips to seconds.

# Recreational fishing gear
# Activity data for recreational fishing gear obtained from Pots et al. (2021) as an estimated number of fishers.
# This data was obtained from questionaires and interviews and is taken from all respondents (method 1), with 18% reportedly from the Western Cape region.
annual_marine_rec_effort <- 547799 * 0.18 # Number of fishers
annual_marine_rec_effort <- annual_marine_rec_effort * 48 * 5 * 60 * 60 # Average of 48 days per year fishing (Pots et al. 2021), and assume 5h spent fishing during those days to convert to seconds.

# Small scale lines
# Activity data for small scale lines taken from the Status of Marine Resources 2023 report. Stating the fishery is made up of 7200 fishers with 85% targetting linefish.
ssl_intensity <- glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif")
ssl_intensity <- rast(ssl_intensity)
ssl_intensity <- as.numeric(ssl_intensity)

total_spatial_ssl_intensity <- global(ssl_intensity, fun = "sum", na.rm = TRUE)
prop_ssl_in_domain <- exact_extract(ssl_intensity, domain, fun = "sum")
prop_ssl_in_domain <- sum(prop_ssl_in_domain) / total_spatial_ssl_intensity

annual_ssl_effort <- 7200 * 0.85 # Number of fishers
annual_ssl_effort <- annual_ssl_effort * 230 * 12 * 60 * 60 # We assume that fishers fish weekdays (260), removing a month due to inclement weather, and assume that fishers spend an average half of the day fishing (12h).
annual_ssl_effort <- annual_ssl_effort * prop_ssl_in_domain[1, 1]

# Subsistence fishing gear
annual_ssf_effort <- 458 + 643 + 1272 # Number of fishers from Clark et al. 2002. Subsistence fishing surveys from zones A-C (roughly matching model domain).
# Based on a perspective testimony from KZN region we assume that fishers spend roughly 12h fishing per day. https://icsf.net/newss/small-scale-fishers-in-south-africa-struggle-to-survive-while-facing-a-litany-of-regulations/
# Based on a study of coastal mangrove subsistence fishers we assume fishers spend roughly 219 days per year fishing. (Ermgassen 2020) Fishers who rely on mangroves....
annual_ssf_effort <- annual_ssf_effort * 219 * 12 * 60 * 60

annual_effort_gears <- data.frame(
    Gear_name = strathe2e_gear_types,
    Gear_code = gear_codes,
    Activity_.s.m2.d. = c(
        annual_mw_effort,
        annual_net_effort,
        annual_pole_and_line_effort,
        annual_sj_effort,
        annual_longline_effort,
        annual_purse_seine_effort,
        annual_demt_effort,
        annual_wcrl_effort,
        annual_marine_rec_effort,
        annual_ssl_effort,
        annual_ssf_effort
    ),
    Plough_rate_.m2.s. = rep(0, length(strathe2e_gear_types))
)

# Convert activity values to m^2/day
annual_effort_gears <- mutate(annual_effort_gears, Activity_.s.m2.d. = Activity_.s.m2.d. / domain_size / 360) # StrathE2E represents 360 days per year

# Add plough rate values from north sea model implementation
# The only gear type causing seabed abrasion in our model implementation is the demersal trawl fleet, using otter trawling methods, this matches "Demersal_Otter_Trawl_TR1".
north_sea_plough_rate <- read.csv("./Data/fishing_activity_NORTH_SEA_2003-2013.csv")
annual_effort_gears[annual_effort_gears$Gear_code == "DMT", ]$Plough_rate_.m2.s. <- north_sea_plough_rate[north_sea_plough_rate$Gear_code == "OT", ]$Plough_rate_.m2.s.

# Need to convert values to seconds/time based units
# Need to ask about plough rate
# Need to calculate

write.csv(annual_effort_gears, glue("./Objects/fishing_activity_{domain_name}_{start_year}-{end_year}.csv"), row.names = FALSE)
