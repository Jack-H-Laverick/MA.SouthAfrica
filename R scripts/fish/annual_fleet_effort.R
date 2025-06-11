packages <- c("ggplot2", "arrow", "tidyverse", "glue", "tabulapdf", "gganimate", "randomcoloR", "terra", "arrow", "blogdown")
sapply(packages, library, character.only = TRUE)
options(dplyr.summarise.inform = FALSE)
source("./R Scripts/@_Region file.R")
habitats <- readRDS("./Objects/Habitats.rds")

year_config <- read_toml("./R scripts/config.toml")
start_year <- year_config$start_year
end_year <- year_config$end_year

# Midwater Trawl
# Effort data for the Midwater Trawl fishing fleet comes from Reed et al. (2017). This fleet mainly consists of one large trawler vessel performing the majority of midwater trawls in South Africa.
# The midwater trawl fishery extends east from 22 degrees East. This means that most of the fleet's effort likely doesn't occur within the Southern Benguela model habitat domain. To get a more accurate estimate of effort we need to scale the total effort estimate by the proportion of effort occurring within the domain area.

# Reed J, Kerwath S.E., Attwood C. (2017). Analysis of bycatch in the South African midwater trawl fishery for horse mackerel *Trachurus capensis* based on observer data.
mw_intensity <- "../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif"
mw_intensity <- rast(mw_intensity)
mw_intensity <- as.numeric(mw_intensity)

total_spatial_mw_intensity <- global(mw_intensity, fun = "sum", na.rm = TRUE)
prop_mw_in_domain <- exact_extract(mw_intensity, habitats, fun = "sum")
prop_mw_in_domain <- sum(prop_mw_in_domain) / total_spatial_mw_intensity

annual_mw_effort <- 5092.3 * prop_mw_in_domain[1, 1] # Number of trawls estimated from Reed et al. 2007. scaled by the proportion of effort in habitat domain.

# Nets including small scale
# Activity data for the Nets fishing fleet comes from Hutchings and Lamberth (2010). This fleet mainly consists of Beach Seine and Gillnet gear types, with some additional catch coming from small scale net types such as encircling nets. Hutchings and Lamberth (2010) carried out questionaire and phone surveys to estimate the annual beach seine activity as 3200 beach-seine hauls, and gillnet activity as 25000 gillnet days per year.
# If we assume that a fisher performs 1 beach seine haul per day, we can estimate the beach-seine activity as 3200 beach-seine days per year, and estimate the proportion of total fleet activity from both net types.
# These effort proportions can be used to weight the two net types when estimating spatial proportions of effort.

# Hutchings K. and Lamberth S.J. (2010). Catch-and-effort estimates for the gillnet and beach-seine fisheries in the Western Cape, South Africa.

beach_seine_days <- 3200
gillnet_days <- 25000
annual_net_effort <- beach_seine_days + gillnet_days # Fisher net days
beach_seine_proportion <- beach_seine_days / annual_net_effort
gillnet_proportion <- gillnet_days / annual_net_effort

# Pole and line
# Activity data for the pole and line (migratory) fishery is taken from West et al. (2024). This report indicates that 165 vessel rights were allocated to fishers for the 2013-2020 period.

# West W., Mketsu Q., Kerwath C., da Silva C. and Meyer M. (2024). South African national report to the Extended Scientific Committee of the Commission for the Conservation of Southern Bluefin Tuna (CCSBT), 2019-2023.
annual_pole_and_line_effort <- 165 # Vessels

# Squid jig
# Activity data for squid jig fishery is taken from Cochrane et al. (2014). This study estimates the total number of boats in the fishery to be 138 based on questionaire surveys from 2012-2013.
# Most of the Chokka Squid fishing occurs along the southern coast, so this activity level needs to be scaled by the proportion of fishing ocurring in the domain.
sj_intensity <- glue("../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Intensity_{crs}.tif")
sj_intensity <- rast(sj_intensity)
sj_intensity <- as.numeric(sj_intensity)

total_spatial_sj_intensity <- global(sj_intensity, fun = "sum", na.rm = TRUE)
prop_sj_in_domain <- exact_extract(sj_intensity, habitats, fun = "sum")
prop_sj_in_domain <- sum(prop_sj_in_domain) / total_spatial_sj_intensity

annual_sj_effort <- 138 * prop_sj_in_domain # Number of vessels in domain

# Longlines
# Activity data for the pelagic longline fishery are taken from Parker et al. (2021). This report contains data on the number of hooks used in the pelagic longline sector from 2010 to 2019. These data are used to calculate an annual average activity, which is combined with spatial data to get an activity estimate for the Southern Benguela domain.
pll_intensity <- glue("../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity_{crs}.tif")
pll_intensity <- rast(pll_intensity)
pll_intensity <- as.numeric(pll_intensity)

total_spatial_pll_intensity <- global(pll_intensity, fun = "sum", na.rm = TRUE)
prop_pll_in_domain <- exact_extract(pll_intensity, habitats, fun = "sum")
prop_pll_in_domain <- sum(prop_pll_in_domain) / total_spatial_pll_intensity

annual_pll_effort <- mean(c(4452420, 5235123, 3816271, 3872846, 1828671, 1614724, 1284756, 1284160, 1325446, 1355677))
annual_pll_effort <- annual_pll_effort * prop_pll_in_domain # Number of hooks in domain

# Activity data for the demersal longline fishery are taken from Bergh (2022). This study contains data on the number of demersal longline sets (for the west coast ~17-18 degE), which can be converted to a number of hooks (assuming 14,000 hooks per demersal longline set (Nyengera and Angel. 2019)).
# Pelagic and demersal longline activity data are then combined.

annual_dll_effort <- mean(c(1176, 1311, 1751, 2153, 2860, 3136, 3113, 2885, 2761, 2700)) * 14000 # Annual demersal longline effort in number of hooks (assuming each set has 14,000 hooks).

annual_longline_effort <- annual_pll_effort + annual_dll_effort

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
gfw_data <- gfw_polygon_extraction("fleet_fishing_ZAF-purse_seine.nc", habitats)
# For each variable, calculate the proportion of effort distributed to each habitat/shore type
gfw_data <- group_by(gfw_data, year) %>%
    summarise(hours = sum(hours)) # %>%

annual_purse_seine_effort <- mean(gfw_data$hours) # Hours of purse seine effort annual average

# Demersal Trawl
# Activity data for the demersal trawl fleet are obtained from Barry Baker et al (2007). This data originates from the Department for Fisheries, Forestry and Environment South Africa. The data from this study covers the 1996-2004 period. We calculate the annual activity value as the average of 2000-2004 activity data, as there is a substantial shift in number of hours from 1990s to 2000s.

# Barry Baker G., Double M.C., Gales R., Tuck G.N., Abbott C., Ryan P.G., Petersen S., Robertson C.J.R, Alderman R. (2007). A global assessment of the impact of fisheries-related mortality on shy and white-capped albatrosses: Conservation implications
annual_demt_effort <- mean(c(80000, 75000, 81000, 75000, 70000)) # Number of trawling hours reported in the South African offshore demersal trawl fishery.

# West Coast Rock Lobster traps
# Activity data for the West Coast Rock Lobster fishery are obtained as the total number of trips per year from Eggers (2021) for the offshore sector. Data
# from this study are available for the 2016-2019 period and were obtained using department of fisheries data. Note that only the offshore sector is considered in activity to avoid double counting, because WCRL landings for the nearshore sector may be incorporated into small scale nets / pots or traps.
annual_wcrl_effort <- mean(c(2696, 2171, 1114)) # Number of trips in the offshore sector.

# Recreational fishing gear
# Activity data for recreational fishing gear obtained from Pots et al. (2021) as an estimated number of fishers.
# This data was obtained from questionaires and interviews and is taken from all respondents (method 1), with 18% reportedly from the Western Cape region.
annual_marine_rec_effort <- 547799 * 0.18 # Number of fishers

# Small scale lines
# Activity data for small scale lines taken from the Status of Marine Resources 2023 report. Stating the fishery is made up of 7200 fishers with 85% targetting linefish.
annual_ssl_effort <- 7200 * 0.85

# Subsistence fishing gear
#
