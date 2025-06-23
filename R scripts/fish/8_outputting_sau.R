library(dplyr)
library(arrow)
library(sf)

source("./R Scripts/@_model_config.R")

domain_size <- readRDS("./Objects/Domains.rds") %>% # We need landings as tonnes per m^2
    st_transform(crs = 9822) %>%
    sf::st_union() %>%
    sf::st_area() %>%
    as.numeric()

known_species <- read.csv("./Objects/updated_known_fish_guilds.csv", row.names = 1)

annual_effort_gears <- read.csv(glue("./Objects/fishing_activity_{domain_name}_{start_year}-{end_year}.csv"))

prop_sau_activity_in_domain <- read.csv("./Objects/proportion_sau_activity_in_domain.csv") %>%
    rename(gear_type_se2e = Gear_name)

# Landings
landings <- read_parquet("./Objects/sau_landings_strath_gears.parq") %>%
    filter(gear_type_se2e != "other gears")
discards <- read_parquet("./Objects/sau_discards_strath_gears.parq") %>%
    filter(gear_type_se2e != "other gears")

# Combine landings and discards into total catch
catch <- rbind(landings, discards) %>%
    left_join(prop_sau_activity_in_domain[, c("gear_type_se2e", "proportion_sau_activity_in_domain")], by = "gear_type_se2e") %>%
    mutate(tonnes = tonnes * proportion_sau_activity_in_domain) %>% # Scale catch tonnes by the proportion of SAU-area effort of each gear in domain
    group_by(year, gear_type_se2e, Guild) %>%
    summarise(annual_total_tonnes = sum(tonnes)) %>% # Calculate total annual catch for each gear type and guild
    group_by(gear_type_se2e, Guild) %>%
    summarise(annual_average_tonnes = mean(annual_total_tonnes)) # Calculate annual average for each gear and guild

# Add longline (pelagic and longline) bird landings data (catch for birds from longlines are added to landings because birds are taken to port in this fishery)
# Data values taken from Rollinson et al (2017). Patterns and trends in seabird bycatch in the pelagic longline fishery off South Africa (over 8 years).
# And: Peterson et al. (2009). Seabird bycatch in the demersal longline fishery off South Africa (over 7 years).
# number of birds * bird mass (kg) / number of study years (for each data source pelagic and demersal)
additional_bird_longline <- c(
    (482 / 8) * 3.8977, # 482 (pelagic longline) Thalassarche cauta/steadi (Rollinson et al. 2017).
    (159 / 8) * 3.2029, # 159 (pelagic longline) Thalassarche melanophris (Rollinson et al. 2017).
    (77 / 8) * 2.1288, # 77 (pelagic longline) Thalassarche carteri (Rollinson et al. 2017).
    ((18 / 8) + (3 / 7)) * 2.1288, # 18 (pelagic longline) Thlassarche chlororhynchos (Rollinson et al. 2017).  3 (demersal longline) (Peterson et al. 2009)
    (3 / 8) * 8.9056, # 3 (pelagic longline) Diomedea sandfordi/epomophora (Rollinson et al. 2017).
    (5 / 8) * 6.9613, # 5 (pelagic longline) Diomedea exulans (Rollinson et al. 2017).
    (7 / 8) * 4.2063, # 7 (pelagic longline) Macronectes halli/giganteus (Rollinson et al. 2017).
    ((1541 / 8) + (38 / 7)) * 1.213, # 1541 (pelagic longline) Procellaria aequinoctialis (Rollinson et al. 2017). 38 (demersal longline) (Peterson et al. 2009)
    (1 / 8) * 1.131, # 1 (pelagic longline) Procellaria cinerea (Rollinson et al. 2017).
    (1 / 8) * 0.4296, # 1 (pelagic longline) Daption capense (Rollinson et al. 2017).
    ((2 / 8) + (11 / 7)) * 0.849, # 2 (pelagic longline) Puffinus gravis (Rollinson et al. 2017). 11 (demersal longline) (Peterson et al. 2009)
    (2 / 8) * 1.650, # 2 (pelagic longline) Catharacta antarctica (Rollinson et al. 2017).
    ((45 / 8) + (18 / 7)) * 2.643 # 45 (pelagic longline) Morus capensis (Rollinson et al. 2017). 18 (demersal longline) (Peterson et al. 2009)
)
additional_bird_longline <- sum(additional_bird_longline) / 1000 # Convert to tonnes from kg
additional_bird_longline <- additional_bird_longline / 0.6 # Calculate total bird catch for longlines assuming 40% is discarded

# Add demersal trawl seabird catch data (assumed that birds are most often killed during waste dumping and are not kept to bring to port, thus discard rate = 1).
# Data values taken from Watkins et al. (2008). Interactions between seabirds and deep-water hake trawl gear: an assessment of impacts in south African waters.
# Number of birds * bird mass (kg)
additional_bird_demersal_trawl <- c(
    11 * 2.643, # 11 Morus capensis (Watkins et al. 2008)
    13 * 3.8977, # 13 Thalassarche cauta (Watkins et al. 2008)
    11 * 3.2029, # 11 Thalassarche melanophris (Watkins et al. 2008)
    3 * 1.213, # 3 Procellaria aequinoctialis (Watkins et al. 2008)
    1 * 0.787, # 1 Ardenna grisea (Watkins et al. 2008)
    1 * 1.65 # 1 Stercorarius antarcticus (Watkins et al. 2008)
)
additional_bird_demersal_trawl <- sum(additional_bird_demersal_trawl) / 1000

catch <- rbind(
    catch,
    data.frame(
        gear_type_se2e = c("longline", "demersal trawl"),
        Guild = c("Birds", "Birds"),
        annual_average_tonnes = c(additional_bird_longline, additional_bird_demersal_trawl)
    )
)

catch_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., catch[, c("gear_type_se2e", "Guild", "annual_average_tonnes")], by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA" & Guild != "")

ggplot() +
    geom_tile(data = catch_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = log(annual_average_tonnes))) +
    scale_fill_viridis_c()

# Discards
processed_discards <- discards %>%
    left_join(prop_sau_activity_in_domain[, c("gear_type_se2e", "proportion_sau_activity_in_domain")], by = "gear_type_se2e") %>%
    mutate(tonnes = tonnes * proportion_sau_activity_in_domain) %>% # Scale catch tonnes by the proportion of SAU-area effort of each gear in domain
    group_by(year, gear_type_se2e, Guild) %>%
    summarise(annual_total_tonnes = sum(tonnes)) %>% # Calculate total annual catch for each gear type and guild
    group_by(gear_type_se2e, Guild) %>%
    summarise(annual_average_discards_tonnes = mean(annual_total_tonnes))

discard_rates <- left_join(catch, processed_discards, by = c("gear_type_se2e", "Guild")) %>%
    mutate(annual_average_discard_rate = annual_average_discards_tonnes / annual_average_tonnes) %>% # Calculate discard rates as a proportion of total catch tonnes
    select(c(gear_type_se2e, Guild, annual_average_discard_rate))

discards_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., discard_rates[, c("gear_type_se2e", "Guild", "annual_average_discard_rate")], by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA" & Guild != "")

# Add discards from seabirds in pelagic longline fishery from Peterson et al. (2009) Seabird bycatch in the pelagic longline fishery off south africa.
# Peterson et al. 2009 suggest that 40% of seabirds are not brought back to shore for operational reasons.
# We can then transform the catch back into total seabird bycatch and then get a discard rate
discards_matrix_data[discards_matrix_data$Guild == "Birds" & discards_matrix_data$gear_type_se2e == "longline", ]$annual_average_discard_rate <- 0.4

# Assumed that birds are most often killed during waste dumping in demersal trawl fishery and are not kept to bring to port, thus discard rate = 1.
discards_matrix_data[discards_matrix_data$Guild == "Birds" & discards_matrix_data$gear_type_se2e == "demersal trawl", ]$annual_average_discard_rate <- 1

ggplot() +
    geom_tile(data = discards_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = annual_average_discard_rate)) +
    scale_fill_viridis_c()


# Output final matrix files
catch_power_data <- catch_matrix_data %>%
    mutate(Gear_code = names(strathe2e_gear_types)[match(gear_type_se2e, strathe2e_gear_types)]) %>%
    mutate(Guild_code = names(strathe2e_guilds)[match(Guild, strathe2e_guilds)]) %>%
    left_join(annual_effort_gears[, c("Gear_code", "Activity_.s.m2.d.")], by = "Gear_code") %>% # Attach activity rates for each gear
    mutate(power = annual_average_tonnes / Activity_.s.m2.d.) %>% # Convert catch tonnes to catching power
    mutate(Guild_nitrogen = mMNpergWW[Guild_code]) %>%
    mutate(power = power * Guild_nitrogen) %>% # Convert catch power from t/activity to mMNpergWW/activity for each guild
    rename(Gear_name = gear_type_se2e) %>%
    select(Gear_name, Gear_code, Guild_code, power) %>%
    mutate(power = ifelse(is.na(power), 0, power)) %>% # Replace NA values with 0s
    pivot_wider(
        id_cols = c(Gear_name, Gear_code),
        names_from = Guild_code,
        values_from = power,
        names_prefix = "Power_"
    )
write.csv(catch_power_data, "./Objects/fishing_power_{domain_name}_{start_year}-{end_year}.csv", row.names = FALSE)

discards_rates_data <- discards_matrix_data %>%
    mutate(Gear_code = names(strathe2e_gear_types)[match(gear_type_se2e, strathe2e_gear_types)]) %>%
    mutate(Guild_code = names(strathe2e_guilds)[match(Guild, strathe2e_guilds)]) %>%
    rename(Gear_name = gear_type_se2e) %>%
    select(Gear_name, Gear_code, Guild_code, annual_average_discard_rate) %>%
    mutate(annual_average_discard_rate = ifelse(is.na(annual_average_discard_rate), 0, annual_average_discard_rate)) %>%
    pivot_wider(
        id_cols = c(Gear_name, Gear_code),
        names_from = Guild_code,
        values_from = annual_average_discard_rate,
        names_prefix = "Discardrate_"
    )
write.csv(discards_rates_data, "./Objects/fishing_discards_{domain_name}_{start_year}-{end_year}.csv", row.names = FALSE)

discard_weight_target <- discards_matrix_data %>%
    left_join(catch_matrix_data, by = c("Guild", "gear_type_se2e")) %>%
    mutate(Gear_code = names(strathe2e_gear_types)[match(gear_type_se2e, strathe2e_gear_types)]) %>%
    mutate(Guild_code = names(strathe2e_guilds)[match(Guild, strathe2e_guilds)]) %>%
    mutate(annual_average_discard_tonnes = annual_average_discard_rate * annual_average_tonnes) %>%
    rename(Gear_name = gear_type_se2e) %>%
    select(Gear_name, Gear_code, Guild_code, annual_average_discard_tonnes) %>%
    mutate(annual_average_discard_rate = ifelse(is.na(annual_average_discard_tonnes), 0, annual_average_discard_tonnes)) %>%
    pivot_wider(
        id_cols = c(Gear_name, Gear_code),
        names_from = Guild_code,
        values_from = annual_average_discard_tonnes,
        names_prefix = "Discardweight_"
    )
write.csv(discard_weight_target, "./Objects/TARGET_raw_discards_t_m2_y_{domain_name}_{start_year}-{end_year}.csv")

# Target live weight landings
landing_data <- landings %>%
    left_join(prop_sau_activity_in_domain[, c("gear_type_se2e", "proportion_sau_activity_in_domain")], by = "gear_type_se2e") %>%
    mutate(tonnes = tonnes * proportion_sau_activity_in_domain) %>% # Scale catch tonnes by the proportion of SAU-area effort of each gear in domain
    group_by(year, Guild) %>%
    summarise(annual_total_tonnes = sum(tonnes)) %>% # Calculate total annual catch for each gear type and guild
    group_by(Guild) %>%
    summarise(annual_average_landings_tonnes = mean(annual_total_tonnes)) %>%
    mutate(Guild_code = names(strathe2e_guilds)[match(Guild, strathe2e_guilds)]) %>%
    mutate(Guild_nitrogen = mMNpergWW[Guild_code]) %>%
    mutate(annual_average_landings_mmN = annual_average_landings_tonnes * Guild_nitrogen) %>% # Convert to nitrogen weightings
    mutate(annual_average_landings_mmN = annual_average_landings_mmN / domain_size) # Convert to landings /m^2 of domain size
