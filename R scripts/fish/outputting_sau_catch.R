library(arrow)
library(sf)

domain_size <- readRDS("./Objects/Domains.rds") %>% # We need landings as tonnes per m^2
    sf::st_union() %>%
    sf::st_area() %>%
    as.numeric()

known_species <- read.csv("./Objects/updated_known_fish_guilds.csv", row.names = 1)

strathe2e_gear_types <- c(
    "MWT" = "midwater trawl",
    "NTS" = "nets including small scale",
    "TPL" = "pole and line",
    "SJ" = "squid jig",
    "LL" = "longline",
    "PS" = "purse seine",
    "DMT" = "demersal trawl",
    "WCRLT" = "WC Rock Lobster traps",
    "RFG" = "recreational fishing gear",
    "SSL" = "small scale lines",
    "SBF" = "subsistence fishing gear"
)
gear_codes <- names(strathe2e_gear_types)
strathe2e_guilds <- c(
    "PF" = "Planktivore",
    "DF" = "Demersal",
    "MF" = "Migratory",
    "FDB" = "Benthos filter/deposit feeder",
    "CSB" = "Benthos carnivore/scavenge feeder",
    "CZ" = "Zooplankton carnivore",
    "BD" = "Birds",
    "SL" = "Pinnipeds",
    "CT" = "Cetacean",
    "KP" = "Macrophyte"
)
guild_codes <- names(strathe2e_guilds)

# Landings
arrow::write_parquet(strath_e2e_gear_landings, "./Objects/sau_landings_strath_gears.parq")

guild_gear_landings <- strath_e2e_gear_landings %>%
    group_by(gear_type_se2e, Guild, year) %>%
    summarise(annual_total_tonnes = sum(tonnes)) %>%
    group_by(gear_type_se2e, Guild) %>%
    summarise(annual_average_tonnes = mean(annual_total_tonnes)) %>%
    filter(gear_type_se2e != "other gears")

landings_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., guild_gear_landings[, c("gear_type_se2e", "Guild", "annual_average_tonnes")], by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA" & Guild != "")

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
landings_matrix_data[landings_matrix_data$Guild == "Birds" & landings_matrix_data$gear_type_se2e == "longline", ]$tonnes <- additional_bird_longline

ggplot() +
    geom_tile(data = landings_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = log(annual_average_tonnes))) +
    scale_fill_viridis_c()

# Discards
arrow::write_parquet(strath_e2e_gear_discards, "./Objects/sau_discards_strath_gears.parq")

guild_gear_discards <- strath_e2e_gear_discards %>%
    group_by(gear_type_se2e, Guild, year) %>%
    summarise(annual_total_tonnes = sum(tonnes)) %>%
    group_by(gear_type_se2e, Guild) %>%
    summarise(annual_average_tonnes = mean(annual_total_tonnes)) %>%
    filter(gear_type_se2e != "other gears")

discards_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., guild_gear_discards, by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA") # %>%

# Add discards from seabirds in pelagic longline fishery from Peterson et al. (2009) Seabird bycatch in the pelagic longline fishery off south africa.
# Peterson et al. 2009 suggest that 40% of seabirds are not brought back to shore for operational reasons.
# We can then transform the catch back into total seabird bycatch and then get a discard rate
total_bird_longline_bycatch <- landings_matrix_data[landings_matrix_data$Guild == "Birds" & landings_matrix_data$gear_type_se2e == "longline", ]$tonnes / 0.6
discards_matrix_data[discards_matrix_data$Guild == "Birds" & discards_matrix_data$gear_type_se2e == "longline", ]$tonnes <- total_bird_longline_bycatch * 0.4

# Add demersal trawl seabird discards data (assumed that birds are most often killed during waste dumping and are not kept to bring to port).
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
discards_matrix_data[discards_matrix_data$Guild == "Birds" & discards_matrix_data$gear_type_se2e == "demersal trawl", ]$tonnes <- sum(additional_bird_demersal_trawl) / 1000

ggplot() +
    geom_tile(data = discards_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = tonnes)) +
    scale_fill_viridis_c()
ggplot() +
    geom_tile(data = discards_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = log(tonnes))) +
    scale_fill_viridis_c()
