library(tidyverse)
library(ggplot2)
library(gganimate)
library(randomcoloR)
library(terra)
library(blogdown)

check_species_match <- function(string_i, strings) {
    if (string_i %in% strings) { # Check if the string occurs in the vector of strings
        return(TRUE)
    } else if (
        (str_detect(string_i, "( )") && (length(str_split(string_i, "( )")[[1]]) == 2)) ||
            (length(str_split(string_i, "( )")[[1]]) == 1)) { # Check if the string is 1 or 2 words (likely contains genus - species name)
        genus <- str_split_i(string_i, "( )", 1)
        if (sum(str_detect(strings, genus), na.rm = TRUE) == 1) {
            return(TRUE)
        } else if (sum(str_detect(strings, genus), na.rm = TRUE) > 1) {
            return(TRUE)
        }
    }

    return(FALSE)
}

find_species_match <- function(string_i, strings) {
    if (string_i %in% strings) { # Check if string is in strings
        if (sum(strings == string_i, na.rm = TRUE) > 1) { # Check if there are multiple or a single string match
            matching_strings <- strings[strings == string_i]
            matching_strings <- matching_strings[!is.na(matching_strings)]
            matching_strings <- matching_strings[!duplicated(matching_strings)]

            # If there are multiple matches for a species in MiMeMo species then just take the first one, assuming species from the same genus are in the same guild
            if (length(matching_strings) > 1) {
                return(matching_strings[1])
            }

            return(matching_strings)
        }

        matching_string <- strings[strings == string_i]
        matching_string <- matching_string[!is.na(matching_string)]
        return(matching_string)
    } else if (
        (str_detect(string_i, "( )") && (length(str_split(string_i, "( )")[[1]]) == 2)) ||
            (length(str_split(string_i, "( )")[[1]]) == 1)) { # Check if the name has 2 words (likely genus-species information that can be used to match species of the same genus)
        genus <- str_split_i(string_i, "( )", 1)
        if (sum(str_detect(strings, genus), na.rm = TRUE) == 1) { # If only genus is available check for other species in that genus
            matching_string <- strings[str_detect(strings, genus)]
            matching_string <- matching_string[!is.na(matching_string)]

            return(matching_string)
        } else if (sum(str_detect(strings, genus), na.rm = TRUE) > 1) {
            matching_strings <- strings[str_detect(strings, genus)]
            matching_strings <- matching_strings[!is.na(matching_strings)]

            # If there are multiple matches for a species in MiMeMo species then just take the first one, assuming species from the same genus are in the same guild
            if (length(matching_strings) > 1) {
                return(matching_strings[1])
            }

            return(matching_strings)
        }
    }
}

year_config <- read_toml("./R scripts/config.toml")
start_year <- year_config$start_year
end_year <- year_config$end_year

sau <- read.csv("../../Fishing Data/SAU_SAWA/SAU EEZ 953 v50-1.csv")
discards <- sau[sau$catch_type == "Discards", ]
landings <- sau[sau$catch_type == "Landings", ]

known_species <- read.csv("./Objects/updated_known_fish_guilds.csv", row.names = 1)

sau_species <- unique(sau$scientific_name)
sau_not_in_guilds <- sau_species[!unlist(sapply(sau_species, function(x) check_species_match(x, known_species$Scientific.name)))]
sau_in_guilds <- sau_species[unlist(sapply(sau_species, function(x) check_species_match(x, known_species$Scientific.name)))]
sau_in_guilds <- data.frame(
    scientific_name = sau_in_guilds,
    matched_mimemo_name = unlist(sapply(sau_in_guilds, function(x) find_species_match(x, known_species$Scientific.name)))
)
sau_in_guilds <- known_species[, c("Scientific.name", "Guild")] %>%
    rename(matched_mimemo_name = Scientific.name) %>%
    left_join(sau_in_guilds, ., by = "matched_mimemo_name")

sau_in_guilds <- sau_in_guilds[!duplicated(sau_in_guilds), ]

landings <- left_join(landings, sau_in_guilds[, c("scientific_name", "matched_mimemo_name", "Guild")], by = "scientific_name")
arrow::write_parquet(landings, "./Objects/sau_landings_assigned_guilds.parq")

discards <- left_join(discards, sau_in_guilds[, c("scientific_name", "matched_mimemo_name", "Guild")], by = "scientific_name")
arrow::write_parquet(discards, "./Objects/sau_discards_assigned_guilds.parq")

strathe2e_gear_types <- c(
    "midwater trawl",
    "nets including small scale",
    "linefishery",
    "squid jig",
    "longline",
    "purse seine",
    "demersal trawl",
    "WC Rock Lobster traps"
)
strathe2e_guilds <- unique(known_species$Guild)
midwater_trawl_sau_gears <- "pelagic trawl"
nets_sau_gears <- c(
    "bagnets",
    "cast nets",
    "small scale gillnets",
    "small scale encircling nets",
    "small scale seine nets",
    "small scale other nets",
    "gillnet"
)
linefishery_sau_gears <- c("hand lines", "pole and line", "recreational fishing gear")
squidjig_sau_gears <- "small scale lines"
longline_sau_gears <- "longline"
purseseine_sau_gears <- "purse seine"
demersal_trawl_sau_gears <- "bottom trawl"
wc_rock_lobster_sau_gears <- "WC Rock Lobster traps"

all_accounted_sau_gears <- c(midwater_trawl_sau_gears, nets_sau_gears, linefishery_sau_gears, squidjig_sau_gears, longline_sau_gears, purseseine_sau_gears, demersal_trawl_sau_gears, wc_rock_lobster_sau_gears)

# Redirect the `unknown class` landings that are WC Rock Lobster to a WC Rock Lobster fishery
# Bycatch is not an issue in this industrial fishery (Status Report, 2023), so all of these landings
# Can be attributed to a new fishery using traps in rocky areas of 100m+.
for (r in seq_len(nrow(landings))) {
    row <- landings[r, ]
    if (row$gear_type == "unknown class" && row$scientific_name == "Jasus lalandii") {
        landings[r, ]$gear_type <- "WC Rock Lobster traps"
    }
}

# Landings by StrathE2E gear type and guild
strath_e2e_gear_landings <- landings %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% linefishery_sau_gears ~ "linefishery",
        gear_type == squidjig_sau_gears ~ "squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        gear_type == wc_rock_lobster_sau_gears ~ "WC Rock Lobster traps",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    filter(!is.na(Guild) & year >= start_year & year <= end_year)

# Redirect the `squid jig` landings that aren't squid to the linefishery
for (r in seq_len(nrow(strath_e2e_gear_landings))) {
    row <- strath_e2e_gear_landings[r, ]
    if (row$gear_type_se2e == "squid jig" && row$Guild != "Zooplankton carnivore") {
        strath_e2e_gear_landings[r, ]$gear_type_se2e <- "linefishery"
    }
}

guild_gear_catch <- strath_e2e_gear_landings %>%
    group_by(gear_type_se2e, Guild) %>%
    summarise(tonnes = mean(tonnes)) %>%
    filter(gear_type_se2e != "other gears")

catch_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., guild_gear_catch[, c("gear_type_se2e", "Guild", "tonnes")], by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA" & Guild != "")


# Add longline (pelagic and longline) bird landings data (catch for birds from longlines are added to landings because birds are taken to port in this fishery)
# Data values taken from Rollinson et al (2017). Patterns and trends in seabird bycatch in the pelagic longline fishery off South Africa
# And: Peterson et al. (2009). Seabird bycatch in the demersal longline fishery off South Africa
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
catch_matrix_data[catch_matrix_data$Guild == "Birds" & catch_matrix_data$gear_type_se2e == "longline", ]$tonnes <- additional_bird_longline

ggplot() +
    geom_tile(data = catch_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = log(tonnes))) +
    scale_fill_viridis_c()


# Discards by StrathE2E gear type and guild
guild_gear_discards <- discards %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% linefishery_sau_gears ~ "linefishery",
        gear_type == squidjig_sau_gears ~ "squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        gear_type == wc_rock_lobster_sau_gears ~ "WC Rock Lobster traps",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    group_by(gear_type_se2e, Guild) %>%
    summarise(tonnes = sum(tonnes)) %>%
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
total_bird_longline_bycatch <- catch_matrix_data[catch_matrix_data$Guild == "Birds" & catch_matrix_data$gear_type_se2e == "longline", ]$tonnes / 0.6
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

ggplot() +
    geom_area(
        data = squid_jig_prop,
        aes(x = year, y = proportion, fill = Guild),
        color = "gray"
    ) +
    theme_minimal() +
    labs(y = "proportion_of_landings")
