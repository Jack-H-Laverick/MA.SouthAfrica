library(tidyverse)
library(ggplot2)
library(gganimate)
library(randomcoloR)
library(terra)

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

# guild_landings <- landings %>%
#     mutate(Guild = if_else(is.na(Guild), paste0("#", functional_group), Guild)) %>% # Mark functional groups from seas-around-us with `#`
#     group_by(year, gear_type, Guild) %>%
#     summarise(tonnes_mean = mean(tonnes), tonnes_sd = sd(tonnes))

# ggplot() +
#     geom_line(data = guild_landings[!str_detect(guild_landings$Guild, "(#)"), ], aes(x = year, y = tonnes_mean, color = Guild)) +
#     # geom_ribbon(data = guild_landings, aes(x = year, ymin = tonnes_mean - tonnes_sd, ymax = tonnes_mean + tonnes_sd), alpha = 0.5) +
#     facet_wrap(~gear_type, scales = "free_y")

# ggplot() +
#     geom_line(data = landings[landings$gear_type == "longline", ], aes(x = year, y = tonnes, color = scientific_name))

strathe2e_gear_types <- c(
    "midwater trawl",
    "nets including small scale",
    "linefishery",
    "small scale lines / squid jig",
    "longline",
    "purse seine",
    "demersal trawl"
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
small_lines_squidjig_sau_gears <- "small scale lines"
longline_sau_gears <- "longline"
purseseine_sau_gears <- "purse seine"
demersal_trawl_sau_gears <- "bottom trawl"

all_accounted_sau_gears <- c(midwater_trawl_sau_gears, nets_sau_gears, linefishery_sau_gears, small_lines_squidjig_sau_gears, longline_sau_gears, purseseine_sau_gears, demersal_trawl_sau_gears)

# Landings by StrathE2E gear type and guild
guild_gear_catch <- landings %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% linefishery_sau_gears ~ "linefishery",
        gear_type == small_lines_squidjig_sau_gears ~ "small scale lines / squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    group_by(fishing_sector, gear_type_se2e, Guild) %>%
    summarise(tonnes = mean(tonnes)) %>%
    filter(gear_type_se2e != "other gears")

# guild_gear_catch_other_gears <- guild_gear_catch[str_detect(guild_gear_catch$gear_type_se2e, "(other )"), ]
# guild_gear_catch <- guild_gear_catch[!str_detect(guild_gear_catch$gear_type_se2e, "(other )"), ]

# redistribute_unaccounted_gears <- function(guild_gear_df, original_gears, additional_gears, sector) {
#     # guild_gear_df - dataframe containing the catch/discards data for all gear_type_se2e and all Guilds. I.e. expand.grid()
#     # original_gears contains the gear and guild -wise tonnes for all accounted strath e2e gears.
#     # additional_gears contains the gear and guild -wise tonnes for additional unaccounted SAU gears (e.g. "other Industrial")

#     contained_catch <- unique(original_gears[, c("fishing_sector", "gear_type_se2e")])
#     additional_catch <- additional_gears[additional_gears$fishing_sector == sector, ]
#     additional_catch <- additional_catch[!is.na(additional_catch$Guild), ]

#     target_se2e_gears <- contained_catch[contained_catch$fishing_sector == sector, ]$gear_type_se2e
#     n_target_se2e_gears <- length(target_se2e_gears)

#     for (guild in additional_catch$Guild) {
#         # Calculate the amount of additional-sector catch that should be redistributed to each of the existing StrathE2E gears of the target sector
#         additional_guild_sector_catch_per_gear <- filter(additional_catch, Guild == guild)$tonnes / n_target_se2e_gears

#         for (t_gear in target_se2e_gears) {
#             prior_catch <- guild_gear_df[guild_gear_df$Guild == guild & guild_gear_df$gear_type_se2e == t_gear, ]$tonnes
#             prior_catch <- ifelse(is.na(prior_catch), 0, prior_catch)

#             guild_gear_df[guild_gear_df$Guild == guild & guild_gear_df$gear_type_se2e == t_gear, ]$tonnes <- prior_catch + additional_guild_sector_catch_per_gear
#         }
#     }

#     return(guild_gear_df)
# }

# filter(gear_type_se2e != "other gears")

catch_matrix_data <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., guild_gear_catch[, c("gear_type_se2e", "Guild", "tonnes")], by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA" & Guild != "") # %>%

# Add longline bird landings data (catch for birds from longlines are added to landings because birds are taken to port in this fishery)
# Data values taken from Rollinson et al (2017). Patterns and trends in seabird bycatch in the pelagic longline fishery off South Africa
additional_bird_longline <- c(
    482 * 3.8977, # Thalassarche cauta/steadi
    159 * 3.2029, # Thalassarche melanophris
    77 * 2.1288, # Thalassarche carteri
    18 * 2.1288, # Thlassarche chlororhynchos
    3 * 8.9056, # Diomedea sandfordi/epomophora
    5 * 6.9613, # Diomedea exulans
    7 * 4.2063, # Macronectes halli/giganteus
    1541 * 1.213, # Procellaria aequinoctialis
    1 * 1.131, # Procellaria cinerea
    1 * 0.4296, # Daption capense
    2 * 0.849, # Puffinus gravis
    2 * 1.650, # Catharacta antarctica
    45 * 2.643 # Morus capensis
)
additional_bird_longline <- sum(additional_bird_longline) / 1000 / 8 # Calculate the annual average (study period 8 years) and convert to tonnes from kg
catch_matrix_data[catch_matrix_data$Guild == "Birds" & catch_matrix_data$gear_type_se2e == "longline", ]$tonnes <- additional_bird_longline

# redistributed_catch <- redistribute_unaccounted_gears(catch_matrix_data, guild_gear_catch, guild_gear_catch_other_gears, "Industrial")
# redistributed_catch <- redistribute_unaccounted_gears(redistributed_catch, guild_gear_catch, guild_gear_catch_other_gears, "Artisanal")
# redistributed_catch <- redistribute_unaccounted_gears(redistributed_catch, guild_gear_catch, guild_gear_catch_other_gears, "Recreational")

# redistributed_catch$redistributed_tonnes <- ifelse(is.na(catch_matrix_data$tonnes), 0, catch_matrix_data$tonnes) - redistributed_catch$tonnes
# redistributed_catch <- filter(redistributed_catch, Guild != "NA" & Guild != "")

# mutate(tonnes = ifelse(is.na(tonnes), 0, tonnes)) %>%
# pivot_wider(names_from = gear_type_se2e, values_from = tonnes) %>%
# column_to_rownames("Guild") %>%
# as.matrix() %>%
# .[order(row.names(.)), order(colnames(.))]

ggplot() +
    geom_tile(data = catch_matrix_data, aes(x = gear_type_se2e, y = Guild, fill = tonnes)) +
    scale_fill_viridis_c() +
    ggtitle("Catch before redistributing unaccounted geartypes based on sector")
ggplot() +
    geom_tile(data = redistributed_catch, aes(x = gear_type_se2e, y = Guild, fill = tonnes)) +
    scale_fill_viridis_c() +
    ggtitle("Catch after redistributing unaccounted geartypes based on sector")

ggplot() +
    geom_tile(data = catch_heatmap, aes(x = gear_type_se2e, y = Guild, fill = log(tonnes))) +
    scale_fill_viridis_c()


# Discards by StrathE2E gear type and guild
guild_gear_discards <- discards %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% linefishery_sau_gears ~ "linefishery",
        gear_type == small_lines_squidjig_sau_gears ~ "small scale lines / squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    group_by(gear_type_se2e, Guild) %>%
    summarise(tonnes = sum(tonnes)) %>%
    filter(gear_type_se2e != "other gears")

discards_heatmap <- expand.grid(
    Guild = strathe2e_guilds,
    gear_type_se2e = strathe2e_gear_types
) %>%
    left_join(., guild_gear_discards, by = c("gear_type_se2e", "Guild")) %>%
    filter(Guild != "NA") # %>%

ggplot() +
    geom_tile(data = discards_heatmap, aes(x = gear_type_se2e, y = Guild, fill = tonnes)) +
    scale_fill_viridis_c()
ggplot() +
    geom_tile(data = discards_heatmap, aes(x = gear_type_se2e, y = Guild, fill = log(tonnes))) +
    scale_fill_viridis_c()
