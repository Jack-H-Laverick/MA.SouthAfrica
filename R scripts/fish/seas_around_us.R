library(tidyverse)
library(ggplot2)
library(gganimate)
library(randomcoloR)

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

landings <- left_join(landings, sau_in_guilds[, c("scientific_name", "Guild")], by = "scientific_name")
arrow::write_parquet(landings, "./Objects/sau_landings_assigned_guilds.parq")
guild_landings <- landings %>%
    mutate(Guild = if_else(is.na(Guild), paste0("#", functional_group), Guild)) %>% # Mark functional groups from seas-around-us with `#`
    group_by(year, gear_type, Guild) %>%
    summarise(tonnes_mean = mean(tonnes), tonnes_sd = sd(tonnes))

ggplot() +
    geom_line(data = guild_landings[!str_detect(guild_landings$Guild, "(#)"), ], aes(x = year, y = tonnes_mean, color = Guild)) +
    # geom_ribbon(data = guild_landings, aes(x = year, ymin = tonnes_mean - tonnes_sd, ymax = tonnes_mean + tonnes_sd), alpha = 0.5) +
    facet_wrap(~gear_type, scales = "free_y")

ggplot() +
    geom_line(data = landings[landings$gear_type == "longline", ], aes(x = year, y = tonnes, color = scientific_name))
