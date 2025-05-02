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

total_landings <- guild_landings %>%
    group_by(year, gear_type) %>%
    summarise(tonnes = sum(tonnes_mean)) %>%
    group_by(year) %>%
    mutate(proportion = tonnes / sum(tonnes))

palette_n <- distinctColorPalette(length(unique(total_landings$gear_type)))
anim_plot <- ggplot() +
    geom_col(data = total_landings, aes(x = gear_type, y = proportion, fill = gear_type)) +
    transition_states(year) +
    labs(title = "Year: {closest_state}") +
    theme_minimal() +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    scale_fill_discrete(palette_n)
anim_save("./Figures/seas_around_us_gear_proportions.gif", anim_plot)

total_landings_small_sum <- total_landings %>%
    mutate(gear_type = if_else(
        str_detect(gear_type, "small scale"),
        "small_scale",
        gear_type
    )) %>%
    group_by(year, gear_type) %>%
    summarise(tonnes = sum(tonnes)) %>%
    mutate(proportion = tonnes / sum(tonnes))
ggplot() +
    geom_area(
        data = total_landings_small_sum,
        aes(x = year, y = proportion, fill = gear_type),
        color = "gray"
    ) +
    theme_minimal() +
    scale_fill_manual(values = palette_n)
ggsave("./Figures/seas_around_us_gear_proportions_small_summed.png")

se2e_gear_landings <- guild_landings %>%
    mutate(gear_type_se2e = case_when(
        gear_type %in% c("bottom trawl", "pelagic trawl") ~ "demersal + midwater trawl",
        gear_type %in% c("bagnets", "cast nets", "small scale gillnets", "small scale encircling nets", "small scale seine nets", "small scale other nets", "gillnet") ~ "nets including small scale",
        gear_type %in% c("hand lines", "pole and line") ~ "linefishery",
        gear_type %in% c("small scale lines", "recreational fishing gear") ~ "small scale lines / squid jig",
        gear_type == "longline" ~ "'longline'",
        gear_type == "purse seine" ~ "purse seine",
        .default = "other gears"
    )) %>%
    group_by(year, gear_type_se2e, Guild) %>%
    summarise(tonnes = sum(tonnes_mean))

total_se2e_gear_landings <- se2e_gear_landings %>%
    group_by(year, gear_type_se2e) %>%
    summarise(tonnes = sum(tonnes)) %>%
    mutate(proportion = tonnes / sum(tonnes))

ggplot() +
    geom_col(
        data = se2e_gear_landings[!str_detect(se2e_gear_landings$Guild, "(#)"), ],
        aes(x = year, y = tonnes, fill = Guild)
    ) +
    facet_wrap(~gear_type_se2e, scales = "free_y") +
    theme_minimal()

ggplot() +
    geom_area(
        data = total_se2e_gear_landings,
        aes(x = year, y = proportion, fill = gear_type_se2e),
        color = "gray"
    ) +
    theme_minimal()
