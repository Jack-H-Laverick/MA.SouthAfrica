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
    "pole and line",
    "squid jig",
    "longline",
    "purse seine",
    "demersal trawl",
    "WC Rock Lobster traps",
    "recreational fishing gear",
    "small scale lines",
    "subsistence fishing gear"
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
pole_and_line_sau_gears <- "pole and line"
squidjig_sau_gears <- "squid jig"
longline_sau_gears <- "longline"
purseseine_sau_gears <- "purse seine"
demersal_trawl_sau_gears <- "bottom trawl"
wc_rock_lobster_sau_gears <- "WC Rock Lobster traps"
recreational_sau_gears <- "recreational fishing gear"
small_scale_lines_sau_gears <- c("small scale lines", "hand lines")
subsistence_sau_gears <- "subsistence fishing gear"

all_accounted_sau_gears <- c(
    midwater_trawl_sau_gears,
    nets_sau_gears,
    pole_and_line_sau_gears,
    squidjig_sau_gears,
    longline_sau_gears,
    purseseine_sau_gears,
    demersal_trawl_sau_gears,
    wc_rock_lobster_sau_gears,
    recreational_sau_gears,
    small_scale_lines_sau_gears,
    subsistence_sau_gears
)

for (r in seq_len(nrow(landings))) {
    row <- landings[r, ]
    # Redirect the `unknown class` landings that are WC Rock Lobster to a WC Rock Lobster fishery
    # Bycatch is not an issue in this industrial fishery (Status Report, 2023), so all of these landings
    # Can be attributed to a new fishery using traps in rocky areas of 100m+.
    if (row$gear_type == "unknown class" && row$scientific_name == "Jasus lalandii") {
        landings[r, ]$gear_type <- "WC Rock Lobster traps"
    }

    # Redirect the `small scale lines` landings that are squid to a `squid jig` gear type.
    # Bycatch is not an issue in the squid jig gear type due to the high selectivity of squid jigs
    if (row$gear_type == "small scale lines" && !is.na(row$Guild) && row$Guild == "Zooplankton carnivore") {
        landings[r, ]$gear_type <- "squid jig"
    }
}

# Landings by StrathE2E gear type and guild
strath_e2e_gear_landings <- landings %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% pole_and_line_sau_gears ~ "pole and line",
        gear_type == squidjig_sau_gears ~ "squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        gear_type == wc_rock_lobster_sau_gears ~ "WC Rock Lobster traps",
        gear_type == recreational_sau_gears ~ "recreational fishing gear",
        gear_type %in% small_scale_lines_sau_gears ~ "small scale lines",
        gear_type == subsistence_sau_gears ~ "subsistence fishing gear",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    filter(!is.na(Guild) & year >= start_year & year <= end_year)
arrow::write_parquet(strath_e2e_gear_landings, "./Objects/sau_landings_strath_gears.parq")

# Discards by StrathE2E gear type and guild
strath_e2e_gear_discards <- discards %>%
    mutate(gear_type_se2e = case_when(
        gear_type == midwater_trawl_sau_gears ~ "midwater trawl",
        gear_type %in% nets_sau_gears ~ "nets including small scale",
        gear_type %in% pole_and_line_sau_gears ~ "pole and line",
        gear_type == squidjig_sau_gears ~ "squid jig",
        gear_type == longline_sau_gears ~ "longline",
        gear_type == purseseine_sau_gears ~ "purse seine",
        gear_type == demersal_trawl_sau_gears ~ "demersal trawl",
        gear_type == wc_rock_lobster_sau_gears ~ "WC Rock Lobster traps",
        gear_type == recreational_sau_gears ~ "recreational fishing gear",
        gear_type %in% small_scale_lines_sau_gears ~ "small scale lines",
        gear_type == subsistence_sau_gears ~ "subsistence fishing gear",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Industrial" ~ "other industrial",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Artisanal" ~ "other artisinal",
        # !gear_type %in% all_accounted_sau_gears & fishing_sector == "Subsistence" ~ "other subsistence",
        .default = "other gears"
    )) %>%
    filter(!is.na(Guild) & year >= start_year & year <= end_year)
arrow::write_parquet(strath_e2e_gear_discards, "./Objects/sau_discards_strath_gears.parq")

# ggplot() +
#     geom_area(
#         data = squid_jig_prop,
#         aes(x = year, y = proportion, fill = Guild),
#         color = "gray"
#     ) +
#     theme_minimal() +
#     labs(y = "proportion_of_landings")



# Comparing Seas Around Us data and pelagic longline landings data from ITOC tuna report
# pll_report <- extract_tables("../../Fishing Data/mike_effort_data/IOTC-2021-SC24-NR25_-_South_Africa.pdf", output = "tibble", pages = c(8))[[1]][5:15, ]
# names(pll_report) <- c("year", "nhooks", "Bigeye tuna", "Yellowfin tuna", "Albacore", "Southern Bluefin tuna", "Swordfish", "Skipjack", "Shortfin mako", "Blue shark", "NEI")
# remove_num_space <- function(x) {
#     return(paste(str_split(x, "( )")[[1]], collapse = ""))
# }

# pll_catch <- pll_report %>%
#     select(!nhooks) %>%
#     pivot_longer(!year, names_to = "common_name", values_to = "tonnes") %>%
#     mutate(tonnes = as.numeric(tonnes))

# ggplot() +
#     geom_col(
#         data = strath_e2e_gear_landings[strath_e2e_gear_landings$gear_type_se2e == "longline" & strath_e2e_gear_landings$Guild == "Migratory", ],
#         aes(x = year, y = tonnes, fill = common_name)
#     )
# ggplot() +
#     geom_col(
#         data = pll_catch,
#         aes(x = year, y = tonnes, fill = common_name)
#     )
