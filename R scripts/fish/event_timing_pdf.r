library(rfishbase)
library(pdftools)
library(tidyverse)
library(glue)
library(readxl)

#### AFTER INFO EXTRACTION MANUAL PROCESSING AND FORMATTING OF TIMING INFORMATION PERFORMED TO CREATE CSV OF SPECIES TIMING DATA ####

linefish_doc <- pdftools::pdf_text("../../Fishing Data/Southern_African_Marine_Linefish_Species_Profiles.pdf")

extract_linefish_info <- function(page) {
    by_line <- unlist(str_split(page, "\n"))

    species_name <- by_line[str_detect(by_line, "SCIENTIFIC NAME:")] %>%
        str_split_i(., "(?<=(: ))", 2) %>%
        str_split_i(., "(?=( [(]))", 1)

    common_names <- by_line[str_detect(by_line, "COMMON NAME")] %>%
        str_split_i(., "(?<=(: ))", 2)

    spawning_season <- str_split_i(page, "spawning season: ", 2) %>%
        str_split_i(., "\nBreed", 1) %>%
        str_split(., "\n") %>%
        unlist() %>%
        glue_collapse(., sep = " ")
    spawning_loc <- str_split_i(page, "spawning locality: ", 2) %>%
        str_split_i(., "\nAge", 1) %>%
        str_split(., "\n") %>%
        unlist() %>%
        glue_collapse(., sep = " ")

    movement <- str_split_i(page, "MOVEMENT: ", 2) %>%
        str_split_i(., "\n\nHABITAT", 1) %>%
        str_split(., "\n") %>%
        unlist() %>%
        glue_collapse(., sep = " ")

    return(c(species_name, common_names, spawning_season, spawning_loc, movement))
}

linefish_doc <- linefish_doc[13:307] # Select only pages with species profiles

m <- matrix(data = NA, nrow = length(linefish_doc), ncol = 5)
fish_info <- data.frame(m)
colnames(fish_info) <- c("species_name", "common_name", "spawning_season", "spawning_locality", "movement")

for (page in seq_along(linefish_doc)) { # Check each page in the pdf document
    by_line <- unlist(str_split(linefish_doc[page], "\n"))

    if (any(str_detect(by_line, "SCIENTIFIC NAME"))) { # Check if page has species information.
        sa_dist <- by_line[str_detect(by_line, "AFRICAN DISTRIBUTION")]
        sa_dist <- str_split(str_split(sa_dist, ": ")[[1]][[2]], ", ")[[1]]

        if (any(c("NC", "WC") %in% sa_dist)) { # Check if species is distributed in Sthe Southern Benguela domain region.
            fish_info[page, ] <- extract_linefish_info(linefish_doc[page])
        }
    }
}

complete_rows <- apply(fish_info, 1, function(x) !all(is.na(x)))
fish_info <- fish_info[complete_rows, ]
write.csv(fish_info, "../../Fishing Data/linefish_species_profiles_extracted.csv")

#### MANUAL PROCESSING AND FORMATTING OF TIMING INFORMATION PERFORMED TO CREATE CSV OF SPECIES TIMING DATA, as well as inclusion of outside literature: ####
event_timing <- readxl::read_excel("../../Fishing Data/event_timing_literature.xlsx", skip = 3)
event_timing <- event_timing[apply(event_timing, 1, function(x) !all(is.na(x))), ] # Remove rows that are completely empty
event_timing <- event_timing[!str_detect(event_timing$event, pattern = "#"), ]
event_timing <- event_timing[!(str_detect(event_timing$event, pattern = "dur")), ] # Remove duration values as they can be calculated from averaged timings
event_timing <- event_timing[!(str_detect(event_timing$guild, "migrat")), ] # Remove migratory fish timings to add from Lynne and Kelly later

event_timing <- event_timing %>%
    group_by(event, species, guild, EwE_group_name) %>%
    mutate(value = mean(value)) # Incase there are multiple sources for a single species take the average of the values

se2e_ewe_match <- read_excel("../../Fishing Data/Strath_EwE_match.xlsx")
se2e_ewe_match <- se2e_ewe_match[!is.na(se2e_ewe_match$EwE_group_biomass_t_km2), ]
se2e_ewe_match <- se2e_ewe_match %>%
    group_by(StrathE2E_group_name) %>%
    mutate(se2e_group_biomass = sum(EwE_group_biomass_t_km2)) %>%
    ungroup() %>%
    mutate(prop_contribution_to_se2e_group_b = EwE_group_biomass_t_km2 / se2e_group_biomass)

# Check that all event timing group names are the same as a group in the master list of groups
all(unique(event_timing$EwE_group_name) %in% se2e_ewe_match$EwE_group_name)

average_event_timing <- event_timing %>%
    left_join(., se2e_ewe_match, by = "EwE_group_name") %>%
    group_by(event, guild) %>%
    summarise(value = weighted.mean(value, prop_contribution_to_se2e_group_b)) %>%
    mutate(
        event_order = str_split_i(event, " ", 2),
        event_type = str_split_i(event, " ", 1)
    ) %>%
    ungroup() %>%
    select(guild, event_type, event_order, value)

event_durations <- expand.grid(guild = unique(average_event_timing$guild), event_type = unique(average_event_timing$event_type), event_order = "duration")
event_durations$value <- 0

for (row in seq_len(nrow(average_event_timing))) {
    row_df <- average_event_timing[row, ]
    if (row_df$event_order == "end") {
        end_timing <- row_df$value
        start_timing <- average_event_timing[
            (average_event_timing$event_type == row_df$event_type) &
                (average_event_timing$guild == row_df$guild) &
                (average_event_timing$event_order == "start"),
        ]$value

        if (start_timing > end_timing) {
            end_timing <- end_timing + 365
        }
        duration <- end_timing - start_timing
        event_durations[
            event_durations$guild == row_df$guild &
                event_durations$event_type == row_df$event_type,
        ]$value <- duration
    }
}

event_timing_final <- rbind(
    average_event_timing,
    event_durations,
    data.frame(
        guild = c("migratory fish", "migratory fish"),
        event_type = c("immigration", "immigration"),
        event_order = c("start", "end"),
        value = c(273, 60)
    )
)
event_timing_final <- event_timing_final[with(event_timing_final, order(guild, event_type, event_order)), ]
