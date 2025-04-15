library(rfishbase)
library(pdftools)
library(tidyverse)
library(glue)

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

        if (any(c("NC", "WC") %in% sa_dist)) { # Check if species is distributed in the Southern Benguela domain region.
            fish_info[page, ] <- extract_linefish_info(linefish_doc[page])
        }
    }
}

complete_rows <- apply(fish_info, 1, function(x) !all(is.na(x)))
fish_info <- fish_info[complete_rows, ]
write.csv(fish_info, "../../Fishing Data/linefish_species_profiles_extracted.csv")
