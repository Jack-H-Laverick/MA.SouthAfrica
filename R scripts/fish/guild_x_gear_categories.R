library(tidyverse)

species_fishery <- readxl::read_excel("../../Fishing Data/Annexure 5_Harvested Marine Species.xlsx", sheet = 2)
species_targetted <- species_fishery %>%
    pivot_longer(!c("Broad Grouping", "Species", "Common name"), names_to = "targetted_by") %>%
    select(!value) %>%
    rename(., c(broad_grouping = "Broad Grouping", species = "Species", common_name = "Common name"))

# The species need further classification into broad guilds. The following groups are not model `guilds`.
broad_group_x_gear <- unique(species_targetted[, c("broad_grouping", "targetted_by")])
