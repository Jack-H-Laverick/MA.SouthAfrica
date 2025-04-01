library(tidyverse)

species_fishery <- readxl::read_excel("../../Fishing Data/Annexure 5_Harvested Marine Species.xlsx", sheet = 2)
species_targetted <- species_fishery %>%
    pivot_longer(!c("Broad Grouping", "Species", "Common name"), names_to = "targetted_by") %>%
    select(!value) %>%
    rename(., c(broad_grouping = "Broad Grouping", species = "Species", common_name = "Common name"))

# The species need further classification into broad guilds. The following groups are not model `guilds`.
broad_group_x_gear <- unique(species_targetted[, c("broad_grouping", "targetted_by")])
unique(broad_group_x_gear$targetted_by)
#  [1] "Pelagic Longline"
#  [2] "Demeral Shark Fishery"
#  [3] "Demersal Longline (Hake Longline)"
#  [4] "Tuna Pole"
#  [5] "Trek Net (Beach Seine)"
#  [6] "Gillnet"
#  [7] "Purse Seine (Small Pelagics)"
#  [8] "Linefish (commercial)"
#  [9] "Recreational linefishery (estuarine, intertidal, shore-based, boat-based)"
# [10] "Crustacean Trawl"
# [11] "Demersal Inshore Trawl"
# [12] "Demersal Offshore Trawl"
# [13] "Midwater Trawl"
# [14] "Patagonian Toothfishery"
# [15] "Small Scale Fishery (Subsistence and Small Scale Commercial/Basket)"
# [16] "West Coast Rocklobster Fishery"
# [17] "Squid Fishery"
# [18] "South Coast Rock Lobster Fishery"
# [19] "Oyster fishery"
# [20] "Seaweed Fishery"
# [21] "Bather Protection (KZN Shark Nets)"
