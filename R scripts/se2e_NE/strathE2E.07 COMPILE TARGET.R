library(readxl)
library(tidyverse)
library(glue)

source("@_model_config.R")

target_data <- read_excel("./Data/annual_observed_SB_revised_v2.xlsx")
target_data$Annual_measure <- as.numeric(target_data$Annual_measure)
target_data$SD_of_measure <- as.numeric(target_data$SD_of_measure)

target_data <- target_data %>%
    mutate(Guild_nitrogen = mMNpergWW[Guild_of_values]) %>% # Add Guild conversion factor to go from t/km^2 or g/m^2 to mMN/m^2
    mutate(
        Annual_measure = if_else(
            (Units == "t/km²/yr") & (!is.na(Annual_measure)) & (!is.na(Guild_nitrogen)),
            Annual_measure * Guild_nitrogen,
            Annual_measure
        )
    ) %>% # If the units of target data are t/km^2 convert to mMN/m^2
    mutate(
        SD_of_measure = if_else(
            (Units == "t/km²/yr") & (!is.na(SD_of_measure)) & (!is.na(Guild_nitrogen)),
            SD_of_measure * Guild_nitrogen,
            SD_of_measure
        )
    ) %>% # If the units of target data are t/km^2 convert to mMN/m^2
    mutate(Units = if_else(Units == "t/km²/yr", "mMN/m2/y", Units)) %>% # Recode the units to correctly state mMN/m^2
    select(Annual_measure, SD_of_measure, Use1_0, Name, Units, Description, Region, Time_period, Source)

target_data[target_data$Name == "Obs_KelpP", ]$Units <- "gC/m2/y"

# Assign values from woa extracted nitrate concentrations to target data slots
woa_nitrate <- read.csv("./Objects/woa23_nitrate_concentrations.csv") %>%
    mutate(mnths = if_else(season == "winter", "NDJF", "MJJA")) %>%
    mutate(dl = if_else(depth == "deep", "d", "s")) %>%
    mutate(Name = glue("Obs_{mnths}_{dl}_nitrate")) %>%
    rename(Annual_measure = mean_conc, SD_of_measure = std_conc) %>%
    select(Annual_measure, SD_of_measure, Name)

target_data <- rows_update(target_data, woa_nitrate, by = "Name")

landings_target <- read.csv("./Objects/guild_landings_target_data.csv") %>%
    mutate(guild_short = case_when(
        Guild_code == "CSB" ~ "Bc",
        Guild_code == "FDB" ~ "Bs",
        Guild_code == "DF" ~ "D",
        Guild_code == "KP" ~ "K",
        Guild_code == "MF" ~ "M",
        Guild_code == "PF" ~ "P",
        Guild_code == "CZ" ~ "Zc"
    )) %>%
    mutate(Name = glue("Obs_{guild_short}land_livewt")) %>%
    mutate(Source = "Seas_Around_Us", Time_period = "2010-2019") %>%
    rename(Annual_measure = annual_average_landings_mMN, SD_of_measure = annual_std_landings_mMN) %>%
    select(Annual_measure, SD_of_measure, Name, Time_period, Source)

target_data <- rows_update(target_data, landings_target, by = "Name")

discard_target <- read.csv("./Objects/guild_discards_target_data.csv") %>%
    mutate(guild_name = case_when(
        Guild_code == "BD" ~ "bird",
        Guild_code == "SL" ~ "seal",
        Guild_code == "CT" ~ "ceta"
    )) %>%
    mutate(Name = glue("Obs_{guild_name}disc")) %>%
    mutate(Source = "Seas_Around_Us", Time_period = "2010-2019", Region = "Southern_Benguela") %>%
    rename(Annual_measure = annual_average_discard_mMN, SD_of_measure = annual_std_discard_mMN) %>%
    select(Annual_measure, SD_of_measure, Name, Time_period, Source, Region)
target_data <- rows_update(target_data, discard_target, by = "Name")

write.csv(target_data, glue("./StrathE2E/Models/{implementation}/2010-2019/Param/annual_observed_{implementation}_{start_year}-{end_year}.csv"), row.names = FALSE)
