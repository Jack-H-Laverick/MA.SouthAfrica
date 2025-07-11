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

write.csv(target_data, glue("./Objects/annual_observed_{domain_name}_{start_year}-{end_year}.csv"), row.names = FALSE)
