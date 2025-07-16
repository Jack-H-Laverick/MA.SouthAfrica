library(readxl)
library(tidyverse)
library(glue)

source("@_model_config.R")

annual_template <- read_excel("./Data/annual_observed_SB_revised_v2.xlsx")
annual_template$Annual_measure <- as.numeric(annual_template$Annual_measure)
annual_template$SD_of_measure <- as.numeric(annual_template$SD_of_measure)

annual_template <- annual_template %>%
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

annual_template[annual_template$Name == "Obs_KelpP", ]$Units <- "gC/m2/y"

# Calculate the Standard Deviation of provided values as 75% of annual measure (before adding in SD of landings/WOA nitrate).
annual_template <- annual_template %>%
    mutate(SD_of_measure = if_else(!is.na(Annual_measure), 0.75 * Annual_measure, NA))

# Assign values from woa extracted nitrate concentrations to target data slots
woa_nitrate <- read.csv("./Objects/woa23_nitrate_concentrations.csv") %>%
    mutate(mnths = if_else(season == "winter", "NDJF", "MJJA")) %>%
    mutate(dl = if_else(depth == "deep", "d", "s")) %>%
    mutate(Name = glue("Obs_{mnths}_{dl}_nitrate")) %>%
    rename(Annual_measure = mean_conc, SD_of_measure = std_conc) %>%
    select(Annual_measure, SD_of_measure, Name)

annual_template <- rows_update(annual_template, woa_nitrate, by = "Name")

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

annual_template <- rows_update(annual_template, landings_target, by = "Name")

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
annual_template <- rows_update(annual_template, discard_target, by = "Name")

targets <- read.csv(stringr::str_glue("./Objects/fitting/PP_target{toupper{implementation}}.csv"))

annual_new <- mutate(annual_template,
    Annual_measure = case_when(
        Description == targets$Description ~ targets$Annual_measure,
        T ~ Annual_measure
    ),
    SD_of_measure = case_when(
        Description == targets$Description ~ targets$SD_of_measure,
        T ~ SD_of_measure
    ), # Add SD
    Use1_0 = if_else(Description == targets$Description, 1, Use1_0)
) # switch on

# Switch on all filled target values
annual_new <- annual_new %>%
    mutate(Use1_0 = if_else(!is.na(Annual_measure) && !is.na(SD_of_measure), 1, 0))

write.csv(annual_new, glue("./StrathE2E/Models/{implementation}/2010-2019/Param/annual_observed_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)
