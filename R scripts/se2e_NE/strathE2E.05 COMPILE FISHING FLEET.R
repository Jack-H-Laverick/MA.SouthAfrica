#### Setup                                            ####

library(tidyverse)
source("./R scripts/@_Region file.R")
source("./R scripts/@_model_config.R")

# Copy ready fishing activity file
activity <- read.csv(str_glue("./Objects/fishing_activity_{implementation}_{start_year}-{end_year}.csv"))
if (nrow(activity) < 12) {
    missing_rows <- 12 - nrow(activity)
    activity <- rbind(
        activity,
        setNames(
            data.frame(matrix(NA, ncol = ncol(activity), nrow = missing_rows)),
            colnames(activity)
        )
    )
}
write.csv(activity, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_activity_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)

# Copy fishing distribution file
distribution <- read.csv(str_glue("./Objects/fishing_distribution_{implementation}_{start_year}-{end_year}.csv"))
if (nrow(distribution) < 12) {
    missing_rows <- 12 - nrow(distribution)
    distribution <- rbind(
        distribution,
        setNames(
            data.frame(matrix(NA, ncol = ncol(distribution), nrow = missing_rows)),
            colnames(distribution)
        )
    )
}
write.csv(distribution, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_distribution_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)

# Copy fishing power
power <- read.csv(str_glue("./Objects/fishing_power_{implementation}_{start_year}-{end_year}.csv"))
if (nrow(power) < 12) {
    missing_rows <- 12 - nrow(power)
    power <- rbind(
        power,
        setNames(
            data.frame(matrix(NA, ncol = ncol(power), nrow = missing_rows)),
            colnames(power)
        )
    )
}
write.csv(power, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_power_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)

# Copy fishing discards
discards <- read.csv(str_glue("./Objects/fishing_discards_{implementation}_{start_year}-{end_year}.csv"))
if (nrow(discards) < 12) {
    missing_rows <- 12 - nrow(discards)
    discards <- rbind(
        discards,
        setNames(
            data.frame(matrix(NA, ncol = ncol(discards), nrow = missing_rows)),
            colnames(discards)
        )
    )
}
write.csv(discards, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_discards_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)

# Copy fishing processing
processing <- read.csv(str_glue("./Objects/fishing_processing_{implementation}_{start_year}-{end_year}.csv"))
if (nrow(processing) < 12) {
    missing_rows <- 12 - nrow(processing)
    processing <- rbind(
        processing,
        setNames(
            data.frame(matrix(NA, ncol = ncol(processing), nrow = missing_rows)),
            colnames(processing)
        )
    )
}
write.csv(processing, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_processing_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)

# Create fishing gear linkage file for our gears
fishing_gear_linkage <- data.frame(
    Gear_id = 1:12,
    Gear_name = c(unname(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_code = c(names(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_to_which_linked = rep(NA, 12),
    Linkage_coefficient = rep(NA, 12),
    Comments = rep(NA, 12)
)
write.csv(fishing_gear_linkage, str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_gear_linkages_{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)
