#### Setup                                            ####

library(tidyverse)
source("./R scripts/@_Region file.R")
source("./R scripts/@_model_config.R")

# #### Turn off fishing effort ####

# Effort <- read.csv(str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity_CELTIC_SEA_2003-2013.csv")) %>%
#   mutate(`Activity_.s.m2.d.` = 0)

# write.csv(Effort, row.names = F,
#           file = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity_{toupper(implementation)}_2010-2019.csv"))   # Read in example boundary drivers

R.utils::copyDirectory(
    "../Celtic Sea/Data/Celtic_Sea_ERSEM_4/2003-2013/", # Copy example model
    stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/")
)

# Copy ready fishing activity file
file.copy(
    from = "./Objects/fishing_activity_{implementation}_{start_year}-{end_year}.csv",
    to = "./StrathE2E/Models/{implementation}/2010-2019/Param/",
    overwrite = TRUE
)

# Copy fishing distribution file
file.copy(
    from = "./Objects/fishing_distribution_{implementation}_{start_year}-{end_year}.csv",
    to = "./StrathE2E/Models/{implementation}/2010-2019/Param/",
    overwrite = TRUE
)

# Copy fishing power
file.copy(
    from = glue("./Objects/fishing_power_{implementation}_{start_year}-{end_year}.csv"),
    to = "./StrathE2E/Models/{implementation}/2010-2019/Param/",
    overwrite = TRUE
)

# Copy fishing discards
file.copy(
    from = glue("./Objects/fishing_discards_{implementation}_{start_year}-{end_year}.csv"),
    to = "./StrathE2E/Models/{implementation}/2010-2019/Param/",
    overwrite = TRUE
)

# Copy fishing processing
file.copy(
    from = glue("./Objects/fishing_processing_{implementation}_{start_year}-{end_year}.csv"),
    to = "./StrathE2E/Models/{implementation}/2010-2019/Param/",
    overwrite = TRUE
)

# Copy fishing fleet file (non-gear specific parameters) from Celtic sea
file.copy(
    from = "../Celtic Sea/Data/Celtic_Sea_ERSEM_4/2003-2013/Param/fishing_fleet_Celtic_Sea_ERSEM_4_2003-2013.csv",
    to = glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_fleet_{implementation}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Create fishing gear multiplier and linkage files for our gears
fishing_gear_multiplier <- data.frame(
    Gear_name = unname(strathe2e_gear_types),
    Gear_code = names(strathe2e_gear_types),
    Multiplier_to_be_applied_to_activity = rep(1, length(strathe2e_gear_types))
)
write.csv(fishing_gear_multiplier, glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_gear_multiplier_{implementation}_{start_year}-{end_year}.csv"), row.names = FALSE)

fishing_gear_linkage <- data.frame(
    Gear_id = 1:12,
    Gear_name = c(unname(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_code = c(names(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_to_which_linked = c(),
    Linkage_coefficient = c(),
    Comments = c()
)
write.csv(fishing_gear_linkage, glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_gear_linkages_{implementation}_{start_year}-{end_year}.csv"), row.names = FALSE)
