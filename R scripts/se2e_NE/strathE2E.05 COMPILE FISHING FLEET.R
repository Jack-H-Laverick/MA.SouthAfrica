#### Setup                                            ####

library(tidyverse)
source("./R scripts/@_Region file.R")
source("./R scripts/@_model_config.R")

# Copy ready fishing activity file
file.copy(
    from = str_glue("./Objects/fishing_activity{implementation}_{start_year}-{end_year}.csv"),
    to = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity{toupper(implementation)}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Copy fishing distribution file
file.copy(
    from = str_glue("./Objects/fishing_distribution{implementation}_{start_year}-{end_year}.csv"),
    to = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_distribution{toupper(implementation)}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Copy fishing power
file.copy(
    from = str_glue("./Objects/fishing_power{implementation}_{start_year}-{end_year}.csv"),
    to = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity{toupper(implementation)}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Copy fishing discards
file.copy(
    from = str_glue("./Objects/fishing_discards{implementation}_{start_year}-{end_year}.csv"),
    to = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_discards{toupper(implementation)}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Copy fishing processing
file.copy(
    from = str_glue("./Objects/fishing_processing{implementation}_{start_year}-{end_year}.csv"),
    to = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_discards{toupper(implementation)}_{start_year}-{end_year}.csv"),
    overwrite = TRUE
)

# Create fishing gear linkage file for our gears
fishing_gear_linkage <- data.frame(
    Gear_id = 1:12,
    Gear_name = c(unname(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_code = c(names(strathe2e_gear_types), rep(NA, 12 - length(strathe2e_gear_types))),
    Gear_to_which_linked = rep(NA, 12),
    Linkage_coefficient = rep(NA, 12),
    Comments = rep(NA, 12)
)
write.csv(fishing_gear_linkage, str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_gear_linkages{toupper(implementation)}_{start_year}-{end_year}.csv"), row.names = FALSE)
