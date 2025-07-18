
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

# Update file which tells StrathE2E where to find driving files

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/MODEL_SETUP.csv")) # Read in example Physical drivers

Setup_file[1,1] <- stringr::str_glue("physical_parameters_{toupper(implementation)}.csv")
Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_2010-2019.csv")
Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_2010-2019.csv")
Setup_file[5,1] <- stringr::str_glue("event_timing_{toupper(implementation)}_2010-2019.csv")
Setup_file[12,1] <- stringr::str_glue("fishing_activity_{toupper(implementation)}_2010-2019.csv")

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/MODEL_SETUP.csv"),
          row.names = F)

# Remove the files which have been replaced by ones for the new region
unlink(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/Driving/chemistry_CELTIC_SEA_2003-2013.csv"))
unlink(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/physical_parameters_CELTIC_SEA.csv"))
unlink(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/Driving/physics_CELTIC_SEA_2003-2013.csv"))     # Delete old file
unlink(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/event_timing_CELTIC_SEA_2003-2013.csv"))
unlink(stringr::str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity_CELTIC_SEA_2003-2013.csv"))

