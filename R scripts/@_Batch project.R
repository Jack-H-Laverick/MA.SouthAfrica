
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

library(tidyverse)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
# "./R scripts/bathymetry/bathymetry.01 DATA WRANGLING.R",
# "./R scripts/bathymetry/bathymetry.02 PLOTTING.R",
# "./R scripts/bathymetry/bathymetry.03 DEFINE DOMAIN.R",
# 
# "./R scripts/ne/nemo-ersem.01 BATH.R",
# "./R scripts/ne/nemo-ersem.02 MONTHLY EXTRACTION.R",
# "./R scripts/ne/nemo-ersem.03 DAILY EXTRACTION.R",
# "./R scripts/ne/nemo-ersem.04 V EXTRACTION.R",
# "./R scripts/ne/nemo-ersem.07 SPATIAL.R",
# "./R scripts/ne/nemo-ersem.08 TIME SERIES.R",
# "./R scripts/ne/nemo-ersem.09 PLOTTING.R"

# "./R scripts/flows/flows.01 VERTICAL DIFFUSIVITY.R",
# "./R scripts/flows/flows.02 WAVES.R",
# "./R scripts/flows/flows.03 MAKE TRANSECTS.R",
# "./R scripts/flows/flows.04 LABEL TRANSECTS NE.R",
# "./R scripts/flows/flows.05 SAMPLE TRANSECTS NE.R",
#  
#  "./R scripts/saltless/saltless.01 ATMOSPHERE ISIMIP.R",
#  "./R scripts/saltless/saltless.02 NE LIGHT.R",
#  "./R scripts/saltless/saltless.04 NE RIVERS.R",

"./R scripts/se2e_NE/strathE2E.01 INITIALISE MODEL.R",
"./R scripts/se2e_NE/strathE2E.02 COMPILE BOUNDARY FILE.R",
"./R scripts/se2e_NE/strathE2E.03 COMPILE PHYSICS FILE.R",
"./R scripts/se2e_NE/strathE2E.04 COMPILE PHYSICAL PARAMETERS.R",
 "./R scripts/se2e_NE/strathE2E.05 COMPILE FISHING FLEET.R",
"./R scripts/se2e_NE/strathE2E.06 EVENT TIMINGS.R",
# "./R scripts/se2e_NE/strathE2E.07 COMPILE TARGETS.R",
"./R scripts/se2e_NE/strathE2E.08 MODEL CLEANUP.R",
#"./R scripts/se2e_NE/strathE2E.09 PLOT UPDATE.R"
## "./R scripts/se2e_NE/strathE2E.10 FIXES.R"
 "./R scripts/se2e_NE/strathE2E.11 BALANCE VOLUMES.R"
# "./R scripts/se2e_NE/strathE2E.11 FIT ECO.R",
# "./R scripts/se2e_NE/strathE2E.12 SPLIT VARIANTS",
# "./R scripts/se2e_NE/strathE2E.13 INITIAL CONDITIONS.R",
# "./R scripts/se2e_NE/strathE2E.14 FUTURE STEADY STATES.R"
) %>% 
  map(MiMeMo.tools::execute)                                                           # Run the scripts

#### Plot run times ####

timings <- tictoc::tic.log(format = F) %>%                                             # Get the log of timings
  lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
  bind_rows() %>%                                                                      # Get a single dataframe
  separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
  separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
  mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
saveRDS(timings, "./Objects/Run time.rds")

#source("./R scripts/@_Script runtimes.R")                                              # Plot run times
