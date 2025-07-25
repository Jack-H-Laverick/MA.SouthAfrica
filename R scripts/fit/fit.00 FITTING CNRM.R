
## Jack fitting a model

rm(list=ls())                                                                   # Wipe the brain
library(tidyverse)
library(StrathE2E2)
library(tictoc)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-1"))

results <- e2e_run(model,nyears = 50)                                # Run the model

#### Initial Ecology fit ####

## Deactivate fishing related target data to first just fit the model to the ecology.

fishing_targets <- c("Proportion_discards_in_diet_of_birds",
                     "Annual_planktivorous_fish_landings_(live_weight)",
                     "Annual_demersal_fish_landings_(live_weight)",
                     "Annual_migratory_fish_landings_(live_weight)",
                     "Annual_susp/dep_benthos_landings_(live_weight)",
                     "Annual_carn/scav_benthos_landings_(live_weight)",
                     "Annual_carn_zooplankton_landings_(live_weight)",
                     "Annual_macrophyte_landings_(live_weight)",
                     "Proportion_of_demersal_fish_catch_discarded",
                     "Annual_bycatch_of_birds",
                     "Annual_bycatch_of_pinnipeds",
                     "Annual_bycatch_of_cetaceans",
                     "Obs_cetacean_landings_by_whale_hunters"
                     )

annual_targets <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/annual_observed_{toupper(implementation)}_2010-2019.csv")) %>% 
  mutate(Use1_0 = if_else(Description %in% fishing_targets, 0, Use1_0))

write.csv(annual_targets, glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv"), row.names = FALSE)


## Are the patterns in the drivers the same as the target data? If the patterns are way off they can't be used and need deactivating

# Satellite chlorophyll vs phytoplankton drivers


# nutrient concentrations vs drivers



## Launch ecology fitting process

fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=200, start_temperature=1,
                                     csv.output=TRUE)

## Keep launching fitting processes until stabilised

round <- 2                                                                     # Fitting round, to pull in new files and update name

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")) 

Setup_file[8,1] <- stringr::str_glue("fitted_preference_matrix-2010-2015-CNRM-ssp370-fitting-{round-1}.csv") # Pull in last set of accepted parameters
Setup_file[9,1] <- stringr::str_glue("fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
Setup_file[10,1] <- stringr::str_glue("fitted_microbiology_others-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv"),
          row.names = F)

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}")) # reload model to update ident

tic()
fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=500, start_temperature=1,   # Go again with the fitting
                                 csv.output=TRUE)
toc()

## If another round is needed, increment round +1 and rerun the section above.
## Once ecology fit has stabilised move onto fishing fit

#### Initial fishing fit ####

## Reactivate fishing targets


## Initial guess at Harvest Ratio Scaling factors on the basis of how far away from the landings we are already


## Launch fitting for HR scaling factors




#### Now refit ecology with a vagule realistic fishing fleet ####


fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=200, start_temperature=1,
                                 csv.output=TRUE)