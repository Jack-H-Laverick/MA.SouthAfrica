
## Jack fitting a model

rm(list=ls())                                                                   # Wipe the brain
library(tidyverse)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting"))

results <- e2e_run(model,nyears = 50)                                # Run the model

#### Initial Ecology fit ####

## Deactivate fishing related target data to first just fit the model to the ecology.




## Are the patterns in the drivers the same as the target data? If the patterns are way off they can't be used and need deactivating

# Satellite chlorophyll vs phytoplankton drivers


# nutrient concentrations vs drivers



## Launch ecology fitting process

fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=200, start_temperature=1,
                                     csv.output=TRUE)


## Once ecology fit has stabilised move onto fishing fit

#### Initial fishing fit ####

## Reactivate fishing targets


## Initial guess at Harvest Ratio Scaling factors on the basis of how far away from the landings we are already


## Launch fitting for HR scaling factors




#### Now refit ecology with a vagule realistic fishing fleet ####


fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=200, start_temperature=1,
                                 csv.output=TRUE)