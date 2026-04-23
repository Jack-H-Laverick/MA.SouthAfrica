rm(list = ls()) # Wipe the brain
library(tidyverse)
library(StrathE2E2)
library(tictoc)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"),
    models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
    model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-1")
)

results <- e2e_run(model, nyears = 50) # Check the model runs

e2e_plot_ts(model, results)

#### Initial Ecology fit ####

## Deactivate fishing related target data to first just fit the model to the ecology.

fishing_targets <- c(
    "Proportion_discards_in_diet_of_birds",
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

annual_targets <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv")) %>%
    mutate(Use1_0 = if_else(Description %in% fishing_targets, 0, Use1_0))

write.csv(annual_targets, str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv"), row.names = FALSE)

round <- 1 # Fitting round, to pull in new files and update name

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv"))

if (round == 1) {
    Setup_file[8, 1] <- stringr::str_glue("fitted_preference_matrix_SENEGAL_CNRM.csv") # Pull in last set of accepted parameters
    Setup_file[9, 1] <- stringr::str_glue("fitted_uptake_mort_rates_SENEGAL_CNRM.csv")
    Setup_file[10, 1] <- stringr::str_glue("fitted_microbiology_others_SENEGAL_CNRM.csv")
} else {
    Setup_file[8, 1] <- stringr::str_glue("fitted_preference_matrix-2010-2015-CNRM-ssp370-fitting-{round-1}.csv") # Pull in last set of accepted parameters
    Setup_file[9, 1] <- stringr::str_glue("fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
    Setup_file[10, 1] <- stringr::str_glue("fitted_microbiology_others-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
}

# Switch to target data file with only production/P-B ratios/nitrate concentractions for fitting
Setup_file[23, 1] <- "annual_observed_SOUTH_AFRICA_MA_prod_only.csv"

write.csv(Setup_file,
    file = stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv"),
    row.names = F
)

hr <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/harvest_ratio_multiplier.csv"))
hr$Harvest_ratio_multiplier <- 0
write.csv(hr, stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/harvest_ratio_multiplier.csv"), row.names = F)

et <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/event_timing_SOUTH_AFRICA_MA_2010-2019.csv"))
et[et$Description == "Propn_of_ocean_population_entering_model_domain_each_year", ]$Value <- 0.1
write.csv(et, stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/event_timing_SOUTH_AFRICA_MA_2010-2019.csv"), row.names = F)

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"),
    models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
    model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}_2025_10_09")
)

results <- e2e_run(model, nyears = 50) # Check the model runs

e2e_plot_ts(model, results)

e2e_compare_obs(selection = "ANNUAL", model = model, results = results)

# This round of fitting created model parameter files with the identity "fitting-1_2025_10-09"
