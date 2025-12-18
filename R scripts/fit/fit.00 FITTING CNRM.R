
## Jack fitting a model

#### Set up ####

rm(list=ls())                                                                   # Wipe the brain
library(tidyverse)
library(StrathE2E2)
library(tictoc)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-1"))

results <- e2e_run(model,nyears = 50)                                           # Check the model runs

e2e_plot_ts(model, results)

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

annual_targets <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv")) %>% 
  mutate(Use1_0 = if_else(Description %in% fishing_targets, 0, Use1_0))

write.csv(annual_targets, str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv"), row.names = FALSE)

## Are the patterns in the drivers the same as the target data? If the patterns are way off they can't be used and need deactivating

# Satellite chlorophyll vs phytoplankton drivers


# nutrient concentrations vs drivers



## Launch ecology fitting process

fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=2000, start_temperature=1,
                                     csv.output=TRUE, toppredlock = TRUE, cooling = 1)

## Keep launching fitting processes until stabilised

round <- 4                                                                     # Fitting round, to pull in new files and update name

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")) 

Setup_file[8,1] <- stringr::str_glue("fitted_preference_matrix-2010-2015-CNRM-ssp370-fitting-{round-1}.csv") # Pull in last set of accepted parameters
Setup_file[9,1] <- stringr::str_glue("fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
Setup_file[10,1] <- stringr::str_glue("fitted_microbiology_others-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv"),
          row.names = F)

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}")) # reload model to update ident

#Inspect

results <- e2e_run(model,nyears = 50)                                           # Check the model runs

e2e_plot_ts(model, results)

e2e_compare_obs(selection = "ANNUAL", model = model, results = results)

# fit again

tic()
fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=2000, start_temperature=1,   # Go again with the fitting
                                 csv.output=TRUE, toppredlock = TRUE, cooling = 1)    # Cooling of one means we don't change the rate of exploration
toc()

# 24.5 hrs for 2000 iterations

## If another round is needed, increment round +1 and rerun the section above.
## Once ecology fit has stabilised move onto fishing fit

## Even with no fishing the guilds go to extinction. 
## For round 5 I multiplied the CZ gross production by 10 after going back to Lynn and Kelly and increased the PB ratio to 30.
## and I released top predators as their P/B ratios were very different from the targets.
## round 6 I increased the SD of net primary production by 10x to see if we can get some bottom up increases in plankton.
## round 7 I ... cached and restarted
## round 1 I added chlorophyll into the annual target data, and started from the Senegal model instead of Celtic Sea.
## round 2 just went again as it took a long time to get off 0.
## round 3 just keep going, similar issues with low biomasses beyond omnivorous zooplankton.
## round 4, update package so it actually fits to Chl?

#### Initial fishing fit ####

## Reactivate fishing targets

annual_targets <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv")) %>% 
  mutate(Use1_0 = if_else(Description %in% fishing_targets & is.finite(Annual_measure), 1, Use1_0))

write.csv(annual_targets, glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Target/annual_observed_{toupper(implementation)}_2010-2019.csv"), row.names = FALSE)

## Initial guess at Harvest Ratio Scaling factors on the basis of how far away from the landings we are already

HR_check <- e2e_run(model,nyears = 50)                                          # Check the model runs

## Landings from annual target data / the landings from HR_check for the first mults.

Sim_landings <- HR_check[["total.annual.catch"]][["inshore_annual_group_land_disc"]][50,] +
  HR_check[["total.annual.catch"]][["inshore_annual_group_land_disc"]][50,]

e2e_plot_ts(model, HR_check)

e2e_compare_obs(selection = "ANNUAL", model = model, results = HR_check)

#annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Use1_0"]

Mults <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/harvest_ratio_multiplier.csv")) %>% 
  mutate(Harvest_ratio_multiplier = case_when(
Group == "Planktivorous_fish" & 
  annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Annual_measure"] / Sim_landings$PFland,
Group == "Demersal_fish" & 
  annual_targets[which(annual_targets$Name == "Obs_Dland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Dland_livewt"), "Annual_measure"] / (Sim_landings$DFQland + Sim_landings$DFNQland),
Group == "Migratory_fish" &
  annual_targets[which(annual_targets$Name == "Obs_Mland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Mland_livewt"), "Annual_measure"] / Sim_landings$MFland,
Group == "Benthos_susp-dep" &
  annual_targets[which(annual_targets$Name == "Obs_Bsland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Bsland_livewt"), "Annual_measure"] / Sim_landings$SBland,
Group == "Benthos_carn-scav" &
  annual_targets[which(annual_targets$Name == "Obs_Bcland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Bcland_livewt"), "Annual_measure"] / Sim_landings$CBland,
Group == "Zooplankton_carn" &
  annual_targets[which(annual_targets$Name == "Obs_Zcland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Zcland_livewt"), "Annual_measure"] / Sim_landings$CZland,
# # Group == "Birds" &
# #   annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Annual_measure"] / Sim_landings$BDland,
# # Group == "Pinnipeds" & 
# #   annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Annual_measure"] / Sim_landings$SLland,
Group == "Cetaceans" &
  annual_targets[which(annual_targets$Name == "Obs_Ctland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Ctland_livewt"), "Annual_measure"] / Sim_landings$CTland,
Group == "Macrophytes" &
  annual_targets[which(annual_targets$Name == "Obs_Kland_livewt"), "Use1_0"] == 1 ~ annual_targets[which(annual_targets$Name == "Obs_Kland_livewt"), "Annual_measure"] / Sim_landings$KPland,
T ~ 1)) %>% 
  mutate(Harvest_ratio_multiplier = ifelse(is.finite(Harvest_ratio_multiplier), Harvest_ratio_multiplier, 1)) # Overwrite any infinities caused by landings of 0 in the simulations

# It looks like overfishing has driven the guilds to extinction. If we are to the right of the yield curve the above guesses
# for multipliers wont work (low landings asks for stronger fihsing, but that just drives overexploitation further).
# Instead I am specifying some very small multipliers to relieve the pressure and allow the fit to latch onto something.
# My first 2000 iterations with the above code returned likelihoods which were all 0.

Mults <- mutate(Mults, Harvest_ratio_multiplier = 0)

write.csv(Mults, str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/Param/harvest_ratio_multiplier.csv"), row.names = FALSE)

## Even with no fishing the guilds go to extinction. I'm going to try and refit the ecology with HRs set to 0.

## Launch fitting for HR scaling factors

## Progressed to fitting HRs on round 4
round <- 4                                                                     # Fitting round, to pull in new files and update name

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
                  model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}")) # reload model to update ident

fitting_HR <- e2e_optimize_hr(model, nyears=50, n_iter=2000, start_temperature=1,
                              csv.output=TRUE)

# Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")) 
# 
# Setup_file[8,1] <- stringr::str_glue("fitted_preference_matrix-2010-2015-CNRM-ssp370-fitting-{round-1}.csv") # Pull in last set of accepted parameters
# Setup_file[9,1] <- stringr::str_glue("fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
# Setup_file[10,1] <- stringr::str_glue("fitted_microbiology_others-2010-2015-CNRM-ssp370-fitting-{round-1}.csv")
# 
# write.csv(Setup_file,
#           file = stringr::str_glue("./StrathE2E/{implementation}/2010-2015-CNRM-ssp370/MODEL_SETUP.csv"),
#           row.names = F)


#### Now refit ecology with a vaguly realistic fishing fleet ####


fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=500, start_temperature=1,
                                 csv.output=TRUE)