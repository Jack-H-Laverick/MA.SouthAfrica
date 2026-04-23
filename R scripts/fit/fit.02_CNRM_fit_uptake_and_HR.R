library(StrathE2E2)
library(ggplot2)
library(tidyverse)

# perform manual changes to model uptake parameter and target data files
# These changes were originally performed by Michael Heath in a manual process
# following fitting-1_2025_10_09 round of ecology model fitting

# uptake rate file
uptake_rates <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-1_2025_10_09.csv")
uptake_rates[uptake_rates$consumer == "benthslar", "Numax"] <- 0.3
uptake_rates[uptake_rates$consumer == "bird", "Numax"] <- 0.8
uptake_rates[uptake_rates$consumer == "bird", "ddmort"] <- 0.035
write.csv(uptake_rates, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-1_2025_10_09 - editMH.csv", row.names = FALSE)

# target data file
target_data <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Target/annual_observed_SOUTH_AFRICA_MA_prod_only.csv")
target_data[c(8, 9, 26, 27, 30, 31, 43, 44), "Annual_measure"] <- c(56.13750, 1.7449, 0.319, 0.396, 0.46, 0.316, 1.696, 0.937) # Manual adjustments to benthos guild production target values

# turn on fishing target data again (following disablement in initial fitting)
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
target_data <- target_data %>% mutate(Use1_0 = if_else(Description %in% fishing_targets, 1, Use1_0))
write.csv(target_data, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Target/annual_observed_SOUTH_AFRICA_MA_prod_landings_revMH.csv", row.names = FALSE)

setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[9, 1] <- "fitted_uptake_mort_rates-2010-2015-CNRM-ssp370-fitting-1_2025_10_09 - editMH.csv"
setup_file[23, 1] <- "annual_observed_SOUTH_AFRICA_MA_prod_landings_refMH.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)


# This round of fitting is used to fit the ecology model with primarily uptake parameters:
cv_eco <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/control/optimize_ecology.csv")
cv_eco[str_detect(cv_eco[, 2], "maximum_uptake"), "Value"] <- 0.005 # Set high CV control for max uptake params
cv_eco[!str_detect(cv_eco[, 2], "maximum_uptake"), "Value"] <- 0.0001 # Set low CV control for other param groups
write.csv(cv_eco, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/control/optimize_ecology.csv", row.names = FALSE)

model <- e2e_read(
    model.name = "South_Africa_MA",
    model.variant = "2010-2015-CNRM-ssp370",
    models.path = "./StrathE2E/",
    results.path = "StrathE2E/Results/", # edit for your own results folder
    model.ident = "2010-2015-CNRM-ssp370-fitting-uptake"
)
fitting_data <- e2e_optimize_eco(model,
    nyears = 50, n_iter = 900, # Go again with the fitting
    csv.output = TRUE, toppredlock = TRUE
)

# Fitting fishing HR parameters
# Default fitting parameter sets are "fitting_10_09 (and - editMH)"
setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[8, 1] <- gsub(setup_file[8, 1], pattern = "2010-2015-CNRM-ssp370-fitting-1_2025_10_09.*.csv", replacement = "fitting-uptake.csv")
setup_file[9, 1] <- gsub(setup_file[9, 1], pattern = "2010-2015-CNRM-ssp370-fitting-1_2025_10_09.*.csv", replacement = "fitting-uptake.csv")
setup_file[10, 1] <- gsub(setup_file[10, 1], pattern = "2010-2015-CNRM-ssp370-fitting-1_2025_10_09.*.csv", replacement = "fitting-uptake.csv")
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

model <- e2e_read(
    model.name = "South_Africa_MA",
    model.variant = "2010-2015-CNRM-ssp370",
    models.path = "./StrathE2E/",
    results.path = "StrathE2E/Results/", # edit for your own results folder
    model.ident = "fitting-hr"
)
results <- e2e_run(model, nyear = 30, csv.output = FALSE)

e2e_plot_ts(model, results)
e2e_compare_obs(selection = "ANNUAL", model, results = results)
results$final.year.outputs$opt_results[, c(1, 3, 4, 5, 6)]
results$final.year.outputs$annual_obj

fitting_data <- e2e_optimize_hr(model,
    nyears = 50, n_iter = 2000, # Go again with the fitting
    csv.output = TRUE
)


# Exploring fitted yield curves
setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[18, 1] <- "harvest_ratio_multiplier-fitting-hr.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

model <- e2e_read(
    model.name = "South_Africa_MA",
    model.variant = "2010-2015-CNRM-ssp370",
    models.path = "./StrathE2E/"
)
results <- e2e_run(model_n, nyear = 30, csv.output = FALSE)

e2e_plot_ts(model_n, results)
e2e_compare_obs(selection = "ANNUAL", model_n, results = results)
results$final.year.outputs$opt_results[, c(1, 3, 4, 5, 6)]
results$final.year.outputs$annual_obj

dem_ycurve <- e2e_run_ycurve(model, selection = "DEMERSAL", HRvector = seq(0,10))
e2e_plot_ycurve(model, selection="DEMERSAL", results=dem_ycurve)

plank_ycurve <- e2e_run_ycurve(model, selection = "PLANKTIV", HRvector = seq(0,50, 5))
e2e_plot_ycurve(model, selection="PLANKTIV", results=plank_ycurve)


baseline_numaxpfish <- 0.0836
baseline_ddpfish <- 2.09e-05
baseline_numaxbird <- 0.8
baseline_numaxseal <- 0.4433482
baseline_numaxceta <- 0.4666212
decrease <- 0.9
increase <- 0.7

model_n <- model
model_n$data$fitted.parameters$u_fishp <- baseline_numaxpfish - (baseline_numaxpfish * decrease)
model_n$data$fitted.parameters$xxpfish <- baseline_ddpfish - (baseline_ddpfish * decrease)

model_n$data$fitted.parameters$u_bird <- baseline_numaxbird + (baseline_numaxbird * increase)
model_n$data$fitted.parameters$u_seal <- baseline_numaxseal + (baseline_numaxseal * increase)
# model_n$data$fitted.parameters$u_ceta <- baseline_numaxceta + (baseline_numaxceta * increase)

plank_ycurve <- e2e_run_ycurve(model_n, selection = "PLANKTIV", HRvector = seq(0,50, 5))
e2e_plot_ycurve(model_n, selection="PLANKTIV", results=plank_ycurve)


# Should go back to the model with 0.59 likelihood: with fitting files named "fitting-uptake" and hr file named "fiting-hr".
# Then I can shuffle around seal, bird and cetacean uptakes if desired, or refit whole ecology parameters.
