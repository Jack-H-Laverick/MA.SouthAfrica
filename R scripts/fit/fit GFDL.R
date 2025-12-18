library(StrathE2E2)
library(ggplot2)
library(tidyverse)

# setup_file <-read.csv("./StrathE2E/South_Africa_MA/2010-2015-GFDL-ssp370/MODEL_SETUP.csv")
# setup_file[4, 1] <- "initial_values-final_2010-2015-CNRM-ssp370.csv"
# setup_file[8, 1] <- "fitted_preference_matrix-CNRM_final_2025_10_31.csv"
# setup_file[9, 1] <- "fitted_uptake_mort_rates-CNRM_final_2025_10_31.csv"
# setup_file[10, 1] <- "fitted_microbiology_others-CNRM_final_2025_10_31.csv"
# setup_file[11, 1] <- "fishing_fleet_SOUTH_AFRICA_MA-CNRM_final_2025_10_31.csv"
# setup_file[18, 1] <- "harvest_ratio_multiplier-CNRM_final_2025_10_31.csv"
# setup_file[23, 1] <- "annual_observed_SOUTH_AFRICA_MA-final_2025_10_31.csv"
# write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-GFDL-ssp370/MODEL_SETUP.csv", row.names = FALSE)

# Load starting model and check results
model <- e2e_read(
  model.name = "South_Africa_MA",
  model.variant = "2010-2015-GFDL-ssp370",
  models.path = "./StrathE2E/",
  results.path = "StrathE2E/Results/", # edit for your own results folder
  model.ident = "eco-fitting_2025_11_11"
)
results <- e2e_run(model, nyear = 50, csv.output = FALSE)

e2e_plot_ts(model, results)
e2e_compare_obs(selection = "ANNUAL", model, results = results)
results$final.year.outputs$opt_results[, c(1, 3, 4, 5, 6)]
results$final.year.outputs$annual_obj

fitting_data <- e2e_optimize_eco(model,
                                 nyears = 50, n_iter = 2000, # Go again with the fitting
                                 csv.output = TRUE, toppredlock = TRUE
)

setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-GFDL-ssp370/MODEL_SETUP.csv")
setup_file[8, 1] <- "fitted_preference_matrix-eco-fitting_2025_11_11.csv"
setup_file[9, 1] <- "fitted_uptake_mort_rates-eco-fitting_2025_11_11.csv"
setup_file[10, 1] <- "fitted_microbiology_others-eco-fitting_2025_11_11.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-GFDL-ssp370/MODEL_SETUP.csv", row.names = FALSE)

model <- e2e_read(
  model.name = "South_Africa_MA",
  model.variant = "2010-2015-GFDL-ssp370",
  models.path = "./StrathE2E/",
  results.path = "StrathE2E/Results/", # edit for your own results folder
  model.ident = ""
)
# model$data$fitted.parameters$u_fishm <- model$data$fitted.parameters$u_fishm * 2.2 # Migratory fish extinction, need to increase uptake rates initially for fitting
results <- e2e_run(model, nyear = 50, csv.output = FALSE)

e2e_plot_ts(model, results)
e2e_compare_obs(selection = "ANNUAL", model, results = results)
pf_ycurve <- e2e_run_ycurve(model, selection = "PLANKTIV")
e2e_plot_ycurve(model, results=pf_ycurve, selection = "PLANKTIV")
df_ycurve <- e2e_run_ycurve(model, selection = "DEMERSAL")
e2e_plot_ycurve(model, results=pf_ycurve, selection="DEMERSAL")
