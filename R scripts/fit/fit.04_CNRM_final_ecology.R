library(StrathE2E2)
library(ggplot2)
library(tidyverse)

setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[9, 1] <- "fitted_uptake_mort_rates-updated_BG_2025_10_30.csv"
setup_file[11, 1] <- "fishing_fleet_SOUTH_AFRICA_MA-updated_BG_2025_10_30.csv"
setup_file[18, 1] <- "harvest_ratio_multiplier-updated_BG_2025_10_30.csv"
setup_file[23, 1] <- "annual_observed_SOUTH_AFRICA_MA-updated_BG_2025_10_30.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

# Load starting model and check results
model <- e2e_read(
  model.name = "South_Africa_MA",
  model.variant = "2010-2015-CNRM-ssp370",
  models.path = "./StrathE2E/",
  results.path = "StrathE2E/Results/", # edit for your own results folder
  model.ident = "eco-fitting_2025_10_30"
)
base_carn_benthos_hrscale <- model$data$fleet.model$HRscale_vector["CB_HR_Scale"]
model$data$fleet.model$HRscale_vector["CB_HR_Scale"] <- base_carn_benthos_hrscale * 150

results <- e2e_run(model, nyear = 30, csv.output = FALSE)

e2e_plot_ts(model, results)
e2e_compare_obs(selection = "ANNUAL", model, results = results)
results$final.year.outputs$opt_results[, c(1, 3, 4, 5, 6)]
results$final.year.outputs$annual_obj

fitting_data <- e2e_optimize_eco(model,
                                 nyears = 50, n_iter = 2000, # Go again with the fitting
                                 csv.output = TRUE, toppredlock = TRUE
)


setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[8, 1] <- "fitted_preference_matrix-eco-fitting_2025_10_30.csv"
setup_file[9, 1] <- "fitted_uptake_mort_rates-eco-fitting_2025_10_30.csv"
setup_file[10, 1] <- "fitted_microbiology_others-eco-fitting_2025_10_30.csv"
setup_file[4, 1] <- "initial_values-post-fit.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

model <- e2e_read(
  model.name = "South_Africa_MA",
  model.variant = "2010-2015-CNRM-ssp370",
  models.path = "./StrathE2E/",
  results.path = "StrathE2E/Results/", # edit for your own results folder
  model.ident = "eco-fitting_2025_10_31"
)

fitting_data <- e2e_optimize_eco(model,
                                 nyears = 60, n_iter = 2000, # Go again with the fitting
                                 csv.output = TRUE, toppredlock = TRUE
)

results <- e2e_run(model, nyears = 50, csv.output = FALSE)
e2e_plot_ts(model, results)
e2e_compare_obs(model = model, results = results, selection = "ANNUAL")

pycurve <- e2e_run_ycurve(model, selection = "PLANKTIV")
e2e_plot_ycurve(model = model, results = pycurve, selection = "PLANKTIV")
dycurve <- e2e_run_ycurve(model, selection = "DEMERSAL")
e2e_plot_ycurve(model = model, results = dycurve, selection = "DEMERSAL")

setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[8, 1] <- "fitted_preference_matrix-eco-fitting_2025_10_31.csv"
setup_file[9, 1] <- "fitted_uptake_mort_rates-eco-fitting_2025_10_31.csv"
setup_file[10, 1] <- "fitted_microbiology_others-eco-fitting_2025_10_31.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)