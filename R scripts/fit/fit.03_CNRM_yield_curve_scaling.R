# Define function to run yield curve uptake and density dependent mortality param scaling
yield_curve_scaling <- function(model, numax, ddmort, hrscale, selection="PLANKTIV", setup_files){
  if (selection == "PLANKTIV") {
    numax_param <- "u_fishp"
    ddmort_param <- "xxpfish"
    hrscale_param <- "PF_HR_scale"
    land_col <- "Plank.fish_landings_live_weight"
    biomass_col <- "Planktivorous_fish"
    hrmult_indx <- 1
  } else {
    numax_param <- "u_fishd"
    ddmort_param <- "xxdfish"
    hrscale_param <- "DF_HR_scale"
    land_col <- "Dem.fish_landings_live_weight"
    biomass_col <- "Demersal_fish"
    hrmult_indx <- 2
  }
  
  # hr_values <- c(0, 1, 5, 10, 15, 20)
  hr_values <- c(0, 1, 1.5, 2, 3, 4, 5)
  
  n_param_sets <- seq_len(nrow(unique(data.frame(data.frame(numax = numax, ddmort = ddmort, hrscale = hrscale)))))
  runs_df <- expand.grid.df(
    data.frame(numax = numax, ddmort = ddmort, hrscale = hrscale, param_set = n_param_sets), 
    data.frame(hrmult = hr_values)
  )
  runs_df$run <- 1:nrow(runs_df)
  
  result_set <- data.frame(numax = numax, ddmort = ddmort, hrscale = hrscale, param_set = n_param_sets)
  result_set$catch_msy <- NA
  result_set$catch_current <- NA
  result_set$biomass_current <- NA
  result_set$ratio <- NA
  result_set$biomass_zero <- NA
  
  model_n <- model
  runs <- future_map(split(runs_df, runs_df$run),
                     function(x) {
                       x <- unlist(x)
                       assign("SETUPFILES", setup_files, envir = getNamespace("StrathE2E2")$pkg.env)
                       
                       model_n$data$fitted.parameters[numax_param] <- x["numax"]
                       model_n$data$fitted.parameters[ddmort_param] <- x["ddmort"]
                       model_n$data$fleet.model$HRscale_vector[hrscale_param] <- x["hrscale"]
                       model_n$data$fleet.model$HRscale_vector_multiplier[hrmult_indx] <- x["hrmult"]
                       
                       results <- e2e_run(model_n, nyears = 30, csv.output = FALSE)
                       Biomass <- results[["final.year.outputs"]][["mass_results_wholedomain"]] %>% 
                         filter(Description %in% c("Planktivorous_fish", "Demersal_fish", "Birds", "Pinnipeds", "Cetaceans")) %>% 
                         select(-Units) %>% 
                         pivot_wider(names_from = Description, values_from = Model_annual_mean)        # extract metrics of interest
                       
                       Landings <- filter(results[["final.year.outputs"]][["annual_flux_results_wholedomain"]],
                                          str_detect(Description, "landings_live_weight")) %>% 
                         filter(!str_detect(Description, "quota")) %>% 
                         select(-Units) %>% 
                         pivot_wider(names_from = Description, values_from = Model_annual_flux)
                       res <- cbind(Biomass, Landings)
                       res$run <- x["run"]
                       res$hrmult <- x["hrmult"]
                       return(res)
                     },
                     # model_n = model_n,
                     # setup_files = setup_files,
                     # numax_param = numax_param,
                     # ddmort_param = ddmort_param,
                     # .options = furrr_options(globals = FALSE),
                     .options = furrr_options(
                       globals  = list(model_n = model_n, setup_files = setup_files, numax_param = numax_param, ddmort_param = ddmort_param, hrscale_param = hrscale_param),
                       packages = c("StrathE2E2", "tidyverse")
                     ),
                     .progress = TRUE
  )
  ycurve <- data.frame(data.table::rbindlist(runs))
  
  for (ps in n_param_sets) {
    run_ids <- runs_df[runs_df$param_set == ps, "run"]
    ps_results <- ycurve[ycurve$run %in% run_ids, ]
    
    msy <- ps_results[, land_col] == max(ps_results[, land_col])
    if(sum(msy) > 1) {
      msy <- which(msy == TRUE)[1]
    }
    catch_msy <- ps_results[msy, land_col]
    
    catch_current <- ps_results[ps_results$hrmult == 1, land_col]
    ratio <- catch_current / catch_msy
    
    biomass_current <- ps_results[ps_results$hrmult == 1, biomass_col]
    biomass_hr_0 <- ps_results[ps_results$hrmult == 0, biomass_col]
    
    result_set[result_set$param_set == ps, ]$catch_msy <- catch_msy
    result_set[result_set$param_set == ps, ]$catch_current <- catch_current
    result_set[result_set$param_set == ps, ]$ratio <- ratio
    result_set[result_set$param_set == ps, ]$biomass_current <- biomass_current
    result_set[result_set$param_set == ps, ]$biomass_zero <- biomass_hr_0
    
  }
  
  return(result_set)
}

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

library(StrathE2E2)
library(furrr)
library(tidyverse)
library(ggplot2)

plan(multisession, workers = 20)

# Workflow code to asses impacts on model
# Setup model using latest fitting
setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[18, 1] <- "harvest_ratio_multiplier-fitting-hr.csv"
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

model <- e2e_read(
  model.name = "South_Africa_MA",
  model.variant = "2010-2015-CNRM-ssp370",
  models.path = "./StrathE2E/"
)
setup_files <- StrathE2E2:::pkg.env$SETUPFILES
results <- e2e_run(model, nyear = 30, csv.output = FALSE)
e2e_plot_ts(model, results)


# Define the numax and ddmort values to assess
baseline_numax_pfish <- model$data$fitted.parameters$u_fishp
baseline_ddmort_pfish <- model$data$fitted.parameters$xxpfish
baseline_hrscale_pfish <- model$data$fleet.model$HRscale_vector["PF_HR_scale"]
decreases <- c(0, seq(0.111, 0.999, 0.111))

# Run through a grid of all values within a range
param_changes <- c(seq(-0.99, -0.11, 0.33), 0, seq(0.5, 1, 0.5))
hr_scaling_changes <- c(1, 10)
numax_pfish <- baseline_numax_pfish + (baseline_numax_pfish * param_changes)
ddmort_pfish <- baseline_ddmort_pfish + (baseline_ddmort_pfish * param_changes)
hr_scaling <- baseline_hrscale_pfish * hr_scaling_changes

unique_params <- expand.grid(numax_pfish = numax_pfish, ddmort_pfish = ddmort_pfish, hr_scaling = hr_scaling)
grid_scaling <- yield_curve_scaling(model, unique_params$numax_pfish, unique_params$ddmort_pfish, unique_params$hr_scaling, setup_files = setup_files)
write.csv(grid_scaling, "./StrathE2E/Results/ycurve_grid_scaling.csv", row.names = FALSE)

# Filtering:
catch_msy <- grid_scaling[grid_scaling$catch_msy < 4.6 & grid_scaling$catch_msy > 2.6, ]
biomass <- grid_scaling[grid_scaling$biomass_zero < 90 & grid_scaling$biomass_zero > 70, ]

# Run for a few hr-vector values where numax and ddmort are suitable to try to reproduce a catch-msy and catch-hr-1 around the same value
numax_pfish <- unique(catch_msy$numax)
ddmort_pfish <- unique(catch_msy$ddmort)
hr_scaling_changes <- 10
hr_scaling <- baseline_hrscale_pfish * hr_scaling_changes

unique_params <- expand.grid(numax_pfish = numax_pfish, ddmort_pfish = ddmort_pfish, hr_scaling = hr_scaling)
start_time <- Sys.time()
constrained_scaling <- yield_curve_scaling(model, unique_params$numax_pfish, unique_params$ddmort_pfish, unique_params$hr_scaling, setup_files = setup_files)
end_time <- Sys.time()

### Identify numax and ddmort param combo that was closest to target landings at msy:
closest_catch_idx <- which.min(abs(constrained_scaling$catch_msy - 3.635080e+00))
closest_numax <- constrained_scaling[closest_catch_idx, ]$numax
closest_ddmort <- constrained_scaling[closest_catch_idx, ]$ddmort

# Do some more fine tune assessment of numax and ddmort values with a hrscale value of baseline*15
param_changes <- c(seq(-0.3, -0.1, 0.1), 0, seq(0.1, 0.3, 0.05))
numax_pfish <- closest_numax + (closest_numax * param_changes)
ddmort_pfish <- closest_ddmort + (closest_ddmort * param_changes)
hr_scaling <- baseline_hrscale_pfish * 15

unique_params <- expand.grid(numax_pfish = numax_pfish, ddmort_pfish = ddmort_pfish, hr_scaling = hr_scaling)
fine_tuned_scaling <- yield_curve_scaling(model, unique_params$numax_pfish, unique_params$ddmort_pfish, unique_params$hr_scaling, setup_files = setup_files)

closest_catch_idx <- which.min(abs(fine_tuned_scaling$catch_msy - 3.635080e+00))
fine_tuned_scaling[closest_catch_idx, ]$catch_msy
closest_numax <- fine_tuned_scaling[closest_catch_idx, ]$numax
closest_ddmort <- fine_tuned_scaling[closest_catch_idx, ]$ddmort

new_pfish_numax <- closest_numax
new_pfish_ddmort <- closest_ddmort
new_pfish_hrscale <- baseline_hrscale_pfish * 15

model_n <- model
model_n$data$fitted.parameters$u_fishp <- new_pfish_numax
model_n$data$fitted.parameters$xxpfish <- new_pfish_ddmort
model_n$data$fleet.model$HRscale_vector["PF_HR_scale"] <- baseline_hrscale_pfish * 15

results <- e2e_run(model_n)
e2e_plot_ts(model_n, results)

ycurve <- e2e_run_ycurve(model_n, selection = "DEMERSAL", HRvector=c(0,0.5,1,1.5,2,4,6))
e2e_plot_ycurve(model_n, results = ycurve, selection = "DEMERSAL")

## Start parameter scaling experiments for demersal fish:
baseline_numax_dfish <- model_n$data$fitted.parameters$u_fishd
baseline_ddmort_dfish <- model_n$data$fitted.parameters$xxdfish
baseline_hrscale_dfish <- model_n$data$fleet.model$HRscale_vector["DF_HR_scale"]

param_changes <- c(seq(-0.8, -0.1, 0.1), 0, seq(0.1, 0.8, 0.1))
hrscale_changes <- 2
numax_dfish <- baseline_numax_dfish + (baseline_numax_dfish * param_changes)
ddmort_dfish <- baseline_ddmort_dfish + (baseline_ddmort_dfish * param_changes)
hrscaling_dfish <- baseline_hrscale_dfish * hrscale_changes

unique_params <- expand.grid(numax_dfish = numax_dfish, ddmort_dfish = ddmort_dfish, hrscaling_dfish = hrscaling_dfish)
fine_dfish_scaling <- yield_curve_scaling(model_n, unique_params$numax_dfish, unique_params$ddmort_dfish, unique_params$hrscaling_dfish, setup_files = setup_files, selection = "DEMERSAL")

closest_catch_idx <- which.min(abs(fine_dfish_scaling$catch_msy - 8.200626e-01))
closest_numax <- fine_dfish_scaling[closest_catch_idx, ]$numax
closest_ddmort <- fine_dfish_scaling[closest_catch_idx, ]$ddmort
closest_hrscale <- fine_dfish_scaling[closest_catch_idx, ]$hrscale

new_dfish_numax <- closest_numax
new_dfish_ddmort <- closest_ddmort
new_dfish_hrscale <- closest_hrscale

model_n$data$fitted.parameters$u_fishd <- closest_numax
model_n$data$fitted.parameters$xxdfish <- closest_ddmort
model_n$data$fleet.model$HRscale_vector["DF_HR_scale"] <- closest_hrscale

baseline_numax_bird <- model_n$data$fitted.parameters$u_bird
baseline_numax_seal <- model_n$data$fitted.parameters$u_seal
baseline_numax_ceta <- model_n$data$fitted.parameters$u_ceta

model_n$data$fitted.parameters$u_bird <- baseline_numax_bird * 7
model_n$data$fitted.parameters$u_seal <- baseline_numax_seal * 5
model_n$data$fitted.parameters$u_ceta <- baseline_numax_ceta * 3

baseline_pfish_hrmult <- model_n$data$fleet.model$HRscale_vector_multiplier[1]
baseline_dfish_hrmult <- model_n$data$fleet.model$HRscale_vector_multiplier[2]
model_n$data$fleet.model$HRscale_vector_multiplier[1] <- 1
model_n$data$fleet.model$HRscale_vector_multiplier[2] <- 1

results <- e2e_run(model_n)
e2e_plot_ts(model_n, results)
e2e_compare_obs(model_n, results = results, selection = "ANNUAL")

ycurve <- e2e_run_ycurve(model_n, selection = "DEMERSAL", HRvector=c(0,0.5,1,1.5,2,4,6))
e2e_plot_ycurve(model_n, results = ycurve, selection = "DEMERSAL")

## Saving out final parameter values after fine tuning and adjusting target data. Then proceeding with final ecology fitting round.
uptake_param <- setup_file[9, 1]
uptake_param <- read.csv(file.path("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param", uptake_param))
uptake_param[uptake_param$consumer == "fishp", c("Numax", "ddmort")] <- c(new_pfish_numax, new_pfish_ddmort) 
uptake_param[uptake_param$consumer == "fishd", c("Numax", "ddmort")] <- c(new_dfish_numax, new_dfish_ddmort) 
uptake_param[uptake_param$consumer == "bird", "Numax"] <- baseline_numax_bird * 7
uptake_param[uptake_param$consumer == "seal", "Numax"] <- baseline_numax_seal * 5
uptake_param[uptake_param$consumer == "ceta", "Numax"] <- baseline_numax_ceta * 3
write.csv(uptake_param, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/fitted_uptake_mort_rates-updated_BG_2025_10_30.csv", row.names=FALSE)

fleet_param <- setup_file[11, 1]
fleet_param <- read.csv(file.path("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param",fleet_param))
fleet_param[stringr::str_detect(fleet_param$Description, "Pelagic"), "Value"] <- new_pfish_hrscale
fleet_param[stringr::str_detect(fleet_param$Description, "Demersal"), "Value"] <- new_dfish_hrscale
write.csv(fleet_param, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/fishing_fleet_SOUTH_AFRICA_MA-updated_BG_2025_10_30.csv", row.names=FALSE)

hr_mult <- setup_file[18, 1]
hr_mult <- read.csv(file.path("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param",hr_mult))
hr_mult[hr_mult$Group %in% c("Planktivorous_fish", "Demersal_fish"), ]$Harvest_ratio_multiplier <- 1
write.csv(hr_mult, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/harvest_ratio_multiplier-updated_BG_2025_10_30.csv", row.names = FALSE)

target_data <- setup_file[23, 1]
target_data <- read.csv(file.path("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Target",target_data))
target_data[target_data$Name %in% c("Obs_PFishP", "Obs_fishp_pb"), ]$Use1_0 <- 0
target_data[str_detect(target_data$Name, "land"), ]$SD_of_measure <- target_data[str_detect(target_data$Name, "land"), ]$Annual_measure * 0.1
write.csv(target_data, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Target/annual_observed_SOUTH_AFRICA_MA-updated_BG_2025_10_30.csv", row.names = FALSE)

# Choose numax and ddmort and HRscale that give catch-msy around target (HRmult 1)
# CHeck demersal fish to make sure that target catch is being caught and (F/Fmsy around 1)
# Look at top predator biomass and timeseries/obs
# Can do a final ecology fit with planktivorous fish turned off

# For 30/10: Check demersal fish ycurve scaling outputs and check ecosystem timeseries and yield curves
# Check top predators and do adjustments
# Perform final round of ecology fitting with pfish targets from EwE turned off
