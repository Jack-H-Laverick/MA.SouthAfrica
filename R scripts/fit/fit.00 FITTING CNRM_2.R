## Jack fitting a model

#### Set up ####

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

## Are the patterns in the drivers the same as the target data? If the patterns are way off they can't be used and need deactivating

# Satellite chlorophyll vs phytoplankton drivers


# nutrient concentrations vs drivers



## Launch ecology fitting process
#
# fitting_data <- e2e_optimize_eco(model,
#     nyears = 50, n_iter = 2000, start_temperature = 1,
#     csv.output = TRUE, toppredlock = TRUE, cooling = 1
# )

## Keep launching fitting processes until stabilised
## Note that round 3 fitted values caused an error, have returned to refit round 2.
## Make initial changes to CV parameter family values:
## pref 0.001 -> 0.005, uptakerates 0.005 -> 0.0001, halfsat coeff 0.005 -> 0.0001, microbial rates 0.001 -> 0.0001, others 0.001 -> 0.0001

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
    model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}")
) # reload model to update ident

# Inspect
#
# # model$data$fitted.parameters$u_phyt <- 30
# # model$data$fitted.parameters$h_phyt <- 20
# #model$data$fitted.parameters$u_omni <- 5
# model$data$fitted.parameters$u_carn <- 6
# model$data$fitted.parameters$u_fishp <- 10
# model$data$fitted.parameters$u_fishd <- 0.09
# model$data$fitted.parameters$u_bird <- 0.9
# model$data$fitted.parameters$u_seal <- 0.9
# model$data$fitted.parameters$u_ceta <- 0.9
# #model$data$fitted.parameters$u_fishm <- 0.04
# #model$data$fitted.parameters$u_benthc <- 1
#
# model$data$chemistry.drivers$so_nitrate <- model$data$chemistry.drivers$so_nitrate * 2
# model$data$chemistry.drivers$si_nitrate <- model$data$chemistry.drivers$si_nitrate * 2
# model$data$chemistry.drivers$so_ammonia <- model$data$chemistry.drivers$so_ammonia * 2
# model$data$chemistry.drivers$si_ammonia <- model$data$chemistry.drivers$si_ammonia * 2

results <- e2e_run(model, nyears = 50) # Check the model runs

e2e_plot_ts(model, results)

e2e_compare_obs(selection = "ANNUAL", model = model, results = results)

# fit again

# tic()
# #fitting_data <- e2e_optimize_eco(model,
# #    nyears = 50, n_iter = 2000, start_temperature = 1, # Go again with the fitting
# #    csv.output = TRUE, toppredlock = TRUE, cooling = 1
# #) # Cooling of one means we don't change the rate of exploration
# fitting_data <- e2e_optimize_eco(model,
#                                  nyears = 50, n_iter = 2000, # Go again with the fitting
#                                  csv.output = TRUE, toppredlock = TRUE
# ) # Cooling of one means we don't change the rate of exploration
# toc()

tic()
# fitting_data <- e2e_optimize_eco(model,
#    nyears = 50, n_iter = 2000, start_temperature = 1, # Go again with the fitting
#    csv.output = TRUE, toppredlock = TRUE, cooling = 1
# ) # Cooling of one means we don't change the rate of exploration
fitting_data <- e2e_optimize_eco(model,
    nyears = 50, n_iter = 900, # Go again with the fitting
    csv.output = TRUE, toppredlock = TRUE
) # Cooling of one means we don't change the rate of exploration
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

HR_check <- e2e_run(model, nyears = 50) # Check the model runs

## Landings from annual target data / the landings from HR_check for the first mults.

Sim_landings <- HR_check[["total.annual.catch"]][["inshore_annual_group_land_disc"]][50, ] +
    HR_check[["total.annual.catch"]][["inshore_annual_group_land_disc"]][50, ]

e2e_plot_ts(model, HR_check)

e2e_compare_obs(selection = "ANNUAL", model = model, results = HR_check)

# annual_targets[which(annual_targets$Name == "Obs_Pland_livewt"), "Use1_0"]

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
        T ~ 1
    )) %>%
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
round <- 4 # Fitting round, to pull in new files and update name

model <- e2e_read(implementation, str_glue("2010-2015-CNRM-ssp370"),
    models.path = "StrathE2E/", results.path = "StrathE2E/Results/",
    model.ident = stringr::str_glue("2010-2015-CNRM-ssp370-fitting-{round}")
) # reload model to update ident

fitting_HR <- e2e_optimize_hr(model,
    nyears = 50, n_iter = 2000, start_temperature = 1,
    csv.output = TRUE
)

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


fitting_data <- e2e_optimize_eco(model,
    nyears = 50, n_iter = 500, start_temperature = 1,
    csv.output = TRUE
)

library(ggplot2)
compare_fitted_parameters_eco <- function(model_param_folder, fitted_param_pattern, baseline_param_pattern, type, display = "difference") {
    fns <- list.files(model_param_folder, full.names = TRUE)
    fns <- fns[grep(type, fns)]
    fitted_fn <- fns[grep(fitted_param_pattern, fns)]
    baseline_fn <- fns[grep(baseline_param_pattern, fns)]

    fitted_data <- read.csv(fitted_fn)
    fitted_data$type <- "fitted"

    baseline_data <- read.csv(baseline_fn)
    baseline_data$type <- "baseline"

    if (type == "microbiology_others") {
        if (display == "difference") {
            diff <- fitted_data
            diff$baseline_fitted <- (fitted_data$Value / baseline_data$Value * 100) - 100
            plot_1 <- ggplot() +
                geom_col(data = diff, aes(x = as.factor(Description), y = baseline_fitted), position = "dodge") +
                coord_flip() +
                labs(y = "% change of baseline parameter")

            return(plot_1)
        }

        all_data <- rbind(fitted_data, baseline_data)

        plot_1 <- ggplot() +
            geom_col(data = all_data, aes(x = as.factor(Description), y = Value, fill = as.factor(type)), position = "dodge") +
            coord_flip()

        return(plot_1)
    }

    if (type %in% c("uptake_mort_rates", "preference_matrix")) {
        names(fitted_data)[1] <- "X"
        names(baseline_data)[1] <- "X"

        if (display == "difference") {
            diff <- (as.matrix(fitted_data[, 2:(ncol(fitted_data) - 1)]) / as.matrix(baseline_data[, 2:(ncol(baseline_data) - 1)]) * 100) - 100
            diff <- as.data.frame(diff)
            diff$X <- baseline_data$X

            diff <- reshape(
                diff,
                varying = names(diff)[names(diff) != "X"], # all columns except id vars
                v.names = "value", # name for the measurement values
                timevar = "variable", # name for the column that will hold the old colnames
                times = names(diff)[names(diff) != "X"], # use colnames as labels
                idvar = "X", # keep these fixed
                direction = "long"
            )

            plot_1 <- ggplot() +
                geom_tile(data = diff, aes(x = X, y = variable, fill = value)) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(fill = "% change of baseline parameter") +
                ggtitle(type) +
                scale_fill_viridis_c()


            return(plot_1)
        }

        fitted_data <- reshape(
            fitted_data,
            varying = names(fitted_data)[2:(ncol(fitted_data) - 1)], # all columns except id vars
            v.names = "value", # name for the measurement values
            timevar = "variable", # name for the column that will hold the old colnames
            times = names(fitted_data)[2:(ncol(fitted_data) - 1)], # use colnames as labels
            idvar = c("X", "type"), # keep these fixed
            direction = "long"
        )

        baseline_data <- reshape(
            baseline_data,
            varying = names(baseline_data)[2:(ncol(baseline_data) - 1)], # all columns except id vars
            v.names = "value", # name for the measurement values
            timevar = "variable", # name for the column that will hold the old colnames
            times = names(baseline_data)[2:(ncol(baseline_data) - 1)], # use colnames as labels
            idvar = c("X", "type"), # keep these fixed
            direction = "long"
        )

        all_data <- rbind(fitted_data, baseline_data)

        plot_1 <- ggplot() +
            geom_tile(data = all_data, aes(x = X, y = variable, fill = value)) +
            facet_wrap(~type) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(type)

        return(plot_1)
    }
}

# Plots to show which parameter values have changed the most between rounds
compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-1_2025_10_09", "SENEGAL", "uptake_mort_rates")
compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-1_2025_10_09", "SENEGAL", "microbiology_others")
compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-1_2025_10_09", "SENEGAL", "preference_matrix")

compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-2.csv", "fitting-1.csv", "uptake_mort_rates")
compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-2.csv", "fitting-1.csv", "microbiology_others")
compare_fitted_parameters_eco("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/Param/", "fitting-2.csv", "fitting-1.csv", "preference_matrix")


library(tidyverse)

# Plots to show change of parameters over iterations

trace <- map_df(c("1_2025_10_09"), ~ { # For each chunk of model fitting

    read.csv(str_glue("./StrathE2E/Results/South_Africa_MA/2010-2015-CNRM-ssp370/annealing_par_acceptedhistory-2010-2015-CNRM-ssp370-fitting-{.x}.csv"))
}) %>%
    rowid_to_column(var = "Iteration") %>%
    pivot_longer(-Iteration, names_to = "Param", values_to = "Value")







## trace plots



trace_plot <- ggplot(trace) +
    geom_path(aes(x = Iteration, y = Value)) +
    facet_wrap(vars(Param), scales = "free_y") +
    theme_classic()

ggsave("./Objects/fitting_parameter_progress.png", trace_plot, height = 50, width = 100, units = "cm")

start_iteration <- 1
end_iteration <- 100

bar_data <- trace[trace$Iteration %in% c(start_iteration, end_iteration), ] %>%
    pivot_wider(., id_cols = "Param", values_from = "Value", names_from = "Iteration")
bar_data <- bar_data[bar_data$Param != "annual_obj", ]
bar_data$percent_change <- ((bar_data[, paste(end_iteration)] - bar_data[, paste(start_iteration)]) / bar_data[, paste(start_iteration)])[, 1] * 100

bar_plot <- ggplot() +
    geom_col(data = bar_data, aes(x = Param, y = percent_change), position = "dodge") +
    coord_flip() +
    labs(y = "% change of baseline parameter value")

ggsave("./Objects/fitting_parameter_difference.png", bar_plot, height = 50, width = 25, units = "cm")
