library(fs)
library(tidyverse)
library(R.utils)

target_fitted_files <- c(
    "fitted_microbiology_others-eco-fitting_2025_11_11",
    "fitted_preference_matrix-eco-fitting_2025_11_11",
    "fitted_uptake_mort_rates-eco-fitting_2025_11_11"
)
final_file_names <- c(
    "fitted_microbiology_others-GFDL_final_2025_11_11",
    "fitted_preference_matrix-GFDL_final_2025_11_11",
    "fitted_uptake_mort_rates-GFDL_final_2025_11_11"
)

source_variant <- "2010-2015-GFDL-ssp370"
variants_to_change <- dir(path = "./StrathE2E/South_Africa_MA/", pattern = "GFDL-ssp")
variants_to_change <- variants_to_change[!str_detect(variants_to_change, "2010-2015-GFDL-ssp370")]

# Apply the correct name to files in the base source variant CNRM-ssp370
for (i in seq_len(length(target_fitted_files))) {
    old_file <- target_fitted_files[i]
    old_file <- list.files(path = "./StrathE2E/South_Africa_MA/2010-2015-GFDL-ssp370", pattern = old_file, recursive = TRUE, full.names = TRUE)

    new_file <- gsub(pattern = target_fitted_files[i], replacement = final_file_names[i], x = old_file)
    file_copy(old_file, new_file, overwrite = TRUE)
}

setup_file <- read.csv("./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv")
setup_file[c(10, 8, 9), 1] <- paste0(final_file_names, ".csv")
write.csv(setup_file, "./StrathE2E/South_Africa_MA/2010-2015-CNRM-ssp370/MODEL_SETUP.csv", row.names = FALSE)

for (variant in variants_to_change) {
    old_folder <- str_glue("./StrathE2E/South_Africa_MA/{source_variant}/")
    new_folder <- str_glue("./StrathE2E/South_Africa_MA/{variant}/")

    # Delete all existing contents in the destination folder
    file.remove(list.files(new_folder, full.names = TRUE, recursive = TRUE, no.. = TRUE))

    # Copy contents of old_folder into new_folder (not nesting old_folder itsel
    file.copy(list.files(old_folder, full.names = TRUE),
        new_folder,
        recursive = TRUE
    )

    file.remove(list.files(new_folder, full.names = TRUE, recursive = TRUE, no.. = TRUE, pattern = "physics_SOUTH_AFRICA_MA"))
    file.copy(
        str_glue("./StrathE2E/South_Africa_MA/2010-2019/Driving/physics_SOUTH_AFRICA_MA_{variant}.csv"),
        str_glue("./StrathE2E/South_Africa_MA/{variant}/Driving/physics_SOUTH_AFRICA_MA_{variant}.csv")
    )
    file.remove(list.files(new_folder, full.names = TRUE, recursive = TRUE, no.. = TRUE, pattern = "chemistry_SOUTH_AFRICA_MA"))
    file.copy(
        str_glue("./StrathE2E/South_Africa_MA/2010-2019/Driving/chemistry_SOUTH_AFRICA_MA_{variant}.csv"),
        str_glue("./StrathE2E/South_Africa_MA/{variant}/Driving/chemistry_SOUTH_AFRICA_MA_{variant}.csv")
    )

    setup_file <- read.csv(str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"))
    setup_file[c(10, 8, 9), 1] <- paste0(final_file_names, ".csv")
    setup_file[2, 1] <- str_glue("physics_SOUTH_AFRICA_MA_{variant}.csv")
    setup_file[3, 1] <- str_glue("chemistry_SOUTH_AFRICA_MA_{variant}.csv")
    write.csv(setup_file, str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"), row.names = FALSE)

    # Run the model for 100 years and extract initial conditions
    model_v <- e2e_read(
        model.name = "South_Africa_MA",
        model.variant = variant,
        model.ident = str_glue("final_{variant}"),
        models.path = "./StrathE2E/"
    )
    results <- e2e_run(model_v, nyears = 100, csv.output = FALSE)
    e2e_extract_start(model_v, results)

    setup_file <- read.csv(str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"))
    setup_file[4, 1] <- str_glue("initial_values-final_{variant}.csv")
    write.csv(setup_file, str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"), row.names = FALSE)
}

all_variants <- dir(path = "./StrathE2E/South_Africa_MA/", pattern = "ssp")
for (variant in all_variants) {
    print(str_glue("Running model for {variant}"))
    esm <- str_extract(variant, "[:upper:]{4}")

    setup_file <- read.csv(str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"))

    if (esm == "CNRM") {
        setup_file[c(8, 9, 10), 1] <- c(
            "fitted_preference_matrix-CNRM_final_2025_10_31.csv",
            "fitted_uptake_mort_rates-CNRM_final_2025_10_31.csv",
            "fitted_microbiology_others-CNRM_final_2025_10_31.csv"
        )
    } else if (esm == "GFDL") {
        setup_file[c(8, 9, 10), 1] <- c(
            "fitted_preference_matrix-GFDL_final_2025_11_11.csv",
            "fitted_uptake_mort_rates-GFDL_final_2025_11_11.csv",
            "fitted_microbiology_others-GFDL_final_2025_11_11.csv"
        )
    }
    write.csv(setup_file, str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"), row.names = FALSE)

    model_v <- e2e_read(
        model.name = "South_Africa_MA",
        model.variant = variant,
        model.ident = str_glue("final_{variant}"),
        models.path = "./StrathE2E/"
    )
    results <- e2e_run(model_v, nyears = 100, csv.output = FALSE)
    e2e_extract_start(model_v, results)

    setup_file <- read.csv(str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"))
    setup_file[4, 1] <- str_glue("initial_values-final_{variant}.csv")
    write.csv(setup_file, str_glue("./StrathE2E/South_Africa_MA/{variant}/MODEL_SETUP.csv"), row.names = FALSE)
}
