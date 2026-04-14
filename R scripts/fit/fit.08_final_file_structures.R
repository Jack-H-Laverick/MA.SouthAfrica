library(tidyverse)

setwd("../downloads/public/resources/StrathE2E2/models/4.0.1/")
monte_carlo_file <- read.csv("./North_Sea/source_files/North_Sea/1970-1999/Param/control/monte_carlo.csv")

control_variants <- list.dirs(".", full.names = TRUE)
control_variants <- control_variants[str_detect(control_variants, "control")]

for (vdir in control_variants) {
  write.csv(monte_carlo_file, file.path(vdir, "monte_carlo.csv"), row.names=FALSE, quote=FALSE)
}

base_variant <- variant <- "2010-2015-CNRM-ssp126"
base_dir <- str_glue("./South_Africa_MA/source_files/South_Africa_MA/{variant}/")
fishing_fleet_fn <- list.files(base_dir, full.names = TRUE, pattern = "fishing_fleet", recursive=TRUE)
ffleet <- read.csv(fishing_fleet_fn)

hrmult_fn <- list.files(base_dir, full.names = TRUE, pattern = "harvest_ratio_multiplier-CNRM", recursive=TRUE)
hrmult <- read.csv(hrmult_fn)

new_ffleet <- ffleet
new_ffleet[12:21, ]$Value <- ffleet[12:21, ]$Value * hrmult$Harvest_ratio_multiplier

sa_fishing_fleet_fn <- list.files("./South_Africa_MA/source_files/South_Africa_MA/", full.names = TRUE, recursive=TRUE, pattern = "fishing_fleet")
sapply(sa_fishing_fleet_fn, function(x) write.csv(new_ffleet, x, row.names=FALSE, quote=FALSE))

sa_hrmult_fn <- list.files("./South_Africa_MA/source_files/South_Africa_MA/", full.names = TRUE, recursive=TRUE, pattern = "harvest_ratio_multiplier-CNRM")
file.remove(sa_hrmult_fn)

sa_model_setup_fn <- list.files("./South_Africa_MA/source_files/South_Africa_MA/", full.names = TRUE, recursive=TRUE, pattern = "MODEL_SETUP")
sapply(sa_model_setup_fn, function(x) {
  model_setup <- read.csv(x)
  model_setup[model_setup$Description == "Harvest ratio multipliers", "Filename"] <- "harvest_ratio_multiplier.csv"
  write.csv(model_setup, x, row.names=FALSE)
})

# Renaming south africa files.
south_africa_dirs <- list.dirs(".", recursive = TRUE, full.names = TRUE)
south_africa_dirs <- south_africa_dirs[str_detect(south_africa_dirs, "South_Africa_MA")]
south_africa_dirs_new <- gsub(south_africa_dirs, pattern = "South_Africa_MA", replacement = "S_Benguela_MA")

file.rename(from = south_africa_dirs, to = south_africa_dirs_new, recursive = TRUE)

south_africa_files <- list.files(".", recursive = TRUE, full.names = TRUE, pattern = "South_Africa_MA", ignore.case = TRUE)
south_africa_files_new <- gsub(south_africa_files, pattern = "South_Africa_MA", replacement = "S_Benguela_MA")
south_africa_files_new <- gsub(south_africa_files_new, pattern = "SOUTH_AFRICA_MA", replacement = "S_BENGUELA_MA")

file.rename(from = south_africa_files, to = south_africa_files_new)

model_setup_files <- list.files("./S_Benguela_MA/source_files/S_Benguela_MA/", full.names=TRUE, pattern = "MODEL_SETUP", recursive=TRUE)
for (setup_file in model_setup_files) {
  sf <- read.csv(setup_file)
  new_filenames <- sf$Filename
  new_filenames <- gsub(new_filenames, pattern = "South_Africa_MA", replacement = "S_BENGUELA_MA", ignore.case = TRUE)
  sf$Filename <- new_filenames
  write.csv(sf, setup_file, row.names = FALSE, quote = FALSE)
}
