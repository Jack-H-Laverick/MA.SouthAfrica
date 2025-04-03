# Script to combine all downloaded global fishing watch apparent fishing data into accumulated parquet files
# that are space and time efficient.
library(tidyverse)
library(arrow)
library(docstring)

combine_csv_parquet <- function(dir) {
    #' Combine multiple CSV files into a single parquet
    #'
    #' @description Combine multiple CSV files, that contain `cell_ll_lat` and `cell_ll_lon`
    #' coordinate columns.
    #'
    #' @param dir charachter. The base directory containing the csv files to combine and store the parquet file.
    #' @return Nothing. Outputs to a file named "dir/dir-end.parq"
    #' @examples
    #' combine_csv_parquet("./daily_fishing_data/")
    #' Outputs to file "./daily_fishing_data/daily_fishing_data.parq"

    filename <- last(str_split(dir, "\\/")[[1]])
    dir_files <- list.files(dir)

    dataframe <- vector(mode = "list", length = length(dir_files))
    for (file in seq_along(dir_files)) {
        csv <- paste0(dir, "/", dir_files[file])
        csv <- read.csv(csv) %>%
            filter(., -36 < cell_ll_lat & -26 > cell_ll_lat & 12.5 < cell_ll_lon & 37.5 > cell_ll_lat)
        dataframe[[file]] <- csv
    }
    dataframe <- data.table::rbindlist(dataframe)
    write_parquet(dataframe, paste0(dir, "/", filename, ".parq"))
}

combine_year_parquets <- function(dir, file_base, pattern) {
    #' Combine multiple parquet files into a single parquet
    #'
    #' @description Combine multiple parquet files that are stored in separate files into a single larger file.
    #'
    #' @param dir character. The base directory containing the subdirectories with files to combine.
    #' @param file_base character. Base to name output file.
    #' @param pattern character. Regex string to filtere target directories that contain parquet files.
    #' @return Nothing. Outputs to a file named "dir/file_base.parq"
    #' @examples
    #' combine_year_parquets(".fishing_data/", "daily_fishing")
    #' Outputs to file "./fishing_data/daily_fishing.parq"

    dirs <- dir(dir, pattern = pattern, full.names = TRUE)
    for (file in seq_along(dirs)) {
        file_end <- last(str_split(dirs[file], "\\/")[[1]])
        dirs[file] <- paste0(dirs[file], "/", file_end, ".parq")
    }
    dataframe <- vector(mode = "list", length = length(dirs))

    for (file in seq_along(dirs)) {
        dataframe[[file]] <- read_parquet(dirs[file])
    }
    dataframe <- data.table::rbindlist(dataframe)

    write_parquet(dataframe, paste0(dir, "/", file_base, ".parq"))
}

csv_dirs <- list.dirs("../../Spatial Data/fishing_effort_data/Global_fishing_watch/")[-1]
walk(csv_dirs, combine_csv_parquet)

file_types <- c("fleet-daily", "fleet-monthly", "mmsi-daily")
patterns <- c("[fleet][-][daily]", "[fleet][-][monthly]", "[mmsi][-][daily]")
combine_parqs <- function(file_type, pattern) combine_year_parquets(dir = "../../Spatial Data/fishing_effort_data/Global_fishing_watch/", file_base = file_type, pattern = pattern)
walk2(file_types, patterns, combine_parqs, .progress = TRUE)
