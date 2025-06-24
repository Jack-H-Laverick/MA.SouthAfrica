library(terra)
library(tidyterra)
library(sf)
library(glue)
library(docstring)
library(tidyterra)
library(exactextractr)

extract_habitat_data <- function(raster, habitats, fun, name) {
    #' Use exactextractr::exact_extract() function to extract raster data from each zone and rename resulting column.
    extracted <- raster %>%
        exact_extract(habitats, fun = fun, append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        rename(!!name := fun)

    return(extracted)
}

format_sanbi_raster <- function(raster_fn, habitats) {
    #' Format a SANBI 'cumulative stress index' raster into usable values that contain just positive effort.
    raster <- rast(raster_fn)
    raster <- crop(raster, habitats)
    raster <- as.numeric(raster)
    raster <- subst(raster, -200:0, NA) # Replace 0 values with NA because there is no fishing in that area.

    return(raster)
}

sanbi_proportion_effort <- function(raster, habitats, fun, name) {
    #' Extract the effort data for each zone from a SANBI raster object and calculate the proportional effort for each zone.
    extracted <- extract_habitat_data(raster, habitats, fun, name)
    extracted[, name] <- ifelse(is.na(extracted[, name]), 0, extracted[, name])
    extracted <- extracted %>% mutate("proportion_{name}" := .[, name] / sum(.[, name]))

    return(extracted)
}

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

create_variable_raster <- function(flag_data, x_var, target) {
    # Then for each fishing variable
    var_raster <- dplyr::select(flag_data, c("cell_ll_lon", "cell_ll_lat", "year", all_of(x_var))) %>% # Select it along with spatial and temporal variables
        split(f = .$year) %>% # Split by year so we get one raster per time step
        map(
            function(x, target, x_var) { # For each year

                raster <- target # Take the target grid
                # cells <- terra::cellFromXY(target, cbind(x[, c("cell_ll_lon", "cell_ll_lat")])) # Find which cells on the grid our observations fall on
                # raster[cells] <- x[, x_var] # Copy over the data
                raster <- rasterize(as.matrix(x[, c("cell_ll_lon", "cell_ll_lat")]), raster, values = x[, x_var])

                return(raster)
            }, # Return an updated raster
            target = target, x_var = x_var
        ) %>% # Specify the target raster
        rast() # And bind each year into a raster brick of one variable for one flag

    return(var_raster)
}

create_effort_raster <- function(dataframe, vars, target, target_flag, area) {
    tic()
    flag_data <- filter(dataframe, flag == target_flag) # Limit to one in turn
    vars <- names(select(flag_data, !c(cell_ll_lon, cell_ll_lat, flag, year)))
    for (var in vars) {
        if (sum(!is.na(flag_data[, var])) == 0) {
            vars <- vars[vars != var]
        }
    }

    print(glue::glue("Now processing {target_flag}")) # Keeping track

    flag_rasters <- map(vars, function(x) create_variable_raster(flag_data, x, target))
    flag_rasters <- lapply(flag_rasters, function(flag_raster) {
        time(flag_raster) <- as.numeric(names(flag_raster))

        return(flag_raster)
    })

    print(glue::glue("Now saving {target_flag}")) # Keeping track
    future_map2(flag_rasters, paste0(target_flag, "-", vars), ~ { # Then save to netcdf
        writeCDF(.x,
            filename = paste0("./Data/fleet_fishing_", .y, "_", area, ".nc"), overwrite = TRUE, # Building a name from flag and variable
            varname = .y, unit = "Hours", zname = "year",
            atts = c("x=Longitude", "y=Latitude")
        )
    }, .progress = TRUE)
    toc()
}
