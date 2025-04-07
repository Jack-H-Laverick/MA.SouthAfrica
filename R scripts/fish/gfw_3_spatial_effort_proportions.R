packages <- c("tidyverse", "exactextractr", "terra", "furrr", "data.table", "sf", "glue") # List handy packages
lapply(c(packages), library, character.only = TRUE)

plan(multisession)

habitats <- readRDS("./Objects/habitats.rds") # Load habitat polygons

#### Extracting a domain wide summary ####
polygon_extraction <- function(file, polygons) {
    flag <- file %>%
        str_split_i(pattern = "(?=[-])", i = 1) %>%
        str_split_i(pattern = "(?<=[_])", i = 3)
    variable <- file %>%
        str_split_i(pattern = "([-])", i = 2) %>% # Split to get last section of file name
        str_split_i(pattern = "([.])", i = 1)

    extracted_data <- rast(glue("./Data/{file}")) %>% # Import a brick of all years
        exact_extract(polygons, fun = "sum", append_cols = c("Habitat", "Shore")) %>% # Sum fishing hours within habitat types
        pivot_longer(
            cols = matches("\\d"),
            names_to = "year",
            values_to = "hours"
        ) %>%
        mutate(year = as.numeric(str_split_i(year, "(?<=[=])", i = 2))) %>%
        mutate(variable = glue("{flag}_{variable}")) %>%
        select(c(Habitat, Shore, year, variable, hours))

    return(extracted_data)
}

nc_files <- list.files("./Data/", pattern = ".nc")
extracted_data <- map(
    nc_files,
    function(x) polygon_extraction(x, habitats)
)
extracted_data <- rbindlist(extracted_data) %>%
    group_by(year) %>%
    mutate(proportion = hours / sum(hours)) %>%
    ungroup()

anim_plot <- ggplot() +
    geom_col(data = extracted_data, aes(x = Habitat, y = proportion, fill = variable)) +
    facet_wrap(~Shore) +
    transition_states(year) +
    labs(title = "Year: {closest_state}") +
    ylab("Proportion of domain-wide effort per year") +
    theme_minimal()
anim_save("./Objects/GFW_domain_effort_plot.gif", anim_plot)
