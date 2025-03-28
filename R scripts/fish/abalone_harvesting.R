source("./R scripts/fish/fisheries_data_functions.R")

# Abalone harvesting areas
abalone <- st_read("../../Spatial Data/fishing_effort_data/Abalone_Harvesting_Areas/Abalone_Harvesting_Areas/Abalone_Harvesting_Areas.shp")
abalone <- select(abalone, !c(OBJECTID_1, Shape_Leng, Shape_Area))
# Ensure that all column names that contain information about 2 years are all separated by "_"
abalone_names <- names(abalone)
abalone_names <- sapply(abalone_names, rgx_separate_year_month)
names(abalone) <- abalone_names

# Separate the columns that contain information on per annum data vs seasonal data
per_annum_abalone <- select(abalone, !matches("\\d"))
# Plot per annum abalone data
dot_plot <- ggplot() +
    geom_point(
        data = per_annum_abalone,
        aes(x = kg_pa, y = hour_pa, color = Zone),
        size = 2.5
    ) +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(legend.position = "none")
map <- ggplot() +
    geom_sf(data = abalone, aes(fill = Zone)) +
    scale_fill_viridis_d() +
    theme_minimal()
grid.arrange(arrangeGrob(grobs = list(dot_plot, map)), nrow = 1)

# Reformat the seasonal abalone fisheries data
seasonal_abalone <- select(abalone, !c(kg_pa, hour_pa))
seasonal_abalone <- pivot_longer(
    seasonal_abalone,
    cols = matches("\\d"),
    names_pattern = "([a-zA-Z]+)_?(\\d{4}_\\d+)",
    names_to = c(".value", "season")
)
seasonal_abalone <- seasonal_abalone[seasonal_abalone$hrs < 8760, ] # Hours must be less than 8760/yr
seasonal_abalone_plotting <- pivot_longer(
    seasonal_abalone,
    cols = c(kg, hrs, cpue),
    names_to = "variable",
    values_to = "value"
)

# Plot seasonal abalone data
timeseries <- ggplot() +
    geom_line(
        data = seasonal_abalone_plotting,
        aes(x = season, y = value, color = Zone, group = interaction(Zone, variable))
    ) +
    scale_color_viridis_d() +
    facet_wrap(~variable, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")
map <- ggplot() +
    geom_sf(data = abalone, aes(fill = Zone)) +
    scale_fill_viridis_d() +
    theme_minimal()
grid.arrange(arrangeGrob(grobs = list(timeseries, map)), nrow = 1)
