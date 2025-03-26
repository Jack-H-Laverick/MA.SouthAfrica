# Convert NGU classess to 8 StrathE2E habitat types

# Sources
# Domain - Inshore/Offshore domain defined by bathymetry in ./bathymetry/bathymetry.01 DEFINE DOMAIN-BIG.R
#
# Sediment habitat classifications - `Birch et al. (1986): Texture and composition of surficial sediments`
# - this map was originally in a pdf form. The pdf was converted to an image and the image was
#   manually georeferenced in QGIS (using thin-line-splice algorithm). Colours were adjusted in
#   GIMP photo processing software, and polygons were created from the raster in QGIS.
#
# Rock habitat classification - SANBI Marine Ecosystem Map (2018).
# - https://bgis.sanbi.org/SpatialDataset/Detail/2681

#### Set up ####

rm(list = ls())

packages <- c("MiMeMo.tools", "stars") # List packages
lapply(packages, library, character.only = TRUE) # Load packages
source("./R scripts/@_Region file.R")
library(ggpattern)

domains <- readRDS("./Objects/Domains.rds") %>% # Load SF polygons of the MiMeMo model domains
    st_transform(crs = 9822) # Moved to CRS EPSG:9822 as it is equal-area to allow for accurate area calculations

sanbi_ecosystem_map <- st_read("./Data/spatial/SANBI-MarineEcosystemMap2018/MarineEcosystemMap2018_beta.shp")
sanbi_ecosystem_map <- st_transform(sanbi_ecosystem_map, crs = 9822)

rocky_broad_categories <- c(
    "Rocky and mixed shore",
    "Deep rocky shelf",
    "Shallow rocky shelf"
)
sanbi_broad <- sanbi_ecosystem_map[sanbi_ecosystem_map$BroadEcosy %in% rocky_broad_categories, ]

sanbi_substratum <- filter(
    sanbi_ecosystem_map,
    str_detect(sanbi_ecosystem_map$Substratum, "Rocky")
)

ggplot() +
    geom_sf(data = domains) +
    geom_sf(data = st_intersection(sanbi_broad, domains), aes(fill = BroadEcosy), alpha = 0.3)

ggplot() +
    geom_sf(data = domains) +
    geom_sf(data = st_intersection(sanbi_substratum, domains), aes(fill = Substratum), alpha = 0.3)


sediment_polygons <- st_read("./Data/spatial/SA_sediment_features.gpkg") # Import full sediment grid
sediment_polygons <- st_transform(sediment_polygons, crs = 9822)
sediment_labels <- c(
    "sandy_gravel" = 1,
    "sand" = 2,
    "muddy_sand" = 3,
    "sandy_mud" = 4,
    "mud" = 5,
    "gravelly_mud" = 6,
    "gravel_mud-sand-gravel" = 7
)
match_sediment_name <- function(x) names(which(sediment_labels == x))
sediment_polygons$surficial_sediment_class <- sapply(
    sediment_polygons$surficial_sediment,
    match_sediment_name
)

mud_labels <- c("sandy_mud", "mud", "gravelly_mud")
sand_labels <- c("muddy_sand", "sand")
# Note gravel_mud-sand-gravel is a single sediment label as the colours of 'grave' and 'mud-sand-gravel'
# were not distinguishable on the Birch et al. 1986 source map.
gravel_labels <- c("sandy_gravel", "gravel_mud-sand-gravel")

sediment_polygons$coarse_class <- ""
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% mud_labels, "mud", sediment_polygons$coarse_class)
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% sand_labels, "sand", sediment_polygons$coarse_class)
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% gravel_labels, "gravel", sediment_polygons$coarse_class)

ggplot() +
    geom_sf(data = domains, color = alpha("black", 0.1)) +
    geom_sf(data = st_intersection(sediment_polygons, domains), aes(fill = coarse_class), alpha = 0.3) +
    geom_sf(data = st_intersection(sanbi_substratum, domains), aes(color = Substratum))

# translate <- read.csv("./Data/Sediment habitats.csv") %>% # Import sediment aggregations
#     mutate(Sed_class = as.factor(SEDKORNSTR)) %>%
#     select(Sed_class, Habitat) # Drop excess columns

# #### Define geographic extent of each habitat type ####
#
# habitats <- left_join(sediment, translate) %>% # Attach habitat labels to predicted NGU classes
#     mutate(
#         Sed_class = as.factor(Sed_class), # Convert to factors
#         Habitat = as.factor(Habitat)
#     ) %>%
#     sfc_as_cols() # Get coordinates from sf formatting to define a raster
#
# numeric_habitats <- mutate(habitats, Habitat = as.numeric(Habitat)) # Convert factor to numeric as st_rasterize expects numbers
#
# polygons <- st_rasterize(numeric_habitats["Habitat"], # Rasterize habiat labels
#     nx = length(unique(habitats$x)), # At the resolution of the original data
#     ny = length(unique(habitats$y))
# ) %>%
#     st_as_sf(aspoints = FALSE, merge = TRUE) %>% # Merge pixels into contiguous polygons
#     mutate(Habitat = factor(Habitat, labels = levels(habitats$Habitat))) %>% # Reinstate labels for factor
#     group_by(Habitat) %>%
#     summarise(Habitat = Habitat[1]) # Combine polygons into a single row per habitat
#
# plot(polygons)
#
# polygons <- st_intersection(
#     st_make_valid(st_transform(polygons, crs = crs)), # Split sediment polygons along model zones
#     st_transform(domains, crs = crs)
# ) %>%
#     select(-c(Elevation, area)) %>% # Drop excess data
#     st_transform(crs = 4326) # Switch back to mercator

saveRDS(polygons, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

proportions <- st_intersection(sediment_polygons, domains) %>%
    mutate(Cover = as.numeric(st_area(.))) %>% # Measure the area of each habitat type
    st_drop_geometry() %>% # Drop SF formatting
    mutate(Cover = Cover / sum(Cover)) %>% # Calculate the proportion of the model zone in each sediment polygon
    rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
    geom_col(aes(x = Shore, y = Cover * 100, fill = Bottom), position = "Dodge") +
    theme_minimal() +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top"
    ) +
    viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
    labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")
