
## There are artifacts in the digitisation of the habitat maps which break SF, I have
## cleaned up the large offshore sand polygon below, to stop the errors.

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars", "raster")                 # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

polygons <- readRDS("./Data/Habitats.rds") %>%                   # Ben's Map
  st_make_valid()

#### Cleaning ####

ggplot(polygons) +                                               # Lots of whispy bits
  geom_sf(aes(fill = Habitat))

sf_use_s2(FALSE)

sand <- filter(polygons, Habitat == "sand", Shore == "Offshore") # Work on just the worst polygon 
plot(sand)

bbp = st_as_sf(st_as_sfc(st_bbox(sand)), crs=4326)               # Get an sf object of the bounding box

test <- st_difference(bbp, sand) %>%                             # cut the negative of the shape
  st_cast("POLYGON") %>%                                         # Access each sub-shape separately
  mutate(area = as.numeric(st_area(st_make_valid(.)))) %>%       # Calculate their size
  filter(area > 100000000) %>%                                   # Now remove all the tiny holes
  st_union()                                                     # And join all the shapes together again
  
plot(test)                                                       # We now only have the "real" holes in the habitat map
plot(sand)

final <- st_difference(bbp, test) %>%                            # Cut the negative out of the bounding box to get the shape back
  rename(geometry = "x") %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  mutate(Habitat = "sand", Shore = "Offshore")                   # Reinstate names

plot(final)

new_polygons <- filter(polygons, paste(Habitat, Shore) != "sand Offshore") %>% # Remove the old polygon
  bind_rows(final)                                               # and add in the cleaned one

ggplot(new_polygons) +
  geom_sf(aes(fill = Habitat), alpha = 0.5)                      # No obvious overlaps

saveRDS(new_polygons, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

proportions <- new_polygons %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")
