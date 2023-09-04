
# Convert Marine Ecosystem Map into StrathE2E habitats

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars")                           # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of model domain
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

MEM <- st_read("./Data/MEM18/") # Import South African map

#### Limit the map to the model domain ####

polygons <- st_intersection(st_make_valid(st_transform(MEM, crs = crs)), # Split sediment polygons along model zones
                            st_transform(domains, crs = crs)) %>% 
  select(-c(Elevation, area)) %>%                                # Drop excess data
  st_transform(crs = 4326) %>%                                   # Switch back to mercator
  mutate(Habitats = case_when(str_detect(Substratum, "Mud") ~ "Mud",
                              str_detect(Substratum, "Sand") ~ "Sand",
                              str_detect(Substratum, "Rock") ~ "Rock",
                              T ~ "unclassified"))

ggplot(polygons) +
  geom_sf(aes(fill = Substratum)) +
  geom_sf(data = domains, fill = NA, colour = "red")
  
polygons <- group_by(polygons, Shore, Habitats) %>% 
  summarise(Shore = Shore[[1]],
            Habitats = Habitats[[1]]) %>%  
  ungroup()
  
ggplot(polygons) +
  geom_sf(aes(fill = Habitats)) 

#saveRDS(polygons, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

proportions <- polygons %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitats)

#saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")

