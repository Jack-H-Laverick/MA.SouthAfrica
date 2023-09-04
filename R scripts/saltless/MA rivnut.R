
library(tidyverse)
library(sf)

domains <- readRDS("./Objects/Domains.rds")
domains <- readRDS("../Objects/Domains.rds") %>% st_transform(4326)

crop <- st_bbox(domains)

#### Flows ####

meta <- st_layers("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_runoff/")

look <- st_read("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_runoff/", quiet = TRUE, 
                query = "SELECT * FROM Global_YearMonthly_River_flow_watermap22_coast WHERE lat BETWEEN 30 AND 60 AND lon BETWEEN 0 AND 30")

small <- filter(look, Date == as.Date("1901-01-01"))

ggplot(small) +
  geom_sf(aes(colour = Dis_m3_s.1))

#### Ammonia ####

meta <- st_layers("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_nutrient_discharges/")


look <- st_read("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_nutrient_discharges/", "TIN_GRQA_coast", quiet = TRUE, 
                query = str_glue("SELECT * FROM NH4N_GRQA_coast WHERE lat_wgs84 BETWEEN {crop[2]} AND {crop[4]} AND lon_wgs84 BETWEEN {crop[1]} AND {crop[3]}"))

small <- filter(look, obs_date >= as.Date("2000-01-01"))

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = small, aes(colour = obs_value))

ggplot(small) +
  geom_point(aes(x = obs_date, y = obs_value, colour = site_id))

#### Nitrate ####

look <- st_read("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_nutrient_discharges/", "NO3N_GRQA_coast", quiet = TRUE, 
                query = str_glue("SELECT * FROM NO3N_GRQA_coast WHERE lat_wgs84 BETWEEN {crop[2]} AND {crop[4]} AND lon_wgs84 BETWEEN {crop[1]} AND {crop[3]}"))

small <- filter(look, obs_date >= as.Date("2000-01-01"))

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = small, aes(colour = obs_value))

ggplot(small) +
  geom_point(aes(x = obs_date, y = obs_value, colour = site_id))

#### ToN ####

look <- st_read("../Shared data/MISSION_ATLANTIC_D3.2_River_data/River_nutrient_discharges/", "TON_GRQA_coast", quiet = TRUE)#, 
                query = str_glue("SELECT * FROM TON_GRQA_coast WHERE lat_wgs84 BETWEEN {crop[2]} AND {crop[4]} AND lon_wgs84 BETWEEN {crop[1]} AND {crop[3]}"))

small <- filter(look, obs_date >= as.Date("2000-01-01"))

ggplot() +
#  geom_sf(data = domains) +
  geom_sf(data = small, aes(colour = obs_value))

ggplot(small) +
  geom_point(aes(x = obs_date, y = obs_value, colour = site_id))
