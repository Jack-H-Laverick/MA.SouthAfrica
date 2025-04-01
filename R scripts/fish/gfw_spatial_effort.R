library(gfwr)
key <- gfw_auth()
sa_eez <- get_region_id(region_name = "SA", region_source = "EEZ", key = key)

sa_fishing_events <- get_event(
    "FISHING",
    start_date = "2010-01-01",
    end_date = "2019-12-31",
    region_source = "EEZ",
    region = sa_eez$id,
    duration = 30,
    key = key
)
sa_gap_events <- get_event(
    "GAP",
    start_date = "2010-01-01",
    end_date = "2019-12-31",
    region_source = "EEZ",
    region = sa_eez$id,
    duration = 30,
    key = key
)
