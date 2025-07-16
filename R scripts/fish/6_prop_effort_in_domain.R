# Calculate the proportion of effort of each gear type from the SAU area that is contained
# within the domain area to scale the SAU catch data in following scripts.

source("./R scripts/fish/fishing_spatial_functions.R")

domain <- readRDS("./Objects/Domains.rds")
domain <- st_union(domain)

# Retrieve just the area that SAU uses for the Atlantic and Cape region, which is the intersection between the SA EEZ and the FAO zone 47
sau_west_area <- read_sf("./Objects/sau_atlantic_cape_region.gpkg")

# Determine the proportion of activity within the sau_west_area that is within the domain for each gear type to scale SAU catch data
#
# Midwater trawls
mw_intensity_fn <- glue("../../Spatial Data/fishing_effort_data/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity/Midwater_Trawl_Intensity_{crs}.tif")

mw_intensity_domain <- format_sanbi_raster(mw_intensity_fn, domain)
mw_activity_domain <- global(mw_intensity_domain, fun = "sum", na.rm = TRUE)
mw_intensity_sau_area <- format_sanbi_raster(mw_intensity_fn, sau_west_area)
mw_activity_sau_area <- global(mw_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_mw <- mw_activity_domain / mw_activity_sau_area
prop_sau_mw <- prop_sau_mw[1, 1]

#
# Nets including small scale
gn_fn <- glue("../../Spatial Data/fishing_effort_data/Gill_Netting_Intensity/Gill_Net_Intensity/Gill_Net_Intensity_{crs}.tif")
bs_fn <- glue("../../Spatial Data/fishing_effort_data/Beach_Seine_Intensity/Beach_seine_Intensity/Beach_Seine_Intensity_{crs}.tif")

## Combining gillnet and beach seine activity data for domain region
gn_intensity_domain <- format_sanbi_raster(gn_fn, domain)
gn_intensity_domain <- subst(gn_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
bs_intensity_domain <- format_sanbi_raster(bs_fn, domain)
bs_intensity_domain <- subst(bs_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
net_intensity_domain <- gn_intensity_domain + bs_intensity_domain
net_activity_domain <- global(net_intensity_domain, fun = "sum", na.rm = TRUE)

gn_intensity_sau_area <- format_sanbi_raster(gn_fn, sau_west_area)
gn_intensity_sau_area <- subst(gn_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
bs_intensity_sau_area <- format_sanbi_raster(bs_fn, sau_west_area)
bs_intensity_sau_area <- subst(bs_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
net_intensity_sau_area <- gn_intensity_sau_area + bs_intensity_sau_area
net_activity_sau_area <- global(net_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_nets <- net_activity_domain / net_activity_sau_area
prop_sau_nets <- prop_sau_nets[1, 1]

#
# Pole and line
tpl_fn <- glue("../../Spatial Data/fishing_effort_data/Tuna_Pole_Intensity/Tuna_Pole_Intensity_{crs}.tif")
tpl_intensity_domain <- format_sanbi_raster(tpl_fn, domain)
tpl_intensity_domain <- tpl_intensity_domain["Tuna_Pole_Intensity_1"]
tpl_activity_domain <- global(tpl_intensity_domain, fun = "sum", na.rm = TRUE)

tpl_intensity_sau_area <- format_sanbi_raster(tpl_fn, sau_west_area)
tpl_intensity_sau_area <- tpl_intensity_sau_area["Tuna_Pole_Intensity_1"]
tpl_activity_sau_area <- global(tpl_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_tpl <- tpl_activity_domain / tpl_activity_sau_area
prop_sau_tpl <- prop_sau_tpl[1, 1]

#
# Squid jig
sj_fn <- glue("../../Spatial Data/fishing_effort_data/Squid_Intensity/Squid_Intensity/Squid_Intensity_{crs}.tif")
sj_intensity_domain <- format_sanbi_raster(sj_fn, domain)
sj_activity_domain <- global(sj_intensity_domain, fun = "sum", na.rm = TRUE)
sj_intensity_sau_area <- format_sanbi_raster(sj_fn, sau_west_area)
sj_activity_sau_area <- global(sj_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_sj <- sj_activity_domain / sj_activity_sau_area
prop_sau_sj <- prop_sau_sj[1, 1]

#
# Longline
pll_fn <- glue("../../Spatial Data/fishing_effort_data/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity/Pelagic_Longline_Intensity_{crs}.tif")
dmll_fn <- glue("../../Spatial Data/fishing_effort_data/Hake_Longline_Intensity/Hake_Longline_Intensity_{crs}.tif")

pll_intensity_domain <- format_sanbi_raster(pll_fn, domain)
pll_intensity_domain <- subst(pll_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
dmll_intensity_domain <- format_sanbi_raster(dmll_fn, domain)
dmll_intensity_domain <- dmll_intensity_domain["Hake_Longline_Intensity_1"]
dmll_intensity_domain <- subst(dmll_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer

ll_intensity_domain <- pll_intensity_domain + dmll_intensity_domain
ll_activity_domain <- global(ll_intensity_domain, fun = "sum", na.rm = TRUE)

pll_intensity_sau_area <- format_sanbi_raster(pll_fn, sau_west_area)
pll_intensity_sau_area <- subst(pll_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
dmll_intensity_sau_area <- format_sanbi_raster(dmll_fn, sau_west_area)
dmll_intensity_sau_area <- dmll_intensity_sau_area["Hake_Longline_Intensity_1"]
dmll_intensity_sau_area <- subst(dmll_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer

ll_intensity_sau_area <- pll_intensity_sau_area + dmll_intensity_sau_area
ll_activity_sau_area <- global(ll_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_longline <- ll_activity_domain / ll_activity_sau_area
prop_sau_longline <- prop_sau_longline[1, 1]

#
# Purse seine
ps_fn <- glue("../../Spatial Data/fishing_effort_data/Small_Pelagic_Intensity/Small_Pelagic_Intensity_{crs}.tif")

ps_intensity_domain <- format_sanbi_raster(ps_fn, domain)
ps_intensity_domain <- ps_intensity_domain["Small_Pelagic_Intensity_1"]
ps_activity_domain <- global(ps_intensity_domain, fun = "sum", na.rm = TRUE)
ps_intensity_sau_area <- format_sanbi_raster(ps_fn, sau_west_area)
ps_intensity_sau_area <- ps_intensity_sau_area["Small_Pelagic_Intensity_1"]
ps_activity_sau_area <- global(ps_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_ps <- ps_activity_domain / ps_activity_sau_area
prop_sau_ps <- prop_sau_ps[1, 1]

#
# Demersal trawl
intrwl_fn <- glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Inshore_Intensity/Trawl_Inshore_Intensity_{crs}.tif")
oftrwl_fn <- glue("../../Spatial Data/fishing_effort_data/Demersal_Trawl_Intensity/Trawl_Offshore_Intensity/Trawl_Offshore_Intensity_{crs}.tif")

intrwl_intensity_domain <- format_sanbi_raster(intrwl_fn, domain)
intrwl_intensity_domain <- intrwl_intensity_domain["Trawl_Inshore_Intensity_1"]
intrwl_intensity_domain <- subst(intrwl_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
oftrwl_intensity_domain <- format_sanbi_raster(oftrwl_fn, domain)
oftrwl_intensity_domain <- oftrwl_intensity_domain["Trawl_Offshore_Intensity_1"]
oftrwl_intensity_domain <- subst(oftrwl_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer

dmtrwl_intensity_domain <- intrwl_intensity_domain + oftrwl_intensity_domain
dmtrwl_activity_domain <- global(dmtrwl_intensity_domain, fun = "sum", na.rm = TRUE)

intrwl_intensity_sau_area <- format_sanbi_raster(intrwl_fn, sau_west_area)
intrwl_intensity_sau_area <- intrwl_intensity_sau_area["Trawl_Inshore_Intensity_1"]
intrwl_intensity_sau_area <- subst(intrwl_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
oftrwl_intensity_sau_area <- format_sanbi_raster(oftrwl_fn, sau_west_area)
oftrwl_intensity_sau_area <- oftrwl_intensity_sau_area["Trawl_Offshore_Intensity_1"]
oftrwl_intensity_sau_area <- subst(oftrwl_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer

dmtrwl_intensity_sau_area <- intrwl_intensity_sau_area + oftrwl_intensity_sau_area
dmtrwl_activity_sau_area <- global(dmtrwl_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_dmtrwl <- dmtrwl_activity_domain / dmtrwl_activity_sau_area
prop_sau_dmtrwl <- prop_sau_dmtrwl[1, 1]

# West Coast Rock Lobster traps
# Assume that all West Coast Rock Lobster traps occurs within the domain as the South Coast Rock Lobster dominates the southern coast of South Africa
prop_sau_wcrl <- 1

#
# Recreational fishing gear
rlf_fn <- glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif")
rsb_fn <- glue("../../Spatial Data/fishing_effort_data/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity/Recreational_Shore_Fishing_Intensity_{crs}.tif")

## Combining recreational boat-based linefishing and shore-based activity data for domain region
rlf_intensity_domain <- format_sanbi_raster(rlf_fn, domain)
rlf_intensity_domain <- subst(rlf_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
rsb_intensity_domain <- format_sanbi_raster(rsb_fn, domain)
rsb_intensity_domain <- subst(rsb_intensity_domain, NA, 0) # Set NA values to 0 for addition of second layer
rec_intensity_domain <- rlf_intensity_domain + rsb_intensity_domain
rec_activity_domain <- global(rec_intensity_domain, fun = "sum", na.rm = TRUE)

rlf_intensity_sau_area <- format_sanbi_raster(rlf_fn, sau_west_area)
rlf_intensity_sau_area <- subst(rlf_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
rsb_intensity_sau_area <- format_sanbi_raster(rsb_fn, sau_west_area)
rsb_intensity_sau_area <- subst(rsb_intensity_sau_area, NA, 0) # Set NA values to 0 for addition of second layer
rec_intensity_sau_area <- rlf_intensity_sau_area + rsb_intensity_sau_area
rec_activity_sau_area <- global(rec_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_rec <- rec_activity_domain / rec_activity_sau_area
prop_sau_rec <- prop_sau_rec[1, 1]

#
# Small scale lines
ssl_fn <- glue("../../Spatial Data/fishing_effort_data/Linefish_Intensity/Linefish_Intensity/Linefish_Intensity_{crs}.tif")
ssl_intensity_domain <- format_sanbi_raster(ssl_fn, domain)
ssl_activity_domain <- global(ssl_intensity_domain, fun = "sum", na.rm = TRUE)
ssl_intensity_sau_area <- format_sanbi_raster(ssl_fn, sau_west_area)
ssl_activity_sau_area <- global(ssl_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_ssl <- ssl_activity_domain / ssl_activity_sau_area
prop_sau_ssl <- prop_sau_ssl[1, 1]

#
# Subsistence fishing gear
ssf_fn <- glue("../../Spatial Data/fishing_effort_data/Subsistence_Harvest_Intensity/Subsistence_Harvest_Intensity_{crs}.tif")
ssf_intensity_domain <- format_sanbi_raster(ssf_fn, domain)
ssf_intensity_domain <- ssf_intensity_domain["Subsistence_Harvest_Intensity_1"]
ssf_activity_domain <- global(ssf_intensity_domain, fun = "sum", na.rm = TRUE)
ssf_intensity_sau_area <- format_sanbi_raster(ssf_fn, sau_west_area)
ssf_intensity_sau_area <- ssf_intensity_sau_area["Subsistence_Harvest_Intensity_1"]
ssf_activity_sau_area <- global(ssf_intensity_sau_area, fun = "sum", na.rm = TRUE)

prop_sau_ssf <- ssf_activity_domain / ssf_activity_sau_area
prop_sau_ssf <- prop_sau_ssf[1, 1]

prop_sau_activity <- data.frame(
    Gear_name = strathe2e_gear_types,
    Gear_code = gear_codes,
    proportion_sau_activity_in_domain = c(
        prop_sau_mw,
        prop_sau_nets,
        prop_sau_tpl,
        prop_sau_sj,
        prop_sau_longline,
        prop_sau_ps,
        prop_sau_dmtrwl,
        prop_sau_wcrl,
        prop_sau_rec,
        prop_sau_ssl,
        prop_sau_ssf
    )
)
write.csv(prop_sau_activity, "./Objects/proportion_sau_activity_in_domain.csv")
