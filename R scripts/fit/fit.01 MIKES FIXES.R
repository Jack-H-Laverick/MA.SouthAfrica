
## Copy over Mikes fixes and fitted parameters into the future model variants

#### Setup ####

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

#### Set iterations ####

decades <- data.frame(Start = seq(2010, 2060, by = 10),                     # Which time periods are we building driving data for?
                      Stop = seq(2019, 2069, by = 10)) %>% 
  rowid_to_column()

runs <- expand.grid(Force = c("GFDL", "CNRM"), S = c("ssp370", "ssp126"),   # Get a combination of forcings and SSPs
                    rowid = decades$"rowid") %>%                            # For each decade we are extracting
  left_join(decades) %>%                                                    # Add the beginning and end for each time period 
  select(-rowid) %>% 
  data.frame()

#### Fixes ####

pmap(runs, safely(function(Force, S, Start, Stop, Boundary_template){
  
  #  Start <- 2030 ; Stop <- 2039 ; Force <- "GFDL" ; S <- "ssp370" 
  
  #### Fix Physics ####
  
  Physics_template <- read.csv(file = stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/Driving/physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"))  # Read in example Physical drivers
  
  write.csv(Physics_template, 
            file = stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/Driving/ORIGINAL_physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"),
            row.names = F)
  
  Physics_new <- mutate(Physics_template,
                        SO_OceanIN = SO_OceanIN * 5.8,
                        D_OceanIN = D_OceanIN * 5.1,
                        SI_OceanIN = SI_OceanIN * 1.2,
                        SI_OceanOUT = SI_OceanOUT * 1.5,
                        SO_SI_flow = SO_SI_flow * 131.9
  ) 
  
  write.csv(Physics_new, 
            file = stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/Driving/physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"),
            row.names = F)
  
  #### Copy over drivers ####
  
  file.copy(stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/Driving/physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"),
            stringr::str_glue("./StrathE2E/{implementation}/2010-2015-{Force}-ssp370/Driving/physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"))
  
  file.copy(stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/Driving/chemistry_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"),
            stringr::str_glue("./StrathE2E/{implementation}/2010-2015-{Force}-ssp370/Driving/chemistry_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"))
  
}))