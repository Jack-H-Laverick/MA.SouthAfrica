
# Create objects to fill the SE2E compiler scripts when an overhang isn't being used.

rm(list=ls())                                                               # Wipe the brain

#### For Physics file ####

user_overhang_diff <- expand.grid(Month = 1:12, 
                                      Year = 1980:2099,
                                      Vertical_diffusivity = 1)             # No overhang (1 gets log10'ed later to return 0)

saveRDS(user_overhang_diff, "./Objects/overhang diffusivity.rds")

#### For boundary file (also used in physics) ####

user_overhang_boundary <- expand.grid(Month = 1:12, 
                            Year = 1980:2099,
                            Direction = c("Downwelling", "Upwelling"),
                            DIN = 0,
                            Detritus = 0,
                            Vertical_velocity = 0)                                         

saveRDS(user_overhang_boundary, "./Objects/overhang exchanges.rds")
