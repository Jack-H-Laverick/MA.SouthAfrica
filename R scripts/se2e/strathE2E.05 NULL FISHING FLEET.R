
#### Setup                                            ####

library(tidyverse)
source("./R scripts/@_Region file.R")

#### Turn off fishing effort ####

Effort <- read.csv(str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity_CELTIC_SEA_2003-2013.csv")) %>% 
  mutate(`Activity_.s.m2.d.` = 0) 

write.csv(Effort, row.names = F,
          file = str_glue("./StrathE2E/Models/{implementation}/2010-2019/Param/fishing_activity_{toupper(implementation)}_2010-2019.csv"))   # Read in example boundary drivers
