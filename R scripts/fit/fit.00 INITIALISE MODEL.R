
## Initialise model

library(ggplot2) ; source("./R scripts/@_Region file.R") # ggplot2 is needed to source the Region file


R.utils::copyDirectory(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/"),
                       stringr::str_glue("./StrathE2E/{implementation}_prefit/2010-2019/")) # Save the prefitted models    

R.utils::copyDirectory("./Data/Mike_fitted/",              # Copy example model 
                       stringr::str_glue("./StrathE2E/{implementation}/"))    # Into new implementation

