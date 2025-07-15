
## Initialise model

library(ggplot2) ; source("./R scripts/@_Region file.R") # ggplot2 is needed to source the Region file

R.utils::copyDirectory("../Celtic Sea/StrathE2E/Celtic_Sea_MA/2010-2019-CNRM-ssp126/",  # Copy example model 
                       stringr::str_glue("./StrathE2E/{implementation}/2010-2019/"))    # Into new implementation

dir.create("./StrathE2E/Results")                                                       # Create results folder for model runs
  