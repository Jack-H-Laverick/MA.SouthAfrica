
# Remove the files which have been replaced by ones for the new region
unlink("./StrathE2E/South_Africa/2010-2019/Driving/chemistry_CELTIC_SEA_2003-2013.csv")
unlink("./StrathE2E/South_Africa/2010-2019/Param/physical_parameters_CELTIC_SEA.csv")
unlink("./StrathE2E/South_Africa/2010-2019/Driving/physics_CELTIC_SEA_2003-2013.csv")     # Delete old file

# Update file which tells StrathE2E where to find driving files

Setup_file <- read.csv("./StrathE2E/South_Africa/2010-2019/MODEL_SETUP.csv") # Read in example Physical drivers

Setup_file[1,1] <- "physical_parameters_SOUTH_AFRICA.csv"
Setup_file[2,1] <- "physics_SOUTH_AFRICA_2010-2019.csv"
Setup_file[3,1] <- "chemistry_SOUTH_AFRICA_2010-2019.csv"

write.csv(Setup_file,
          file = "./StrathE2E/South_Africa/2010-2019/MODEL_SETUP.csv",
          row.names = F)
