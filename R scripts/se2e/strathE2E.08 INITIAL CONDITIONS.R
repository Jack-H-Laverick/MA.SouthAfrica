
library(StrathE2E2)

model <- e2e_read("South_Africa","2010-2019", models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
 
results <- e2e_run(model,nyears = 50)                                # Run the model

e2e_plot_ts(model, results)                                          # Have we reached a steady state?

#### Update starting conditions ####

# e2e_extract_start(model, results, csv.output = TRUE)                # Update starting conditions to the end of a simulation
# 
# file.rename("./StrathE2E/South_Africa/2010-2019/Param/initial_values-base.csv",
#             "./StrathE2E/South_Africa/2010-2019/Param/initial_values_SOUTH_AFRICA_2010-2019.csv")
# 
# unlink("./StrathE2E/South_Africa/2010-2019/Param/initial_values_CELTIC_SEA_2003-2013.csv")
# 
# ## Update set up file
# 
# Setup_file <- read.csv("./StrathE2E/South_Africa/2010-2019/MODEL_SETUP.csv")
# 
# Setup_file[4,1] <- "initial_values_SOUTH_AFRICA_2010-2019.csv"
# 
# write.csv(Setup_file,
#           file = "./StrathE2E/South_Africa/2010-2019/MODEL_SETUP.csv",
#           row.names = F)
