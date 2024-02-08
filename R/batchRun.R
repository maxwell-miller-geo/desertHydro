# Test a bunch of files

# source("ALHM.R")
## ----------------------------------
# Demo Section
WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements)"

# Demo Model Folder
ModelFolder <- ModelDemo <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Demo_Test)"

# Date
date <- "2004-06-29"

# Script
# z <- arid_model(date, ModelFolder,
#            time_step = 1,
#            simulation_length = 25,
#            WatershedElements = WatershedElements,
#            save = F,
#            All_save = T,
#            crop = F,
#            discharge = F)

  # Date
# date <- "2004-06-29"
#
# # Base Folder
# base <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\)"
#
# # Mini version
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
#
# # Model folder to save outputs
# ModelFolder <- ModelFolder_mini <-  paste0(base, date, "-mini-1")
#
# # Script
# arid_model(date, ModelFolder, time_step = 4, simulation_length = 25, WatershedElements = WatershedElements, mini = T, save = F, All_save = T)
#
# ##------------------------------------------
# # Full Model
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)" # folder to save watershed characteristics
#
# # Model Folders
# ModelFolder <- paste0(base, date, "-full")
#
# # Script
# arid_model(date, ModelFolder, time_step = 5, simulation_length = NA, WatershedElements = WatershedElements)
#
# ##-----------------------------------------
# # Option for running a lot of the discharge days - find errors within them
# days <- readr::read_csv("days-of-disharge.csv") # located in base folder
# day_list <- as.vector(days[,2])
#
# # Loop through all of the days and create relevant discharge information
# for(date in day_list){
#
# }
