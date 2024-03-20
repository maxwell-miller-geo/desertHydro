# # Test a bunch of files
#
# source("ALHM.R")
# library(profvis)
# ## ----------------------------------
# # Date
# date <- "2001-06-26"
# date <- "2001-08-22"
# date <- "2002-06-08" # no rainfall - check date
#
# date <- "2018-08-01" # has discharge - not very good
# date <- "2012-07-15"
#
# # Keys dates
# dates <- c("2022-07-05", "2022-07-15", "2022-07-24", "2022-08-27")
# # Demo Section
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements)"
#
# for(x in dates){
#   print(x)
#   # Demo Model Folder
#   ModelFolder <- ModelDemo <- file.path(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Demo_Test)", date)
#   # Script
#   z <- arid_model(ModelFolder,
#                   date = date,
#                   time_step = 1,
#                   simulation_length = NA,
#                   overwrite = T,
#                   WatershedElements = WatershedElements,
#                   store = T,
#                   gif = T,
#                   crop = F,
#                   discharge = F,
#                   impervious = T)
#   gc()
# }
#
#
#
#
#
#
# #plot(z)
# # Date
# # date <- "2004-06-29"
# #
# # Base Folder
# base <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\)"
#
# # Mini version
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
#
# # Model folder to store outputs
# ModelFolder <- ModelFolder_mini <-  paste0(base, date, "-mini")
#
# # Script
# library(profvis)
#
# mini <- arid_model(ModelFolder,
#                    date = date,
#                    time_step = 1,
#                    simulation_length = 30,
#                    WatershedElements = WatershedElements,
#                    mini = T,
#                    store = T,
#                    gif = T,
#                    impervious = T,
#                    overwrite = T,
#                    rainfall_method = "gauges")
#
#
#
# #
# # ##------------------------------------------
# # Full Model
# date <- "2016-07-01"
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)" # folder to store watershed characteristics
#
# # Model Folders
# ModelFolder <- paste0(base, date, "-full-t")
#
# # Script
# arid_model(ModelFolder, date, time_step = 1, simulation_length = NA, WatershedElements = WatershedElements)
#
#
#
# # ##-----------------------------------------
# # # Option for running a lot of the discharge days - find errors within them
# source("ALHM.R")
# # Demo version
# #WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements)" # demo version
#
# # Mini version
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
#
# days <- readr::read_csv("rainfall_discharge_dates.csv", show_col_types = F) # located in base folder
# day_list <- data.frame(day = days[,1]) # dataframe with list of available dates
#
# #day_list <- days
# # Loop through all of the days and create relevant discharge information
#
#
# for(x in 1:nrow(day_list)){
#   print(day_list[x, 1])
#   date <- as.Date(day_list[x, 1])
#   # Demo Model Folder
#   #baseFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\All\Demo)"
#   baseFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\All\Cropped)"
#   ModelFolder <- ModelDemo <- file.path(baseFolder, date)
#   # Script
#   z <- arid_model(ModelFolder,
#                   date = date,
#                   time_step = 1,
#                   simulation_length = NA,
#                   WatershedElements = WatershedElements,
#                   rainfall_created = F,
#                   store = T,
#                   gif = T,
#                   crop = F,
#                   discharge = T,
#                   impervious = T,
#                   mini = T
#               )
#   rm(z)
# }
#
