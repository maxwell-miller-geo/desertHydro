# # Model Run complete
# source("ALHM.R")
# 
# date <- "2022-08-27"
# x <- date
# dates <- c("2022-07-05", "2022-07-15", "2022-07-24", "2022-08-27")
# dates <- c("2005-08-19", "2007-01-08", "2010-08-05", "2015-06-11")
# 
# #WatershedElements <- r"(Z:\Desktop\ThesisGIS\Arid-Land-Hydrology\R\Example\WatershedElements)" # folder to save watershed characteristics
# # Keys dates
# 
# # Base Folder
# base <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\All\)"
# 
# # Mini version
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
# 
# # Full version
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)" # full version
# 
# # Model folder to store outputs
# ModelFolder <- ModelFolder_mini <-  paste0(base, date, "-mini")
# 
# #base <- r"(Z:\Desktop\ThesisGIS\Arid-Land-Hydrology\Data\)"
# 
# # Model Folders
# # ModelFolder <- paste0(base, date, "-full")
# 
# for(x in dates){
#   print(x)
#   # Model folder to store outputs
#   ModelFolder <-  paste0(base, x, "-full")
#   # Script Execution
#   nonspatial <-  arid_model(ModelFolder,
#                      date = x,
#                      time_step = 1,
#                      simulation_length = 5,
#                      WatershedElements = WatershedElements,
#                      rain_spatial = F,
#                      overwrite = F,
#                      impervious = T)
# 
#   
#   # Model folder to store outputs
#   ModelFolderSpatial <-   paste0(base, x, "-spatial")
#   # Script Execution
#   spatial <-  arid_model(ModelFolderSpatial,
#                          date = x,
#                          time_step = 1,
#                          simulation_length = NA,
#                          WatershedElements = WatershedElements,
#                          rain_spatial = T,
#                          overwrite = F,
#                          impervious = T)
#   
#   # Model folder to store outputs
#   ModelFolderPervious <-  ModelFolder <- paste0(base, x, "-pervious")
#   # Script Execution
#   pervious <-  arid_model(ModelFolderPervious,
#                          date = x,
#                          time_step = 1,
#                          simulation_length = NA,
#                          WatershedElements = WatershedElements,
#                          rain_spatial = F,
#                          overwrite = F,
#                          impervious = F)
#   
# }
# mini <-  arid_model(ModelFolder,
#                        date = date,
#                        time_step = 1,
#                        simulation_length = NA,
#                        WatershedElements = WatershedElements,
#                        rain_spatial = T,
#                        overwrite = T,
#                        impervious = T,
#                        mini = T)
