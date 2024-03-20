# Singular model
#source("ALHM.R")

# date <- "2022-08-27"
# rainfall_method <-  "gauges"
# time_step <- .5
# time_seconds <- time_step * 60
# #base <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\All\RainComparison)"
# base <- r"(Z:\Thesis\Arid-Land-Hydrology\Data)"
# #watershedbase <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example)"
# watershedbase <- r"(Z:\Thesis\Arid-Land-Hydrology\R\Example)"
# ModelFolder <- file.path(base, paste0("runoff-", rainfall_method,"-watershedcarve-",time_seconds,"-", date))
# simulation_length <- 20
# impervious <- T
# WatershedElements <- file.path(watershedbase, "ExperimentalElements")
# #ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\All\Mini\runoff2022-07-24)"
# mini = F
# store = T
# gif = F
# crop = T
# discharge = T
# impervious = T
# overwrite = T
# write = T
# restartModel = F
# land_cover_file = "land_cover_soils.shp"
# key = "MUKEY"
# library(profvis)
# out <- arid_model(ModelFolder,date,time_step,simulation_length,WatershedElements,mini,rainfall_method,store,gif,crop,discharge,impervious,overwrite,write,restartModel,land_cover_file,key)
#
# # Gif creation
# gifCreation(ModelFolder, out[[1]], rainfall_method, gif = T)
#
# # Points of Interest
# # Last layer
# # last <- length(names(out[[3]]))
# # plot(out[[3]][[last]], legend = T) # Velocity plot
# soil <- terra::rast(file.path(ModelFolder, "soilStorage.tif"))
# surface <- terra::rast(file.path(ModelFolder, "SurfaceStorage.tif"))
# velocity <- terra::rast(file.path(ModelFolder, "VelocityStorage.tif"))
# slope <- terra::rast(file.path(ModelFolder, "slope.tif"))
# SS <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
# mannings_n <- SS$mannings_n
# for(x in 1:(nlyr(surface)-1)){
#   terra:::add(mannings_n) <- SS$mannings_n
# }
#
# velocities <- ManningsWideChannelVelocity(mannings_n, surface, slope, length = 10)
# last <- length(names(surface))
# plot(surface[[last]])
# maxDepth <- terra::vect(file.path(ModelFolder, "max-depth.shp"))
# points(maxDepth, col = "red", )
# maxVel <- terra::vect(file.path(ModelFolder, "max-velocity.shp"))
# points(maxVel, col = "blue")
# gaugedLocations <- vect(file.path(WatershedElements, "gauges.shp"))
