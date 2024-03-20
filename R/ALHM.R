# Complete Model
# Wrap it up
arid_model <- function(ModelFolder,
                       date = NULL,
                       time_step = 1,
                       simulation_length = NA,
                       WatershedElements = r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)",
                       mini = F,
                       rainfall_method = "gauges",
                       store = T,
                       gif = T,
                       crop = T,
                       discharge = F,
                       impervious = F,
                       overwrite = F,
                       write = T,
                       restartModel = F,
                       land_cover_file = NA,
                       ...){

# ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Test_1)"
## Section 1
## 1. Load in necessary input data
# Libraries that are necessary
libs <- c(
  "tidyverse", "tidyterra", "data.table",
  "ggmap", "classInt", "gifski",
  "gganimate", "reshape2", "tidyverse",
  "dplyr", "readxl", "gridExtra",
  "ggplot2", "zoo", "purrr",
  "ggtext", "whitebox", "bookdown",
  "terra", "viridis", "viridisLite")

# Check if packages are install or not
installed_libraries <- libs %in% rownames(installed.packages())

if(any(installed_libraries == F)){
  install.packages(libs[!installed_libraries])
}
invisible(lapply(
  libs, library, character.only = T
))

##-------------------------
# Section 2
# Adjust this folder to the location of where Watershed elements are stored
#WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)" # folder to store watershed characteristics
#WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
# Adjust this folder of where to store the model run
#ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Taylor_Model)" # folder to store modeled outputs
#ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Test_1)"

if(!file.exists(ModelFolder)){ # Create Model folder, if it doesn't exist
  dir.create(ModelFolder)
}
model_complete <- file.path(ModelFolder, "ModelComplete.txt")
print("Checking if model is completed...")
if(file.exists(model_complete) & !overwrite){ # check if model complete
  print("The model already exists in: ")
  print(ModelFolder)
  print("Next model...")
  return(0)
}
# Each model folder should contain one rainfall event
#date <- "2007-07-23" # date of event - large event - 1062 cfs
#date <- "2015-10-19" # date of event - medium event - 110 cfs
#date <- "2018-08-01" # date of event - smaller event - 76 cfs - bad date
#date <- "2004-06-29" # cfs 124
# Options
#rainfall_created <- F
# rainfall_weighted <- TRUE
# store = T
# rain_spatial <- TRUE
# spatial <- FALSE

##------------------------------------
# Preprocess
# Input files
#source("WatershedSetUp.R")
# Files below are not needed for Example Script
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)" # path of dem extent
# land_cover_path <-  r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # land cover - unclipped file.
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)" # watershed boundary
#print(rainfall_method)
dem_path <- file.path(WatershedElements, "waterholes_extent.tif") # path of dem extent
if(!crop){ # if it isn't cropped, it will adjust to look for the demo file.
  watershed_shape_path <-  NA
  dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements\demo_dem.tif)"
}else if(mini){
  watershed_shape_path <- file.path(WatershedElements,"mini_ws.shp") # watershed boundary ### IF NOT MINI
}else{
  watershed_shape_path <- file.path(WatershedElements,"waterholes_shape.shp") # watershed boundary ### IF NOT MINI
}

# Function adjusts digital elevation model (smooths with preserved features)and land cover map is projected in same coordinate system and clipped to watershed.
landcovername <- watershedElements(Outpath = WatershedElements, DEM = dem_path, WatershedShape = watershed_shape_path, land_cover_file = land_cover_file)

##--------------------------------
# Initial conditions
# store initial conditions for a particular model.
#source("initialConditions.R")
# Assign the models - DEM
# Smoothed DEM - can use the unaltered DEM or a filled/ breached model.
# It is not recommended to use the original DEM
model_dem <- file.path(WatershedElements, "model_dem.tif") # can adjust the input dem
#"C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\"
smooth_dem <- terra::rast(model_dem)

initial_conditions(ModelOutputs = ModelFolder, model_dem = model_dem) # saves initial conditions into model folder
##-----------------------------------
## Initial Soil conditions
#source("initialSoilConditions.R")

LandCoverCharacteristics <- file.path(WatershedElements, "LandCoverCharacteristics.xlsx") # default excel file within current project folder
#LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
landcovername <- "landcover.tif"

ClassificationMap <- file.path(WatershedElements, landcovername) # adjusted/cropped classification map - must be named correctly
#DEM <- file.path(WatershedElements, "cropped_dem.tif") # clipped dem, elevations unaltered - must be named correctly
DEM <- file.path(WatershedElements, "model_dem.tif") # modified dem, elevations unaltered - must be named correctly
# Initial soil conditions for model - a stacked map of soil characteristics including:
# Initial saturation, porosity, soil depth, hydraulic conductivity, etc.
# See initialSoilConditions.R for more details
initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
                        ClassificationMap = ClassificationMap,
                        DEM = DEM,
                        ModelOutputs = ModelFolder) # Saves the modeled soil stack as raster brick

##--------------------------------------
## Rainfall
## 2b. Weather - Rain data
#eventDate <- "2012-07-15"
#source("Rainfall_Process.R")
# Read in the rainfall data from a saved file, normalize it, and create a
rain_file <- rainfallCreation(ModelFolder, WatershedElements, date = date, method = rainfall_method, overwrite = overwrite)
# Slight issue: will use saved rainfall data if present - does not check to see what type of data the rainfall is

##---------------------
## Discharge presence - obtain information for graphing
#source("Discharge_Process.R")
rain_discharge <- dischargeCreate(date = date, ModelFolder, WatershedElements)

# if(discharge){# Run if discharge data is present
#   print("Processing discharge data...")
#   # Load in the filtered rainfall file
#   rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
#   rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")
#   if(!file.exists(rain_discharge_file) | !file.exists(rainFiltered_file)){
#
#   #rainFiltered <- readr::read_csv(rain_file)
#   # Check if discharge present on day - returns the date or optional date
#   date <- lubridate::date(discharge_present(WatershedElements, date))[1] # returns discharge date or next day (first     entry)
#   # Load in stream data from Waterholes - GCMRC
#   dischargeDataPath <- file.path(WatershedElements, "Waterholes_Stream_gcmrc20231127132459.tsv")
#   # Calculate the daily discharge for given date
#   dischargeDF <- dailyDischarge(discharge_file_path = dischargeDataPath,
#                                 discharge_date = date,
#                                 save_location = ModelFolder,
#                                 saveGraphs = store)
#
#   # Combine the rainfall and discharge into a single .csv file
#   rain_discharge <- rainfall_discharge_combine(rainfallDF = rainFiltered,
#                                                dischargeDF,
#                                                outpath = ModelFolder,
#                                                store = store)
#   } else{
#     rain_discharge <- readr::read_csv(rain_discharge_file, show_col_types = F)
#     rainFiltered <- readr::read_csv(rainFiltered_file, show_col_types = F)
#   }
# }else{ # If no discharge present
#   print("No discharge present...")
#   rain_discharge <- readr::read_csv(rain_file, show_col_types = F) |>
#     dplyr::select(time, Total_in) |>
#     dplyr::add_row(Total_in = 0,
#                    time = 0,
#                    .before = 1) |>
#     dplyr::arrange(time)
#     # write rain-discharge into model folder
#     rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")
#     readr::write_csv(x = rain_discharge, file = rain_discharge_file)
# }

# For both cases of discharge
# Calculate the number of observations
observations <- nrow(rain_discharge)

# Calculate the length of recorded rainfall-discharge - could be within script..
duration <- max(rain_discharge$time) # minutes

# Calculate the total rainfall
total_rain <- sum(rain_discharge$Total_in)


# store plots
if(store & discharge){
  plot_rainfall_discharge(rain_discharge, date = date, store = store, outpath = ModelFolder)
}
#
## ------------------------------------
## Pre-model checks
## 3. Checks
# Don't run model if the files are not present

# # Rainfall check
# if(rain_spatial){
#   rain_file <- rain_spatial_file
# } else{
#   rain_file <- rain_file
# }

# Necessary elements for the model
SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
flowStack_file <- file.path(WatershedElements, "stack_flow.tif")
landCover_file <- file.path(WatershedElements, "landcover.tif")
slope_file <- file.path(WatershedElements, "model_slope.tif")
rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")

files_needed <- c(WatershedElements, ModelFolder, landCover_file, SoilStack_file, flowStack_file, rain_file, slope_file, rain_discharge_file)

for(x in files_needed){
  if(!file.exists(x)){
    print(paste0("The file ", x, " does not exist in this current location. \n"))
    break
  }
}
print("All files checked.")

#time_step <- .25
if(is.na(simulation_length)){
  simulation_length <- max(rain_discharge$time) # Simulation length derived from the discharge data
}

#simulation_length <- 5
files_recommended <- c(time_step, simulation_length)
##--------------------------- Flow Model
## Flow Model
# ## 4. Model Script
# source("flowModel.R")
# source("utils.R")
# Necessary elements for the model
# SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
# flow_file <- file.path(WatershedElements, "stack_flow.tif")
# land_cover_clip_file <- file.path(WatershedElements, "landcover.tif")
# slope_file <- file.path(WatershedElements, "model_slope.tif")

#library(profvis)
# profvis({
# Soil Moisture Routing Model - writes
print("Beginning Model Run...")
flowModel(SoilStack_file = SoilStack_file,
          flowStack_file = flowStack_file,
          landCover_file = landCover_file,
          slope_file = slope_file,
          rain = rain_file,
          ModelFolder = ModelFolder,
          time_step = time_step,
          simulation_length = simulation_length,
          write = write,
          rainfall_method = rainfall_method,
          impervious = impervious,
          gif = gif,
          restartModel = restartModel
          )
## ----------------------------------------------------------------------------
## Display - Ought to be modular
# source("utils.R")
# source("postAnalysis.R")

print(paste0("Creating graphics in ", ModelFolder))
# Path to stacked rasters
surfaceStorage <- terra::rast(file.path(ModelFolder, "SurfaceStorage.tif"))
velocityStorage <- terra::rast(file.path(ModelFolder, "VelocityStorage.tif"))
subsurfaceStorage <- terra::rast(file.path(ModelFolder, "subsurfaceStorage.tif"))
#velocityStorage <- terra::rast(file.path(ModelFolder, "Velocities.tif"))
#subsurfaceStorage <- terra::rast(file.path(ModelFolder, "Soil_Moisture_percent.tif"))

#x_sections_path <- "gauge_waterholes.shp" # shapefile with points to measure discharge
x_sections_path <- file.path(WatershedElements, "gauges.shp")
if(file.exists(x_sections_path) & discharge){
  print(paste("Creating discharge figures..."))
  rain_discharge <- readr::read_csv(file.path(ModelFolder, "rain-discharge.csv"), show_col_types = F)
  #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
  cross_section <- terra::vect(x_sections_path) # bring vector into R
  # # Extract the height from the surface stack
  surface_Height <- terra::extract(surfaceStorage, cross_section, method = "simple") # surface height in cm
  surface_velocity <- terra::extract(velocityStorage, cross_section) # velocity at given time (m/s)
  cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
  for(x in 1:nrow(surface_Height)){
    surface_discharge <- as.numeric(surface_Height[x,3:ncol(surface_Height)]* cm_to_m2 * surface_velocity[x,3:ncol(surface_velocity)])
    xvalues <- as.numeric(colnames(surface_Height[x,3:ncol(surface_Height)]))
    estimated <- data.frame(time = xvalues, predDis = surface_discharge)
    # Combined discharges for comparisons
    compareDis <- compareDischarge(rain_discharge, estimated) # outputs: time|recDis|predDis

    dischargePlot <- ggplot() +
      geom_line(aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
      geom_line(aes(x = compareDis$time, y = compareDis$predDis, color = "Predicted")) +
      labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date),
           x = paste0("Time (minutes)"),
           y = bquote("Discharge "(ft^3/s)),
                      color = "Legend")

    dischargePlot
    if(store){
      ggsave(filename = file.path(ModelFolder, paste0("compare-discharge-xsection-", x, "-", date,".png")),
             plot = dischargePlot, width = 6)
    }
  }

  # cm_to_ft <- 1/(2.54*12) # conversion factor - Conversion 1/(2.54*12 = .0328)
  # surface_Height <- surface_Height[,3:ncol(surface_Height)] * cm_to_ft
  # height values
  # Calculate linear model for discharge and height for the day
  # B <- quadratic_lm(rain_discharge$height, rain_discharge$discharge)
  # # Calculate discharge values
  # surface_discharge <- B[1] + B[2] * surface_Height + B[3] * surface_Height ^2
  # xvalues <- as.numeric(colnames(surface_Height))

  #surface_discharge <- as.numeric(surface_Height[,3:ncol(surface_Height)] * surface_velocity[,3:ncol(surface_velocity)])
  #estimated <- data.frame(time = xvalues, predDis = as.numeric(as.vector(surface_discharge[1,])))

} else if(file.exists(x_sections_path)){
  print(paste("Creating discharge figures..."))
  #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
  cross_section <- terra::vect(x_sections_path) # bring vector into R
  # # Extract the height from the surface stack
  surface_Height <- terra::extract(surfaceStorage, cross_section) # surface height in cm
  surface_velocity <- terra::extract(velocityStorage, cross_section) # velocity at given time (m/s)
  cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
  for(x in 1:nrow(surface_Height)){
    estimated <- as.numeric(surface_Height[x,3:ncol(surface_Height)] * cm_to_m2 * surface_velocity[x,3:ncol(surface_velocity)])
    xvalues <- seq(time_step, simulation_length, by = time_step)
    dischargePlot <- ggplot() +
      #geom_line(aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
      geom_line(aes(x = xvalues, y = estimated, color = "Predicted")) +
      labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date),
           x = paste0("Time (minutes)"),
           y = bquote("Discharge "(ft^3/s)),
           color = "Legend")
    dischargePlot
  if(store){
    ggsave(filename = file.path(ModelFolder, paste0("discharge-prediction","-xsection-", x, "-", date,".png")),
           plot = dischargePlot, width = 6)
    }
  }
}

##--------------------------------
# Surface GIF
# Functions for visualizations
# source("Plotting.R")
# # Libraries
# library(ggplot2)
# library(viridisLite)
# library(gganimate)
# library(viridis)

print("Retrieving rainfall data for simulation")
if(discharge){ # gathers total rain and rain duration values
  print("Retrieving rainfall data from simulation: rain_discharge")
  total_rain <- round(sum(rain_discharge$Total_in),3)
  rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
  rainFiltered <- readr::read_csv(rainFiltered_file, show_col_types = F)
  total_rain_duration <- as.numeric((tail(rainFiltered$Time_minute, n = 1) - rainFiltered$Time_minute[1]))
  rain <- readr::read_csv(rain_file, show_col_types = F)
}else{
  print(paste("No rain-discharge data: Retrieving rainfall data from rainfile...", rain_file))
  rain <- readr::read_csv(rain_file, show_col_types = F)
  total_rain <- round(sum(rain$Total_in),3)
  total_rain_duration <- as.numeric((tail(rain$time, n = 1) - rain$time[1]))
}

if(gif){
  print(paste("Creating surface depth animation..."))
# Load in surface storage
#surfaceStorage <- rast(file.path(ModelFolder, "Surface_Storage.tif"))
# Use custom function to melt the raster stack into a dataframe for plotting
meltedSurface <- meltStack(surfaceStorage, timevalues = xvalues) # Surface depths through time

# Create an animated ggplot - Surface Storage
surface_plot <- animateStack(meltedSurface,
                             title = "Surface Depth",
                             units = "Depth (cm)",
                             caption = paste0(total_rain," inches of rain over ",  round(total_rain_duration, 2)," minutes."))
# Display the animation
#gganimate::animate(surface_plot)
# store the animated GIF
anim_save(filename = paste0(date,"-surface-Depth.gif"), path = ModelFolder, animation = surface_plot, fps = 10, renderer = gifski_renderer())
##--------------------------------------
## Post model Checks
# Does not currently account for spatial distribution
# precipitation_actual <- length(terra::cells(surfaceStorage)) * total_rain * 2.54 # ncells * in
# # Last surface layer calculated
# lastLayer <- nlyr(surfaceStorage)
# precipitation_last <- surfaceStorage[[lastLayer]]
# depth_sum <- sum(values(precipitation_last), na.rm = T)

# # Testing functions
# df_max <- dfMax(precipitation_last) # create data.frame
#
# vectCreation(df_max, saveLoc = ModelFolder, name = "depth-max.shp", coords = crs(SoilStack)) # create vector
## ---------------------------------------
# Subsurface depths
#Load in surface moisture
print(paste("Creating soil moisture animation..."))
#subsurfaceStorage <- rast(file.path(ModelFolder, "Soil_Moisture_percent.tif"))
meltedMoistureContent <- meltStack(subsurfaceStorage, timevalues = xvalues) # subsurface % fill through time
# Create an animated ggplot - Subsurface Storage
subsurface_plot <- animateStack(meltedMoistureContent,
                                title = "Moisture Content",
                                units = "% Full",
                                caption = paste0(total_rain," inches of rain over ",  round(total_rain_duration, 2)," minutes."))

# Display the animation
#gganimate::animate(subsurface_plot)
# store the animated GIF
anim_save(filename = paste0(date, "-moisture-content.gif"), path = ModelFolder, animation = subsurface_plot, fps = 10, renderer = gifski_renderer())
## ------------------------------
# Load in the velocity storage
print(paste("Creating surface velocity animation..."))
#velocityStorage <- rast(file.path(ModelFolder, "Velocities.tif"))
velocityStack <- meltStack(velocityStorage, timevalues = xvalues) # subsurface % fill through time
# Create an animated ggplot - Subsurface Storage
velocity_plot <- animateStack(velocityStack,
                              title = "Surface Velocities",
                              units = "m/s",
                              caption = paste0(total_rain," inches of rain over ", round(total_rain_duration, 2)," minutes."))
# Display the animation
#gganimate::animate(velocity_plot)
# store the animated GIF
anim_save(filename = paste0(date,"-velocity.gif"), path = ModelFolder, animation = velocity_plot, fps = 10, renderer = gifski_renderer())
##
}

print(paste0("End of script, thank you. You stuff is saved in ", ModelFolder))
return(list(surfaceStorage, subsurfaceStorage, velocityStorage, rain))
}
