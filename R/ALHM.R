#' Desert Hydrology Model Full Suite
#'
#' @param ModelFolder The designated folder path to save the outputs from the files.
#' The script will write and modify the elements present within the output folder.
#' @param WatershedElements The folder path that contains all the necessary components to
#' create and run the hydrological model.
#' @param date Optional date string. must be in format "YYYY-MM-DD".
#' The date will be evaluated and pull necessary event information from downloaded .csv files - rain and discharge data.
#' Not very flexible and is based on the formatting of the event file date columns. Uses tidyverse packages to filter date information.
#' @param time_step Optional based time step in fraction of minutes. Defaults to 1 minute. Recommended: 0.25 - 1 minute.
#' The time step is the default evaluation time step for a model.
#' The script will dynamically adjust the time step based on the calculated surface velocities.
#' @param simulation_length Optional length of simulation in minutes. Defaults to rainfall duration + discharge duration.
#' @param mini Optional: T/F. Default False. If TRUE, script will look for "mini-ws.shp" within the Watershed Elements to crop
#' and reduce the size of the computational boundary. Best used for testing smaller portions of watershed.
#' Large computational areas + 1 million cells will take considerable time depending on the length of simulation.
#' @param rainfall_method Optional rainfall method string. Default "gauges" creates weighted average of rainfall within a given watershed.
#' The "gauges" data must be formatted correctly to apply rainfall. For additional options of rainfall see documenatatin for
#' rainfallCreate.
#' @param gif Optional: T/F: If TRUE, will create gif animations for each of the typical outputs - Surface water depth, surface water velocity, and soil moisture.
#' @param crop Optional: T/F: If TRUE, will crop using terra::crop datasets to boundary layer. Boundary or extent mismatches in spatial data
#' will prevent successful model simulations.
#' @param discharge Optional: T/F: If TRUE, script will use observed discharge data -expected format ".tsv"-
#' to modify simulation length and create compiled hydrographs with containing predicted and observed discharges.
#' Expects ESRI shapefile point at gauged location named "gauges.shp" within WatershedElements folder.
#' @param impervious Optional: T/F: If TRUE, model calculate runoff without infiltration.
#' Can be used to perform quicker model simulations to determine sensitivity or runoff potential.
#' @param overwrite Optional: T/F: If TRUE, overwrite elements within WatershedElements folder and ModelFolder when necessary.
#' @param write Optional to write outputs from model. Default = TRUE
#' @param restartModel Optional: T/F: If TRUE, model will attempt to restart from last recorded time and water surface elevations
#'  Note: If files within Model Folder are edited, it may not work.
#' @param land_cover_file Optional: Default NA: Expects string format of land cover tif file e.g. "landcover.tif" that is present within the WatershedElements fold.
#' Note: If changed, the key parameter needs to be changed to match the category column name. See vignette for expected structure.
#' @param key Optional: Default: "NLCD_Key" string for name of land cover types in excel table and the land cover map ".tif".
#' This will convert excel table hydrological characteristics into a stacked raster map with each layer corresponding to a hydrological characteristic and spatially distributed them.
#' See vignette for expected structure.
#' @param ... Additional parameters to pass into inner functions.
#'
#' @return Returns nothing outputs written to input model folder
#' @export
#'
#' @examples \dontrun{
#' # Change this to output folder
#' ModelFolder <- r"(C:/Thesis/Arid-Land-Hydrology/R/Example/SampleModel)"
#' WatershedElements <- file.path("inst/extdata/DemoElements") # demo elements
#' arid_model(ModelFolder, WatershedElements)
#' }
arid_model <- function(ModelFolder,
                       WatershedElements,
                       date = NULL,
                       time_step = 1,
                       simulation_length = NA,
                       mini = F,
                       rainfall_method = "gauges",
                       # store = T,
                       gif = T,
                       crop = T,
                       discharge = T,
                       impervious = F,
                       overwrite = F,
                       write = T,
                       restartModel = F,
                       land_cover_file = NA,
                       key = "NLCD_Key",
                       ...){

# ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Test_1)"
## Section 1
## 1. Load in necessary input data
# Libraries that are necessary
# libs <- c(
#   "tidyverse", "tidyterra", "data.table",
#   "ggmap", "classInt", "gifski",
#   "gganimate", "reshape2", "tidyverse",
#   "dplyr", "readxl", "gridExtra",
#   "ggplot2", "zoo", "purrr",
#   "ggtext", "whitebox", "bookdown",
#   "terra", "viridis", "viridisLite",
#   "stringr")
#
# # Check if packages are install or not
# installed_libraries <- libs %in% rownames(installed.packages())
#
# if(any(installed_libraries == F)){
#   install.packages(libs[!installed_libraries])
# }
# invisible(lapply(
#   libs, library, character.only = T
# ))

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
  print("Folder created...")
}
model_complete <- file.path(ModelFolder, "ModelComplete.txt")
print("Checking if model is completed...")
if(file.exists(model_complete) & !overwrite){ # check if model complete
  print("The model already exists in: ")
  print(ModelFolder)
  print("Next model...")
  return(0)
}
# Preprocess - Create watershed
##------------------------------------

# Input files
# source("WatershedSetUp.R")
# Files below are not needed for Example Script
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)" # path of dem extent
# land_cover_path <-  r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # land cover - unclipped file.
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)" # watershed boundary
#print(rainfall_method)
dem_path <- file.path(WatershedElements, "waterholes_extent.tif") # path of dem extent
if(!crop){ # if it isn't cropped, it will adjust to look for the demo file.
  watershed_shape_path <-  NA
  dem_path <- file.path(WatershedElements, "demo_dem.tif")
  #dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements\demo_dem.tif)"
}else if(mini){
  watershed_shape_path <- file.path(WatershedElements,"mini_ws.shp")
  file.exists(watershed_shape_path)# watershed boundary ### IF NOT MINI
}else{
  watershed_shape_path <- file.path(WatershedElements,"waterholes_shape.shp") # watershed boundary ### IF NOT MINI
}
if(is.na(watershed_shape_path)){
  print("No computational boundary layer")
}else if(file.exists(watershed_shape_path) | watershed){
  print("Located computational boundary layer")
}else{
  error(paste0("Could not locate computaitonal boundary:", watershed_shape_path))
}
# Function adjusts digital elevation model (smooths with preserved features)and land cover map is projected in same coordinate system and clipped to watershed.
landcovername <- watershedElements(Outpath = WatershedElements, DEM = dem_path, WatershedShape = watershed_shape_path, land_cover_file = land_cover_file, ModelFolder = ModelFolder)

# Initial conditions
##--------------------------------

# store initial conditions for a particular model.
#source("initialConditions.R")
# Assign the models - DEM
# Smoothed DEM - can use the unaltered DEM or a filled/ breached model.
# It is not recommended to use the original DEM
model_dem <- file.path(WatershedElements, "model_dem.tif") # can adjust the input dem
#"C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\"
#smooth_dem <- terra::rast(model_dem)

initial_conditions(ModelOutputs = ModelFolder, model_dem = model_dem) # saves initial conditions into model folder

## Initial Soil conditions
##-----------------------------------

#source("initialSoilConditions.R")

if(key == "NLCD_Key"){
  LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx" # default excel file within current project folder
}else if(key == "MUKEY"){
  LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
}else if(key == "KEY"){

}

ClassificationMap <- file.path(WatershedElements, landcovername) # adjusted/cropped classification map - must be named correctly
#DEM <- file.path(WatershedElements, "cropped_dem.tif") # clipped dem, elevations unaltered - must be named correctly
#DEM <- file.path(WatershedElements, "model_dem.tif") # modified dem, elevations unaltered - must be named correctly
# Initial soil conditions for model - a stacked map of soil characteristics including:
# Initial saturation, porosity, soil depth, hydraulic conductivity, etc.
# See initialSoilConditions.R for more details
initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
                        ClassificationMap = ClassificationMap,
                        DEM = model_dem,
                        ModelOutputs = ModelFolder,
                        key = key,
                        overwrite = overwrite
                        ) # Saves the modeled soil stack as raster brick

## Rainfall
##--------------------------------------

## 2b. Weather - Rain data
#eventDate <- "2012-07-15"
# source("Rainfall_Process.R")
# Read in the rainfall data from a saved file, normalize it, and create a
rain_file <- rainfallCreation(ModelFolder, WatershedElements, date = date, method = rainfall_method, overwrite = overwrite)
# Slight issue: will use saved rainfall data if present - does not check to see what type of data the rainfall is

## Discharge presence - obtain information for graphing
##---------------------

# source("Discharge_Process.R")
rain_discharge <- dischargeCreate(date = date, ModelFolder, WatershedElements, rain_file = rain_file, discharge = discharge)

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
## Pre-model checks
## ------------------------------------
## 3. Checks
# Don't run model if the files are not present

# Necessary elements for the model
SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
flowStack_file <- file.path(WatershedElements, "stack_flow.tif")
landCover_file <- file.path(WatershedElements, "landcover.tif")
slope_file <- file.path(WatershedElements, "model_slope.tif")
rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")
#rain_file <- file.path(ModelFolder, "Model-Rainfall.csv") #Uncheck for troubleshooting
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

# Necessary elements for the model
# SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
# flow_file <- file.path(WatershedElements, "stack_flow.tif")
# land_cover_clip_file <- file.path(WatershedElements, "landcover.tif")
# slope_file <- file.path(WatershedElements, "model_slope.tif")

#library(profvis)
# profvis({
# Soil Moisture Routing Model - writes
print("Beginning Model Run...")
gc()
# source("utils.R")
flowModel(SoilStack_file = SoilStack_file,
          flowStack_file = flowStack_file,
          landCover_file = landCover_file,
          slope_file = slope_file,
          rain_file = rain_file,
          ModelFolder = ModelFolder,
          time_step = time_step,
          simulation_length = simulation_length,
          write = write,
          rainfall_method = rainfall_method,
          impervious = impervious,
          gif = gif,
          restartModel = restartModel
          )
## Display - Ought to be modular
## ----------------------------------------------------------------------------

# source("utils.R")
# source("postAnalysis.R")

print(paste0("Creating graphics in ", ModelFolder))
# Path to stacked rasters
if(store){
  dischargeAnalysis(ModelFolder, WatershedElements, discharge = discharge, store = store)
}

#velocityStorage <- terra::rast(file.path(ModelFolder, "Velocities.tif"))
#subsurfaceStorage <- terra::rast(file.path(ModelFolder, "Soil_Moisture_percent.tif"))
#x_sections_path <- "gauge_waterholes.shp" # shapefile with points to measure discharge



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



# Surface GIF
##--------------------------------

# Functions for visualizations
#source("Plotting.R")
# Libraries
# library(ggplot2)
# library(viridisLite)
# library(gganimate)
# library(viridis)

print("Retrieving rainfall data for simulation")
if(gif){
  gifCreation(ModelFolder, rain_file, rainfall_method = rainfall_method, gif = gif, discharge = discharge)
}


print(paste0("End of script, thank you. You stuff is saved in ", ModelFolder))
return(list(rain_file))
}
