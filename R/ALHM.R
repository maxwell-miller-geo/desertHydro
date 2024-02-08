# Complete Model
# Wrap it up

#' Arid Model - Runs Spatially Distributed Hydrology Model
#'
#' @param date The date of rainfall event in form "YYYY-MM-DD"
#' @param ModelFolder Folder path to saved modeled outputs
#' @param time_step Time in minutes for algorithm step for model, will dynamically adjust
#' @param simulation_length Duration of model in minutes: NA = default entire event
#' @param WatershedElements Folder path where watershed specific elements are located.
#'  DEM, Computational boundary, and rainfall storage
#' @param mini T/F Optional for miniature version of water
#' @param rainfall_created Optional if rainfall event has been created
#' @param save Optional if True will save plots in Model Folder
#' @param All_save Optional if TRUE will save important variables
#' @param crop Optional: if TRUE watershed boundary shapefile will clip watershed elements
#' @param discharge Optional: if TRUE script expects recorded discharge data in watershed elements for comparisons
#'
#' @return Creates stacked rasters of surface depth, velocity, and soil mositure
#' @export
#'
#' @examples
#' ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Demo_Test)"
#' WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements)"
#' # Do Not RUN
#' arid_model(date, ModelFolder, time_step = 5, simulation_length = NA, WatershedElements = WatershedElements)
#'
arid_model <- function(date, ModelFolder,
                       time_step = .5,
                       simulation_length = NA,
                       WatershedElements = r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)",
                       mini = F,
                       rainfall_created = F,
                       save = T,
                       All_save = T,
                       crop = T,
                       discharge = T){

# ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Test_1)"
## Section 1
## 1. Load in necessary input data
# Libraries that are necessary
# libs <- c(
#   "tidyverse", "tidyterra", "sf",
#   "ggmap", "classInt", "gifski",
#   "gganimate", "reshape2", "tidyverse",
#    "readxl", "gridExtra",
#   "ggplot2", "zoo", "purrr",
#   "ggtext", "whitebox", "bookdown",
#   "terra", "viridis", "viridisLite")
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
#WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)" # folder to save watershed characteristics
#WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)" # mini version
# Adjust this folder of where to save the model run
#ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Taylor_Model)" # folder to save modeled outputs
#ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Test_1)"

if(!file.exists(ModelFolder)){ # Create Model folder, if it doesn't exist
  dir.create(ModelFolder)
}
# Each model folder should contain one rainfall event
#date <- "2007-07-23" # date of event - large event - 1062 cfs
#date <- "2015-10-19" # date of event - medium event - 110 cfs
#date <- "2018-08-01" # date of event - smaller event - 76 cfs - bad date
#date <- "2004-06-29" # cfs 124
# Options
rainfall_created <- F
rainfall_weighted <- TRUE
save = T
rain_spatial <- TRUE
spatial <- FALSE

##------------------------------------
# Preprocess
# Input files
source("WatershedSetUp.R")
# Files below are not needed for Example Script
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)" # path of dem extent
# land_cover_path <-  r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # land cover - unclipped file.
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)" # watershed boundary

dem_path <- file.path(WatershedElements, "waterholes_extent.tif") # path of dem extent
if(!crop){ # if it isn't cropped, it will adjust to look for the demo file.
  watershed_shape_path <-  NA
  dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements\demo_dem.tif)"
}else if(mini){
  watershed_shape_path <- file.path(WatershedElements,"mini_ws.shp") # watershed boundary ### IF NOT MINI
}else{
  watershed_shape_path <- file.path(WatershedElements,"waterholes_lc.shp") # watershed boundary ### IF NOT MINI
}

# Function adjusts digital elevation model (smooths with preserved features)and land cover map is projected in same coordinate system and clipped to watershed.
watershedElements(Outpath = WatershedElements, DEM = dem_path, WatershedShape = watershed_shape_path)

##--------------------------------
# Initial conditions
# Save initial conditions for a particular model.
source("initialConditions.R")
# Assign the models - DEM
# Smoothed DEM - can use the unaltered DEM or a filled/ breached model.
# It is not recommended to use the original DEM
model_dem <- file.path(WatershedElements, "model_dem.tif") # can adjust the input dem
#"C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\"
smooth_dem <- terra::rast(model_dem)

initial_conditions(ModelOutputs = ModelFolder, model_dem = model_dem) # saves initial conditions into model folder
##-----------------------------------
## Initial Soil conditions
source("initialSoilConditions.R")

LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx" # default excel file within current project folder
ClassificationMap <- file.path(WatershedElements, "landcover.tif") # adjusted/cropped classification map - must be named correctly
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
# Read in the rainfall data from a saved file, normalize it, and create a
#eventDate <- "2012-07-15"



if(!rainfall_created){
  # Rainfall with a time distribution
  # Read csv file and create a dataframe - assumes within model folder
  source("Rainfall_Process.R")
  source("Discharge_Process.R")

  if(!is.null(date)){
    if(discharge){ # Run if discharge data is present
      # Check if discharge present on day - returns the date or optional date
      date <- lubridate::date(discharge_present(WatershedElements, date))[1] # returns discharge date or next day (first entry)
      # Load in stream data from Waterholes - GCMRC
      discharge_path <- file.path(WatershedElements, "Waterholes_Stream_gcmrc20231127132459.tsv")
      # Calculate the daily discharge for given date
      dischargeDF <- dailyDischarge(discharge_file_path = discharge_path,
                                    discharge_date = date,
                                    save_location = ModelFolder,
                                    saveGraphs = T)
    }

    # Search the rainfall data and gather the rainfall recorded by the minute
    #rainDF2 <- rainfallTotalRain(WatershedElements, level = "minute", write = T)
    rainDF <- utils::read.csv("rain-data-minute.csv") # assumes rain is in R directory

    # Filter the rainfall for a given day
    rainFiltered <- rainfallForEvent(rainDF, date)

    # Spatial distributed rainfall - a little janky just returns rainfall- not rain-discharge
    if(rain_spatial){ # create table with time | Water-1 | Water-2 | Water-G
      cols <- c("WATER-1", "WATER-2", "WATER-G")
      spatial_rain <- rainFiltered |>
        select(Time_minute, cols) |> # Select relevant columns
        mutate(time = (as.numeric(rainFiltered$`Time_minute` - base::min(rainFiltered$`Time_minute`)
        ) / 60) + 1) |>
        dplyr::add_row(Time_minute = c(rainFiltered[1,1] - minutes(1)),
                       `WATER-1` = 0,
                       `WATER-2` = 0,
                       `WATER-G` = 0,
                       time = 0, .before = 1) |>
        select(time, cols) |>
        round(5)

      # Save the output
      rain_spatial_file <- file.path(ModelFolder, paste0(date, "-Spatial-Rainfall.csv"))

      write_csv(spatial_rain, rain_spatial_file)
    }


    # Weight the input rainfall
    if(rainfall_weighted){
      # Weights from voronoi calculations order is: WATER-1, WATER-2, WATER-G
      #weights <- c(.49254, .40299, .10448)
      weights <- c(.33333, .33333, .33333) # equally weighted
      rainFiltered$`WATER-1` <- rainFiltered$`WATER-1` * weights[1]
      rainFiltered$`WATER-2` <- rainFiltered$`WATER-2` * weights[2]
      rainFiltered$`WATER-G` <- rainFiltered$`WATER-G` * weights[3]

      # Readjust the total column
      rainFiltered$Total_in <- rowSums(rainFiltered[, c("WATER-1", "WATER-2", "WATER-G")])
    }

    if(discharge){
      # Combine the rainfall and discharge into a single .csv file
      rain_discharge <- rainfall_discharge_combine(rainfallDF = rainFiltered,
                                                   dischargeDF,
                                                   outpath = ModelFolder,
                                                   save = T)
    }else{
      rain_discharge <- rainFiltered |>
        mutate(time = (as.numeric(rainFiltered$`Time_minute` - base::min(rainFiltered$`Time_minute`)
      ) / 60) + 1) |>
        select(time, Total_in) |>
      dplyr::add_row(Total_in = 0,
                     time = 0, .before = 1) |>
        arrange(time)
      #return(rainFiltered)
    }


    # Calculate the number of observations
    observations <- nrow(rain_discharge)

    # Calculate the length of recorded rainfall-discharge - could be within script..
    duration <- max(rain_discharge$time) # minutes

    # Calculate the total rainfall
    total_rain <- sum(rain_discharge$Total_in)

    # # Save rainfall to model folder
    rainNormal <- rain_discharge |>
      select(time, Total_in) |>
      round(4) # round columns to 4 decimals


    rain_file <- file.path(ModelFolder, paste0(date, "-Rainfall.csv"))

    write_csv(rainNormal, rain_file)

    # Save plots
    if(save & discharge){
      plot_rainfall_discharge(rain_discharge, date = date, save = T, outpath = ModelFolder)
    }


    ## Daily distribution of rainfall - just rainfall
    # # Create a time and amount normalized rainfall
    # rainNormal <- rainfallDayDistribution(rainFiltered, write = F)

  }

  # rain_path <- file.path(ModelFolder, "Rainfall.csv")
  # rain <- read_csv(rain_path)
  # rain <- rainfallProcess(rain_path)
  # total_rain_duration <- max(rain$rain_duration) # gets the rainfall duration - assumes minutes
  # total_rain <- rainfallTotal(rain_path) # gets the total rainfall in units of file - assumes inches for now

}else{
  # Rainfall for a constant amount

  # Test case -
  # Rainfall for Waterholes 1 - 2015-10-19,0.12
  # Rainfall for waterholes 2 - 2015-10-19,0.37
  # Rainfall for waterholes G - 2015-10-19,0.51
  total_rain_duration <- 15
  #total_rain <- mean(c(.51, .12, .37))
  total_rain <- .51
  rain_step <- 0.1 # time interval of rainfall (minutes) - should be less than default model time step

  rain_duration <- seq(0, total_rain_duration, rain_step) # rainfall in minutes
  rainfall_amount_per_step <- (total_rain/total_rain_duration)*rain_step # amount of rainfall (rainfall/min) * (min)
  rainfall_rate <- c(0, rep(rainfall_amount_per_step, total_rain_duration/rain_step))
  rain <- cbind.data.frame(rain_duration, rainfall_rate) # Time(minutes) | Rain (in)
  # Create a csv rainfile
  rain_file <- file.path(ModelFolder, "model-rainfall.csv")
  write_csv(rain, rain_file)
}
## ------------------------------------
## Pre-model checks
## 3. Checks
# Don't run model if the files are not present

# Necessary elements for the model
SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
flowStack_file <- file.path(WatershedElements, "stack_flow.tif")
landCover_file <- file.path(WatershedElements, "landcover.tif")
slope_file <- file.path(WatershedElements, "model_slope.tif")

# Rainfall check
if(spatial){
  rain_file <- rain_spatial_file
} else{
  rain_file <- rain_file
}

files_needed <- c(WatershedElements, ModelFolder, landCover_file, SoilStack_file, flowStack_file, rain_file, slope_file)

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
##---------------------------
## Flow Model
# ## 4. Model Script
source("flowModel.R")
source("utils.R")
# Necessary elements for the model
# SoilStack_file <- file.path(ModelFolder, "model_soil_stack.tif")
# flow_file <- file.path(WatershedElements, "stack_flow.tif")
# land_cover_clip_file <- file.path(WatershedElements, "landcover.tif")
# slope_file <- file.path(WatershedElements, "model_slope.tif")

#library(profvis)
# profvis({
# Soil Moisture Routing Model - writes
print(paste0("time-step=",time_step))
flowModel(SoilStack_file = SoilStack_file,
          flowStack_file = flowStack_file,
          landCover_file = landCover_file,
          slope_file = slope_file,
          rain = rain_file,
          ModelOutputs = ModelFolder,
          time_step = time_step,
          simulation_length = simulation_length,
          write = T,
          spatialrain = F,
          impervious = F,
          All_save = All_save)
## -------------------------------------
## Display
source("utils.R")
source("postAnalysis.R")

print(paste0("time-step=",time_step))
# Path to Cross-sections
surfaceStorage <- terra::rast(file.path(ModelFolder, "Surface_Storage.tif"))
surfaceVelocity <- terra::rast(file.path(ModelFolder, "Velocities.tif"))
x_sections_path <- "gauge_waterholes.shp" # shapefile with points to measure discharge
if(file.exists(x_sections_path) & discharge){
  #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
  cross_section <- terra::vect(x_sections_path) # bring vector into R
  # # Extract the height from the surface stack
  surface_Height <- terra::extract(surfaceStorage, cross_section) # surface height in cm
  surface_velocity <- terra::extract(surfaceVelocity, cross_section) # velocity at given time (m/s)
  cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
  surface_discharge <- (as.numeric(surface_Height[,3:ncol(surface_Height)]* cm_to_m2) * surface_velocity[,3:ncol(surface_velocity)])
  xvalues <- as.numeric(colnames(surface_Height[,3:ncol(surface_Height)]))
  estimated <- data.frame(time = xvalues, predDis = as.numeric(as.vector(surface_discharge[1,])))
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

  # Combined discharges for comparisons
    compareDis <- compareDischarge(rain_discharge, estimated) # outputs: time|recDis|predDis

  dischargePlot <- ggplot() +
    geom_line(aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
    geom_line(aes(x = compareDis$time, y = compareDis$predDis, color = "Predicted")) +
    labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date), x = paste0("Time (minutes)"), y = "Discharge (ft^{3}/s)", color = "Legend")

  dischargePlot
} else if(file.exists(x_sections_path)){
  #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
  cross_section <- terra::vect(x_sections_path) # bring vector into R
  # # Extract the height from the surface stack
  surface_Height <- terra::extract(surfaceStorage, cross_section) # surface height in cm
  surface_velocity <- terra::extract(surfaceVelocity, cross_section) # velocity at given time (m/s)
  cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
  estimated <- (as.numeric(surface_Height[,3:ncol(surface_Height)] * cm_to_m2) * surface_velocity[,3:ncol(surface_velocity)])
  # Extract values?
  xvalues <- seq(0, simulation_length - time_step, by = time_step)
  dischargePlot <- ggplot() +
    #geom_line(aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
    geom_line(aes(x = xvalues, y = estimated, color = "Predicted")) +
    labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date), x = paste0("Time (minutes)"), y = "Discharge (ft^{3}/s)", color = "Legend")

  dischargePlot
}
# Save plot
ggsave(filename = file.path(ModelFolder, paste0("compare-discharge-",date,".png")), plot = dischargePlot, width = 6)
##--------------------------------
# Surface GIF
# Functions for visualizations
# source("Plotting.R")
# # Libraries
# library(ggplot2)
# library(viridisLite)
# library(gganimate)
# library(viridis)

if(TRUE){ # gathers total rain and rain duration values
  total_rain <- round(sum(rain_discharge$Total_in),3)
  total_rain_duration <- as.numeric((tail(rainFiltered$Time_minute, n = 1) - rainFiltered$Time_minute[1]))
}

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
# Save the animated GIF
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
subsurfaceStorage <- terra::rast(file.path(ModelFolder, "Soil_Moisture_percent.tif"))
meltedMoistureContent <- meltStack(subsurfaceStorage, timevalues = xvalues) # subsurface % fill through time
# Create an animated ggplot - Subsurface Storage
subsurface_plot <- animateStack(meltedMoistureContent,
                                title = "Moisture Content",
                                units = "% Full",
                                caption = paste0(total_rain," inches of rain over ",  round(total_rain_duration, 2)," minutes."))

# Display the animation
#gganimate::animate(subsurface_plot)
# Save the animated GIF
anim_save(filename = paste0(date, "-moisture-content.gif"), path = ModelFolder, animation = subsurface_plot, fps = 10, renderer = gifski_renderer())
## ------------------------------
# Load in the velocity storage
velocityStorage <- terra::rast(file.path(ModelFolder, "Velocities.tif"))
velocityStack <- meltStack(velocityStorage, timevalues = xvalues) # subsurface % fill through time
# Create an animated ggplot - Subsurface Storage
velocity_plot <- animateStack(velocityStack,
                              title = "Surface Velocities",
                              units = "m/s",
                              caption = paste0(total_rain," inches of rain over ", round(total_rain_duration, 2)," minutes."))
# Display the animation
#gganimate::animate(velocity_plot)
# Save the animated GIF
anim_save(filename = paste0(date,"-velocity.gif"), path = ModelFolder, animation = velocity_plot, fps = 10, renderer = gifski_renderer())
##
print(paste0("End of script, thank you. You stuff is saved in ", ModelFolder))

}
