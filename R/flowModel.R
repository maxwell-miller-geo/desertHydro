# Model Main
# The script takes in the necessary input parameters and performs flow routing
# The script writes the outputs of the script to the model folder
# Necessary Libraries
# libs <- c(
#   "tidyverse", "tidyterra", "sf", "readxl", "whitebox", "terra")
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

# Custom Functions necessary
#source("setup_LandCover.R") # Land Cover script
# If flow direction has already been calculated, then it can be loaded in there.
# source("setup_FlowPartition.R")
# # Weather functions
# source("Rainfall_Process.R")
# # Model functions
# source("Main_Script_Functions.R")
# # Digital Elevation Functions
# source("demProcessing.R")
# # Utility functions
# source("utils.R")

##  Model Script
#LandCoverCharacteristics <- storage_amount(LandCoverCharacteristics) # adjusts the storage amount must reassign values for table




flowModel <- function(SoilStack_file, flowStack_file, slope_file, landCover_file, rain, ModelOutputs, time_step = 1, simulation_length = 120, write = TRUE, spatialrain = F, impervious = F, All_save = T){
  print(paste0("time-step=",time_step))
  # Load in the Soil Stack and flowStack
  SoilStack <- terra::rast(SoilStack_file)
  # Attach slope to rast
  SoilStack$slope <- terra::rast(slope_file)
  flowStack <- terra::rast(flowStack_file)
  land_cover_raster <- terra::rast(landCover_file)

  # Check and determine the rainfall input
  if(!spatialrain){
    rain <- readr::read_csv(rain) # read in rain.csv file
    total_rain_duration <- max(rain$time)
    rain_step <- mean(diff(rain$time)) # find the average time step
  }

  gridsize <- 10 # manually set grid-size, based on DEM

  # Initialize storage variables
  # # Create variable to store subsurface soil storage through time
  subsurfaceStorage <- SoilStack$currentSoilStorage
  ## Create variable to store surface storage in through time
  surfaceStorage <- SoilStack$currentSoilStorage * 0
  # Create variable to store velocities in
  velocityStorage <- SoilStack$currentSoilStorage * 0
  # Create distance storage
  SoilStack$distStorage <- SoilStack$currentSoilStorage * 0

  # Stores the Amount of time elapse for each iteration
  timeStorage <- c(0)
  velocityFile <- c(0)

  # Stores shapefiles for velocities
  velocityMax_df <- dfMax(velocityStorage)

  # Important set-up variables
  simulation_duration <- seq(0, simulation_length, by = time_step) # minute duration of simulation
  saveRate <- time_step
  counter <- saveRate # counter to save rasters on certain intervals
  # Progress Bar
  progressBar <- txtProgressBar(min = 0, max = length(simulation_duration), style = 3)
  start <- Sys.time()

for(t in 2:length(simulation_duration)){

  setTxtProgressBar(progressBar, t)

  beginning_time <- simulation_duration[t-1]
  end_time <- simulation_duration[t]
  timeElapsed <- end_time - beginning_time # time elapsed in minutes
  simulationTimeSecs <- timeElapsed * 60 # time elapse in minutes * seconds

  ## [1] Rainfall
  ## - Calculates the amount of rainfall in a given time step
  if(simulation_duration[t] < total_rain_duration){ # could cut off rainfall if not careful
    rainfall_for_timestep <- cumulativeRain(rain, left = beginning_time, right = end_time)
    # timeIndex <- (seq(beginning_time, end_time, by = rain_step) / rain_step) + 1 # finds the index value for the rainfall
    # #timeSeq <- timeIndex[1]:timeIndex[2] # creates index sequence
    # rainfall_rate <- sum(as.vector(rain$Total_in[timeIndex])) # calculates sum of rainfall along time sequence
    # rain_for_timestep <- rainfall_rate * timeElapsed # in/minute * minute
  }else{
    rainfall_for_timestep <- 0
  }


  # Calculate rainfall for time-step
  in_to_cm <- 2.54
  total_rain_cm <- rainfall_for_timestep * in_to_cm
  # # Same amount of rain per time step
  # rainfall_for_step <- total_rain_cm / length(simulation_duration) # inches / number of iterations - every timestep
  #rainfall_for_step <- total_rain_cm / time_step # inches / min - every timestep
  # Create rainfall map
  SoilStack$current_rainfall <- ifel(is.finite(land_cover_raster), total_rain_cm, NA) # rainfall distribution map

  ## [2] Canopy
  # Evaluate canopy storage - (current-storage + rainfall)
  # SoilStack$current_canopy_storage <- SoilStack$maxCanopyStorageAmount
  # SoilStack$maxCanopyStorageAmount
  # Evaluate canopy
  # Calculate throughfall
  SoilStack$throughfall <- SoilStack$current_rainfall
  # Temporary set up - assuming all rainfall is throughfall
  # After the water has made its way through the canopy it is now throughfall

  ## [3] Subsurface - Surface
  # Subsurface Lateral Flow
  # Lateral flow is based on Darcy's Law, with gradient equal to land slope, and direction maps
  # Calculated from the elevation model - flow partitioned
  # Diversions and tile drains are assumed to effectively divert the lateral flow, so that it goes directly into the stream rather than downhill?? - Not in here
  # Calculate the amount of runoff - based on the difference between the throughfall and current soil storage amount

    if(t == 2){
      SoilStack$surfaceWater <- surfaceStorage
    }
  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  if(!impervious){
    # Adjustments
    #SoilStack$effectiveInfiltrationRate <- SoilStack$surfaceWater * .4 # Infiltration rate is 40% of surface water

  SoilStack$effectiveInfiltrationRate <- effectiveConductivity(SoilStack$surfaceWater, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepth, SoilStack$conductivityAtFieldCapacity) * simulationTimeSecs

  SoilStack$pondedWater <- ifel(((SoilStack$throughfall + SoilStack$surfaceWater) > SoilStack$effectiveInfiltrationRate),
                                (SoilStack$throughfall  + SoilStack$surfaceWater) - SoilStack$effectiveInfiltrationRate, 0) # if

  SoilStack$currentSoilStorage <- SoilStack$throughfall + SoilStack$currentSoilStorage + SoilStack$surfaceWater - SoilStack$pondedWater

  SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$pondedWater # water infiltrated

  # Adjust the soil storage - keeping it under the maximum soil storage amount
  # SoilStack$throughfall
  # SoilStack$maxSoilStorageAmount
  # SoilStack$currentSoilStorage
  # SoilStack$currentSoilStorage <- terra::ifel((SoilStack$throughfall + SoilStack$currentSoilStorage) > SoilStack$maxSoilStorageAmount, SoilStack$maxSoilStorageAmount)

  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  # Rate of m/s
  SoilStack$effectiveConductivity <- effectiveConductivity(SoilStack$currentSoilStorage, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepth, SoilStack$conductivityAtFieldCapacity)

  # [3b] Lateral flow
  ## The following command uses Darcy's law to rout lateral flow through different land use
  # Lateral flow minimum between Darcy's law and amount of water on the surface

  SoilStack$lateralFlow <- lateral_flow(SoilStack$effectiveConductivity, SoilStack$slope, SoilStack$soilDepth, SoilStack$storageAmount)
  # #sum(values(SoilStack$storageAmount), na.rm = TRUE)
  # # Adjust the storage amount in each cell from lateral flow
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - SoilStack$lateralFlow + flowRouting(SoilStack$lateralFlow, flowDirectionMap = flowStack_file) # pass flow file location to avoid pointer errors

  } else{
    SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall # water not infiltrated
  }

  # Calculates the current storage of the throughfall and current soil storage - adjust for rate of infiltration?
 #becomes surface water

  ## [4] Surface Runoff
  # Calculate the surface runoff for the water present at the surface.

  velocity <- ManningsWideChannelVelocity(SoilStack$mannings_n, SoilStack$surfaceWater, slope = SoilStack$slope, length = gridsize) #m/second
  maxVelocity <- minmax(velocity)[2] # max discharge for base time-step (m/s)
  # Save maximum velocity

  velocityMax_df <- rbind(velocityMax_df, dfMax(velocity))

  velocityFile <- append(velocityFile, maxVelocity)

  timestepSeconds <- timeElapsed * 60

  maxDistanceFlow <- maxVelocity * timeElapsed * 60 # farthest flow length

  if(maxDistanceFlow > gridsize){ # time step is to large - moves farther than 1 cell
    #ratio <- maxDistanceFlow / gridsize # how much further it travels
    adjustmentRatio <- ceiling(maxDistanceFlow / gridsize) # how much to change the time step by

    # Add to a list how much it adjusted by how much did it slow down.
    for(dt in 1:adjustmentRatio){
      #print("Decreased Timestep")
      timeStorage <- append(timeStorage, timestepSeconds/adjustmentRatio + tail(timeStorage, 1))

      percentLengthMoved <- percentLength(velocity, timestepSeconds, adjustmentRatio = adjustmentRatio) # calculate percent water moved
      # Add the distance storage
      SoilStack$distStorage <- SoilStack$distStorage + percentLengthMoved
      # Add water to runoff, it if can move 10m within a time-step, otherwise it stays put.
      cellsMove <- terra::ifel(SoilStack$distStorage > gridsize, 1, 0)  # Depths of water that will move

      # Multiple the runoff by which cells move
      runoffDepth <- cellsMove * SoilStack$surfaceWater
      # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
      SoilStack$surfaceWater <- SoilStack$surfaceWater - runoffDepth + flowRouting(runoffDepth, flowDirectionMap = flowStack_file) # pass flow file location to avoid
    }
  }else{
    timeStorage <- append(timeStorage, timestepSeconds + tail(timeStorage, 1))
    percentLengthMoved <- percentLength(velocity, timestepSeconds)
    # Add the distance storage
    SoilStack$distStorage <- SoilStack$distStorage + percentLengthMoved
    # Add water to runoff, it if can move 10m within a time-step, otherwise it stays put.
    cellsMove <- terra::ifel(SoilStack$distStorage > gridsize, 1, 0)  # Depths of water that will move
    # Multiple the runoff by which cells move
    runoffDepth <- cellsMove * SoilStack$surfaceWater
    #runoffDepth <- percentWaterMoved * SoilStack$surfaceWater # % * water depth (cm)
    # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
    SoilStack$surfaceWater <- SoilStack$surfaceWater - runoffDepth + flowRouting(runoffDepth, flowStack_file) # pass flow file location to avoid
  }



  #
  # lengthMoved <- velocity * timestepSeconds # m/s * seconds
  # percentMoved <- lengthMoved / gridsize # (m) /  (m)
  # runoffDepth <- percentMoved * (SoilStack$surfaceWater) # % * water depth (cm)
  # maxDepthMoved <- minmax(runoffDepth)[2] # Maximum calculate runoff volume

  # discharge <- velocity * (SoilStack$surfaceWater/100) * gridsize # (m/s) * water depth (cm) /100 * cell length
  # maxDischarge <- minmax(discharge)[2] # maximum discharge calculated

  # Remove the runoff from a given cell and portion that water up and add it to the cells downstream.
  # Assuming the cell sizes are the same - a % volume change is equivalent to a % depth change
  # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
  # SoilStack$surfaceWater <- SoilStack$surfaceWater - runoffDepth + flowRouting(runoffDepth, flowStack_file) # pass flow file location to avoid pointer errors

  #if(counter %% saveRate == 0){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(All_save | counter %% saveRate == 0 | t == length(simulation_duration)){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
    print(paste0("saved ", counter))
    subsurface_temp <- round(SoilStack$currentSoilStorage, 2)
    names(subsurface_temp) <- beginning_time
    subsurfaceStorage <- c(subsurfaceStorage, subsurface_temp)

    surfaceWater_temp <- round(SoilStack$surfaceWater, 2)
    names(surfaceWater_temp) <- beginning_time
    # Store the current surface water depth across the map. - Can be adjusted to add every nth iteration
    surfaceStorage <- c(surfaceStorage, surfaceWater_temp)

    velocity_temp <- round(velocity, 2)
    names(velocity_temp) <- beginning_time

    # Adjust the velocity storage
    velocityStorage <- c(velocityStorage, velocity_temp)
  }

  ### give a name to the current storage based upon iteration
  print(counter)
  counter <- counter + 1
}
print(paste("The model took: ", as.numeric(Sys.time() - start), "Seconds/Minutes/Hours"))

close(progressBar)

if(write){
  # Saved rasters
  writeRaster(surfaceStorage, filename = file.path(ModelOutputs, "Surface_Storage.tif"), overwrite = T)
  writeRaster(subsurfaceStorage, filename = file.path(ModelOutputs, "Soil_Moisture_percent.tif"), overwrite = T)
  writeRaster(velocityStorage, filename = file.path(ModelOutputs, "Velocities.tif"), overwrite = T)
  # Additional save data
  vectCreation(tail(velocityMax_df, -2), saveLoc = ModelOutputs, name = "max-depth.shp", coords = crs(SoilStack))  # Save the surface flow tif
  timeStorage <- as.data.frame(timeStorage)
  write_csv(timeStorage, file = file.path(ModelOutputs, "Time_steps_seconds.csv"))
  velocityFile <- as.data.frame(velocityFile)
  write_csv(velocityFile, file = file.path(ModelOutputs, "max_velocity_per_time.csv"))
}
# print("Got through it!")
#return(surfaceStorage)
}


# Test script
# Soil Stack
# ModelElements <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\UnchangingElements)" # Test location for outputs
# ModelOutputs <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_10-19-2015)"
# slope_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_slope.tif)"
# land_cover_clip_file <- file.path(ModelElements, "waterholes_lc.tif")
# soilstack_file <- file.path(ModelOutputs, "model_soilstack.tif")
# flowStack_file <- file.path(ModelElements, "stack_flow.tif") # must be created with names of layers
# rain_file <- file.path(ModelOutputs, "model-rainfall.csv") # must be created in set-up
# ModelOutputs <- ModelOutputs # Location to save the model output - likely the same as the inputfiles
# time_step <- 1
# simulation_length <- 180


# testflowModel <- flowModel(SoilStack_file = soilstack_file,
#                            flowStack_file = flowStack_file,
#                            landCover_file = land_cover_clip_file,
#                            slope_file = slope_file,
#                            rain = rain_file,
#                            ModelOutputs = ModelOutputs,
#                            time_step = time_step,
#                            simulation_length = simulation_length,
#                            write = F)


# Run multiple versions!
# model_files <- c(#r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_1_2in)",
#                  #r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_1in)",
#                 #r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_2in)",
#                 r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_3in)",
#                 r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_4in)"
#                 )

# for(x in model_files){
#   print(x)
#   rain_file <- file.path(x, "model-rainfall.csv") # must be created in set-up
#   soilstack_file <- file.path(x, "model_soilstack.tif")
#
#   ModelElements <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\UnchangingElements)" # Test location for outputs
#   slope_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_slope.tif)"
#   land_cover_clip_file <- file.path(ModelElements, "waterholes_lc.tif")
#   flowStack_file <- file.path(ModelElements, "stack_flow.tif") # must be created with names of layers
#   #rain_file <- file.path(ModelOutputs, "model-rainfall.csv") # must be created in set-up
#   time_step <- 5
#   simulation_length <- 90
#
#   flowModel(SoilStack_file = soilstack_file,
#             flowStack_file = flowStack_file,
#             landCover_file = land_cover_clip_file,
#             slope_file = slope_file,
#             rain = rain_file,
#             ModelOutputs = x,
#             time_step = time_step,
#             simulation_length = simulation_length,
#             write = T)
#
#   print(paste0("Finished with ", x, "model"))
# }

