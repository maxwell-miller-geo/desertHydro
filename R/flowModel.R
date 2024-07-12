# Model Main
# The script takes in the necessary input parameters and performs flow routing
# The script writes the outputs of the script to the model folder
# Necessary Libraries
# libs <- c(
#   "tidyverse", "tidyterra", "tidyverse", "dplyr", "readxl", "whitebox", "terra", "data.table")
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
# # If flow direction has already been calculated, then it can be loaded in there.
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

flowModel <- function(SoilStack_file,
                      flowStack_file,
                      rain_file,
                      ModelFolder,
                      time_step = 0.5,
                      simulation_length = 120,
                      store = TRUE,
                      rainfall_method = "gauges",
                      impervious = F,
                      gif = T,
                      restartModel = F,
                      ...){
  gc()
  print(paste("Time step:", time_step))
  print(paste("Simulation length:", simulation_length))
  print(paste("Estimated run time:", round(simulation_length/ time_step * 25/60), " minutes."))

  start_time <- Sys.time()
  # Load soil stack
  SoilStack <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
  flowStack <- terra::rast(file.path(ModelFolder, "stack_flow.tif"))

  gridsize <- 10 # manually set grid-size, based on DEM
  drainCells <- data.table::fread(file.path(ModelFolder, "drainCells.csv"))
  # terra::extract(SoilStack$surfaceWater, drainCells$cell[1])*length^2
  # SoilStack$surfaceWater[drainCells[1,2:3]]
  #keyCells <- getCellNumber(drainCells, SoilStack) # key cells list(empty, discharge)

  # Check and determine the rainfall input
  rainList <- loadRain(rain_file, rainfall_method = rainfall_method)
  rain <- rainList[[1]]
  total_rain_duration <- rainList[[2]]
  # Important set-up variables
  simulation_duration <- seq(0, simulation_length, by = time_step) # minute duration of simulation

  # Create data frame with time information: Simulation length | Time-step | Simulation time
  simulationDF <- data.frame(simlength = simulation_length, timestep = time_step, simtime = 0)

  # Temporary files - for restarts
  simulationProgress <- file.path(ModelFolder, "simulation-progress.csv") # time data
  tempStorage <- file.path(ModelFolder, "tempStorage.tif") # soils data

  #write.csv(simulationDF, file = simulationProgress)ra
  #readin <- read.csv(simulationProgress)
  if(file.exists(tempStorage) & file.exists(simulationProgress) & !restartModel){ # if model was cutoff during simulation - restart option
    print("Picking up where model left off...")
    # Determine the time it left off - model simulation and overwrite the last step recorded
    simulationDF <- data.table::fread(simulationProgress)
    lastSimulationTime <- simulationDF$simtime
    print(paste0("Starting off at ", lastSimulationTime))
    if(lastSimulationTime == simulation_length){
      stop(paste0("The last time of the simulation '", lastSimulationTime, "' equals the simulation length '", simulation_length,"' please
                  adjust the 'simulation_length' or restart the model from scratch by setting restartModel = TRUE." ))
    }
    simulation_duration <- seq(lastSimulationTime, simulation_length, by = simulationDF$timestep)
    timeCheck <- terra::rast(file.path(ModelFolder, "VelocityStorage.tif"))
    lastTime <- as.numeric(terra::tail(names(timeCheck), n = 1))
    # Reado previous step? What if there are partial saves - doesn't check for
    SoilStack <- terra::rast(tempStorage)
    subsurfacePath <- file.path(ModelFolder, "soilStorage.tif")
    #surfacePath <- file.path(ModelFolder, "surfaceStorage.tif")
    velocityPath <- file.path(ModelFolder, "velocityStorage.tif")
    distancePath <- file.path(ModelFolder, "distanceStorage.tif")
    # runoffDepthPath <- file.path(ModelFolder, "runoffDepth.tif")
    flowMapPath <- file.path(ModelFolder, "flowMap.tif")
    # drainMapPath <- file.path(ModelFolder, "drainMap.tif")
    #velocityMax_df <- as.data.frame(terra::vect(file.path(ModelFolder, "max-velocity.shp")))
    #depthMaxDF <- as.data.frame(terra::vect(file.path(ModelFolder, "max-depth.shp")))
    timeVelocity <- data.table::data.table(data.table::fread(file.path(ModelFolder, "time-velocity.csv")))
    volumeOut <- data.table::fread(file.path(ModelFolder, "volumeOut.csv"))
    volumeIn <- data.table::fread(file.path(ModelFolder, "volumeIn.csv"))

  }
  else{
    volumeOut <- data.table::data.table(timestep = 0, volume = 0, discharge = 0)
    data.table::fwrite(volumeOut, file.path(ModelFolder, "volumeOut.csv"))
    volumeIn <- data.table::data.table(timestep = 0, total_rain_cm = 0) # Sum all of the rainfall during a rainfall event
    data.table::fwrite(volumeIn, file.path(ModelFolder, "volumeIn.csv"))
    # Create initial rasters for outputs
    subsurfacePath <- initializeRaster(SoilStack$currentSoilStorage, "soilStorage", ModelFolder)
    surfacePath <- initializeRaster(SoilStack$currentSoilStorage*0, "surfaceStorage", ModelFolder)
    velocityPath <- initializeRaster(SoilStack$currentSoilStorage*0, "velocityStorage", ModelFolder)

    # Create distance storage
    SoilStack$distStorage <- SoilStack$currentSoilStorage * 0 # Creates new layer
    distancePath <- initializeRaster(SoilStack$distStorage, "distanceStorage", ModelFolder)

    # Create runoff Depth before loop
    runoffDepth <- SoilStack$distStorage
    runoffDepthPath <- initializeRaster(SoilStack$currentSoilStorage*0, "runoffDepth", ModelFolder)

    # Empty flow map
    flowMapPath <- initializeRaster(SoilStack$distStorage, "flowMap", ModelFolder)
    # Empty Drain Map
    drainMapPath <- initializeRaster(SoilStack$mannings_n*0, "drainMap", ModelFolder)
    #SoilStack$flowMap <- SoilStack$currentSoilStorage * 0

    # Stores the Amount of time elapse for each iteration
    timeVelocity <- data.frame(time = 0, velocity = 0)

    # Stores shapefiles for velocities
    velocityMax_df <- dfMax(terra::rast(file.path(ModelFolder, "velocityStorage.tif")), rename = 0)
    # Stores points for maximum surface water depths
    #depthMaxDF <- dfMax(terra::rast(file.path(ModelFolder, "surfaceStorage.tif")), rename = 0)
  }

  saveRate <- time_step
  counter <- saveRate # counter to save rasters on certain intervals

  # Progress Bar
  progressBar <- utils::txtProgressBar(min = 0, max = length(simulation_duration), style = 3)
  start <- Sys.time()


for(t in 1:(length(simulation_duration)-1)){

  utils::setTxtProgressBar(progressBar, t)

  beginning_time <- simulation_duration[t]
  end_time <- simulation_duration[t+1]
  timeElapsed <- end_time - beginning_time # time elapsed in minutes
  simulationTimeSecs <- timeElapsed * 60 # time elapse in minutes * seconds
  ## [1] Rainfall
  ## - Calculates the amount of rainfall in a given time step
  if(simulation_duration[t] < total_rain_duration){ # could cut off rainfall if not careful
   rainfall_for_timestep <- rainfallAccum(rain, beginning_time, end_time, rainfall_method = rainfall_method, ModelFolder = ModelFolder)
   if(rainfall_method == "goes" & inherits(rainfall_for_timestep, "SpatRaster")){
     if(terra::ext(rainfall_for_timestep) != terra::ext(SoilStack)){
       rainfall_for_timestep <- terra::crop(rainfall_for_timestep, terra::ext(SoilStack))
     }
   }
  }else{
    rainfall_for_timestep <- 0
  }


  # Calculate rainfall for time-step
  in_to_cm <- 2.54
  total_rain_cm <- rainfall_for_timestep * in_to_cm
  # # Same amount of rain per time step
  # Check rainfall extent

  SoilStack$current_rainfall <- terra::ifel(is.finite(SoilStack$model_dem), total_rain_cm, NA) # rainfall distribution map

  # Volume calculations
  totalDepthCM <- sum(terra::values(SoilStack$current_rainfall), na.rm = T)
  area <- terra::expanse(SoilStack$current_rainfall, unit = "m")[[2]]
  volumeM3 <- totalDepthCM/100 * area
  volumeIn <- rbind(volumeIn,
                    list(end_time, volumeM3))
  data.table::fwrite(volumeIn, file.path(ModelFolder, "volumeIn.csv"))
  #print(SoilStack$current_rainfall)
  ## [2] Canopy
  # Evaluate canopy storage - (current-storage + rainfall)
  # SoilStack$current_canopy_storage <- SoilStack$maxCanopyStorageAmount
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
  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  if(!impervious){
    # Adjustments
    #SoilStack$effectiveInfiltrationRate <- SoilStack$surfaceWater * .4 # Infiltration rate is 40% of surface water
    # Infiltration rate
    soilAdjustments <- infiltration(SoilStack, simulationTimeSecs)
    SoilStack$surfaceWater <- soilAdjustments[[1]]
    SoilStack$currentSoilStorage <- soilAdjustments[[2]]

    # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
    # Rate of m/s
    SoilStack$currentSoilStorage <- subsurfaceFlow(SoilStack, simulationTimeSecs, flowStack_file)

  } else{
    #SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall # water not infiltrated
  }

  # Calculates the current storage of the throughfall and current soil storage - adjust for rate of infiltration?
 #becomes surface water

  ## [4] Surface Runoff
  # Calculate the surface runoff for the water present at the surface.
  runoffList <- routeWater2(ModelFolder,
                            SoilStack,
                           flowDirectionMap = flowStack_file,
                           time_step = simulationTimeSecs,
                           length = gridsize,
                           timeVelocity = timeVelocity,
                           drainCells = drainCells,
                           end_time = end_time)
  #print(runoffList)
  #print(paste0("runoff",runoffList))
  SoilStack$surfaceWater <- runoffList[[1]]
  #print(SoilStack$surfaceWater)
  velocity <- SoilStack$velocity <- runoffList[[2]]
  #print(terra::minmax(velocity)[2])
  #terra::plot(velocity)
  SoilStack$slope <- runoffList[[3]]
  flowStack <- runoffList[[4]]
  #print(runoffList)
  volumeDrained <- sum(unlist(runoffList[[5]]))
  #print(volumeDrained)
  dischargeCalc <- volumeDrained/simulationTimeSecs
  volumeOut <- rbind(volumeOut, list(end_time, volumeDrained, dischargeCalc))
  data.table::fwrite(volumeOut, file.path(ModelFolder, "volumeOut.csv"))
  #terra::plot(velocity)
  # maxVelocity <- terra::minmax(velocity)[2] # max discharge for base time-step (m/s)
  #
  # # Save maximum velocity
  # velocityMax_df <- rbind(velocityMax_df, dfMax(velocity, rename = end_time))
  # depthMaxDF <- rbind(depthMaxDF, dfMax(SoilStack$surfaceWater, rename = end_time))
  #
  # maxDistanceFlowed <- maxVelocity * simulationTimeSecs # farthest flow length - meters
  #
  # if(maxDistanceFlowed > gridsize){ # time step is to large - moves farther than 1 cell
  #   adjustmentRatio <- ceiling(maxDistanceFlowed / gridsize) # how much to change the time step by
  #   # Add to a list how much it adjusted by how much did it slow down.
  #   shortTime <- (simulationTimeSecs/ adjustmentRatio) / 60
  #   for(dt in 1:adjustmentRatio){
  #     #print("Decreased Timestep")
  #     timeVelocity <- rbind(timeVelocity, list((beginning_time + dt*shortTime), maxVelocity))
  #     #timeStorage <- append(timeStorage, simulationTimeSecs/adjustmentRatio + tail(timeStorage, 1))
  #     percentLengthMoved <- percentLength(velocity, simulationTimeSecs, adjustmentRatio = adjustmentRatio) # calculate percent water moved
  #     # Add the distance storage
  #     waterMovement(SoilStack$surfaceWater, percentLengthMoved, ModelFolder)
  #     #runoffDepth <- movementList[[1]]
  #     runoffDepth <- terra::rast(runoffDepthPath)
  #     #adjustments <- flowRouting(runoffDepthPath, flowStack_file) - terra::rast(runoffDepthPath)
  #     SoilStack$surfaceWater <- SoilStack$surfaceWater - terra::rast(runoffDepthPath) + flowRouting(runoffDepthPath, flowStack_file)
  #
  #   }
  # }else{
  #   timeVelocity <- rbind(timeVelocity, list(end_time, maxVelocity)) # Add time velocity
  #   percentLengthMoved <- percentLength(velocity, simulationTimeSecs) # returns percentage of cell distance
  #   # Add the distance storage
  #   waterMovement(SoilStack$surfaceWater, percentLengthMoved, ModelFolder)
  #   runoffDepth <- terra::rast(runoffDepthPath)
  #   # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
  #   SoilStack$surfaceWater <- SoilStack$surfaceWater - terra::rast(runoffDepthPath) + flowRouting(runoffDepthPath, flowStack_file)
  # }
  ##---------------- Save step-------------
  #if(counter %% saveRate == 0){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(TRUE | gif | counter %% saveRate == 0 | t == length(simulation_duration)){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep

    # writeLayers to disk space
    # writeLayer(subsurfacePath, round(SoilStack$currentSoilStorage, 3), layername = end_time)
    # writeLayer(surfacePath, round(SoilStack$surfaceWater, 3), layername = end_time)
    # writeLayer(velocityPath, round(velocity, 3), layername = end_time)



    # Write single rasters for important features
    if(t == 1 | end_time %% 0.5 == 0){
    if(!impervious){
      soil <- "soil"
      rasterWrite(round(SoilStack$currentSoilStorage,3), ModelFolder, end_time, layername = soil)
    }
    #plot(SoilStack$surfaceWater)
    surface <- "surface"
    rasterWrite(round(SoilStack$surfaceWater,3), ModelFolder, end_time, layername = surface)
    velocityName <- "velocity"
    rasterWrite(round(velocity,3), ModelFolder, end_time, layername = velocityName)
    }
    # Shapefile saves
    # vectCreation(velocityMax_df, saveLoc = ModelFolder, name = "max-velocity.shp", coords = terra::crs(SoilStack))
    # vectCreation(depthMaxDF, saveLoc = ModelFolder, name = "max-depth.shp", coords = terra::crs(SoilStack))

    # CSV saves
    # Adjust time storage file
    simulationDF$simtime <- end_time
    data.table::fwrite(simulationDF, simulationProgress) # write current simulation time
    #data.table::fwrite(data.table::data.table(tempvelocityFile), file = file.path(ModelFolder, "max_velocity_per_time.csv"))
    data.table::fwrite(timeVelocity, file = file.path(ModelFolder, "time-velocity.csv"))
    # Save a temporary version of the soil stack
    temporary <- SoilStack + 0 # create temporary Soil Stack

    terra::writeRaster(temporary, filename = tempStorage, overwrite = T)
    terra::writeRaster(flowStack, file.path(ModelFolder, "AdjustedFlowMaps.tif"), overwrite = T)

  }
 ##------------------------------------##
  ### give a name to the current storage based upon iteration
  #print(counter)
  counter <- counter + 1


}
print(paste("The model took: ", paste0(difftime(Sys.time(), start_time))))

close(progressBar)

  # Saved rasters
  # terra::writeRaster(surfaceStorage, filename = file.path(ModelFolder, "Surface_Storage.tif"), overwrite = T)
  # terra::writeRaster(subsurfaceStorage, filename = file.path(ModelFolder, "Soil_Moisture_percent.tif"), overwrite = T)
  # terra::writeRaster(velocityStorage, filename = file.path(ModelFolder, "Velocities.tif"), overwrite = T)
  # Additional save data

  model_complete <- "Model Complete"
  utils::write.table(model_complete, file = file.path(ModelFolder, "ModelComplete.txt"))
  #file.remove(tempStorage)

  if(!impervious){
    rasterCompile(ModelFolder, "soil", remove = T)
  }
  rasterCompile(ModelFolder, "surface", remove = T)
  rasterCompile(ModelFolder, "velocity", remove = T)

# Save simulation time as text
end_time <- Sys.time()
duration <- difftime(end_time, start_time)
out_duration <- paste("Simulation took", round(duration[[1]], 2),  units(duration), "to run.")
writeLines(out_duration, file.path(ModelFolder, "simulation_time.txt"))
# print("Got through it!")
#return(surfaceStorage)
}
