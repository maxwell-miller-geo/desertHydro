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




flowModel <- function(SoilStack_file,
                      flowStack_file,
                      slope_file,
                      landCover_file,
                      rain,
                      ModelFolder,
                      time_step = 1,
                      simulation_length = 120,
                      store = TRUE,
                      rainfall_method = "gauges",
                      impervious = F,
                      gif = T,
                      restartModel = F,
                      ...){

  print(paste("Time step:", time_step))
  print(paste("Simulation length:", simulation_length))
  print(paste("Estimated run time:", round(simulation_length/ time_step * 25/60), " minutes."))

  SoilStack <- terra::rast(SoilStack_file)
  # Attach slope to rast
  SoilStack$slope <- terra::rast(slope_file)
  flowStack <- terra::rast(flowStack_file)
  land_cover_raster <- terra::rast(landCover_file)
  gridsize <- 10 # manually set grid-size, based on DEM

  # Check and determine the rainfall input
  if(rainfall_method == "spatial"){
    rain <- readr::read_csv(rain, show_col_types = F) # read in rain.csv file
    total_rain_duration <- max(rain$time)
    #rain_step <- mean(diff(rain$time)) # find the average time step
  } else{
    rain <- readr::read_csv(rain, show_col_types = F) # read in rain.csv file
    total_rain_duration <- max(rain$time)
    #rain_step <- mean(diff(rain$time))
  }

  # Important set-up variables
  simulation_duration <- seq(0, simulation_length, by = time_step) # minute duration of simulation

  # Create data frame with time information: Simulation length | Time-step | Simulation time
  simulationDF <- data.frame(simlength = simulation_length, timestep = time_step, simtime = 0)

  # Temporary files - for restarts
  simulationProgress <- file.path(ModelFolder, "simulation-progress.csv") # time data
  tempStorage <- file.path(ModelFolder, "tempStorage.tif") # soils data

  #write.csv(simulationDF, file = simulationProgress)
  #readin <- read.csv(simulationProgress)
  if(file.exists(tempStorage) & file.exists(simulationProgress) & !restartModel){ # if model was cutoff during simulation - restart option
    print("Picking up where model left off...")
    # Determine the time it left off - model simulation and overwrite the last step recorded
    simulationDF <- data.table::fread(simulationProgress)
    lastSimulationTime <- simulationDF$simtime
    print(paste0("Starting off at ", lastSimulationTime))
    simulation_duration <- seq(lastSimulationTime, simulationDF$simlength, by = simulationDF$timestep) # minute duration of simulation
    timeCheck <- terra::rast(file.path(ModelFolder, "VelocityStorage.tif"))
    lastTime <- as.numeric(tail(names(timeCheck), n = 1))
    # Reado previous step? What if there are partial saves - doesn't check for
    SoilStack <- terra::rast(tempStorage)
    subsurfacePath <- file.path(ModelFolder, "subsurfaceStorage.tif")
    surfacePath <- file.path(ModelFolder, "SurfaceStorage.tif")
    velocityPath <- file.path(ModelFolder, "VelocityStorage.tif")
    distancePath <- file.path(ModelFolder, "distanceStorage.tif")
    runoffDepthPath <- file.path(ModelFolder, "runoffDepth.tif")
    flowMapPath <- file.path(ModelFolder, "flowMap.tif")
    drainMapPath <- file.path(ModelFolder, "drainMap.tif")
    velocityMax_df <- as.data.frame(terra::vect(file.path(ModelFolder, "max-velocity.shp")))
    timeVelocity <- data.table::data.table(fread(file.path(ModelFolder, "time-velocity.csv")))
    # timeStorage <- data.table::fread(file.path(ModelFolder, "Time_steps_seconds.csv"))
    # velocityFile <- data.table::fread(file.path(ModelFolder, "max_velocity_per_time.csv"))
    # Other storage items
  }else{
    # Create initial rasters for outputs
    subsurfacePath <- initializeRaster(SoilStack$currentSoilStorage, "subsurfaceStorage", ModelFolder)
    surfacePath <- initializeRaster(SoilStack$currentSoilStorage*0, "SurfaceStorage", ModelFolder)
    velocityPath <- initializeRaster(SoilStack$currentSoilStorage*0, "VelocityStorage", ModelFolder)

    # Create distance storage
    SoilStack$distStorage <- SoilStack$currentSoilStorage * 0 # Creates new layer
    distancePath <- initializeRaster(SoilStack$distStorage, "distanceStorage", ModelFolder)

    # Create runoff Depth before loop
    runoffDepth <- SoilStack$distStorage
    runoffDepthPath <- initializeRaster(SoilStack$distStorage, "runoffDepth", ModelFolder)

    # Empty flow map
    flowMapPath <- initializeRaster(SoilStack$distStorage, "flowMap", ModelFolder)
    # Empty Drain Map
    drainMapPath <- initializeRaster(SoilStack$mannings_n*0, "drainMap", ModelFolder)
    #SoilStack$flowMap <- SoilStack$currentSoilStorage * 0

    # Stores the Amount of time elapse for each iteration
    timeVelocity <- data.frame(time = 0, velocity = 0)

    # Stores shapefiles for velocities
    velocityMax_df <- dfMax(terra::rast(file.path(ModelFolder, "VelocityStorage.tif")), rename = 0)
  }

  saveRate <- time_step
  counter <- saveRate # counter to save rasters on certain intervals

  # Progress Bar
  progressBar <- txtProgressBar(min = 0, max = length(simulation_duration), style = 3)
  start <- Sys.time()

  gc()
for(t in 1:(length(simulation_duration)-1)){

  setTxtProgressBar(progressBar, t)

  beginning_time <- simulation_duration[t]
  end_time <- simulation_duration[t+1]
  timeElapsed <- end_time - beginning_time # time elapsed in minutes
  simulationTimeSecs <- timeElapsed * 60 # time elapse in minutes * seconds

  ## [1] Rainfall
  ## - Calculates the amount of rainfall in a given time step
  if(simulation_duration[t] < total_rain_duration){ # could cut off rainfall if not careful
    if(rainfall_method == "spatial"){
      # Get rainfall from shape
      rainForGauges <- cumulativeRain(rain, left = beginning_time, right = end_time, spatial = T)
      # Could adjust voronoi
      rainfall_for_timestep <- rasterizeRainfall(voronoi_shape = "Example/WatershedElements/waterholes_voronoi.shp",
                                                 rainAtGauges = rainForGauges,
                                                 rainfallRaster = terra::rast(file.path(WatershedElements, "model_dem.tif")))

      # Need the Voronoi shape
      # Need to make sure it is referenced properly
      # assign values to table

    }else{
      rainfall_for_timestep <- cumulativeRain(rain, left = beginning_time, right = end_time)
    }

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
  SoilStack$current_rainfall <- terra::ifel(is.finite(land_cover_raster), total_rain_cm, NA) # rainfall distribution map
  # Create rainfall map
  # if(!rain_spatial){
  #   SoilStack$current_rainfall <- terra::ifel(is.finite(land_cover_raster), total_rain_cm, NA) # rainfall distribution map
  # }else{
  # if(class(total_rain_cm) == "SpatRaster"){
  #   SoilStack$current_rainfall <- total_rain_cm  # rainfall distribution map
  # }else{
  #   SoilStack$current_rainfall <- terra::ifel(is.finite(land_cover_raster), total_rain_cm, NA) # rainfall distribution map
  # }


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

    if(t == 1){
      SoilStack$surfaceWater <- terra::rast(file.path(ModelFolder, "SurfaceStorage.tif"))
      SoilStack$surfaceWater <- SoilStack$soilDepthCM*0
      # flowMap <- surfaceStorage
      # names(flowMap) <- "flowMap"
    }
  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  if(!impervious){
    # Adjustments
    #SoilStack$effectiveInfiltrationRate <- SoilStack$surfaceWater * .4 # Infiltration rate is 40% of surface water

  SoilStack$effectiveInfiltrationRate <- effectiveConductivity(SoilStack$surfaceWater, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepthCM, SoilStack$conductivityAtFieldCapacity) * simulationTimeSecs

  SoilStack$pondedWater <- terra::ifel(((SoilStack$throughfall + SoilStack$surfaceWater) > SoilStack$effectiveInfiltrationRate),
                                (SoilStack$throughfall  + SoilStack$surfaceWater) - SoilStack$effectiveInfiltrationRate, 0) # if

  # Calculate the soil storage after infiltration rate is calculated
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage + SoilStack$surfaceWater + SoilStack$throughfall - SoilStack$pondedWater

  # Calculate the excess moisture in soil as saturation excess
  throughflow <- terra::ifel(SoilStack$currentSoilStorage > SoilStack$maxSoilStorageAmount,
                             SoilStack$currentSoilStorage - SoilStack$maxSoilStorageAmount, 0)
  # Calculate the surface depth of water
  SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$pondedWater + throughflow # water stored on surface

  # Recalculate the soil storage
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - throughflow

  # Adjust the soil storage - keeping it under the maximum soil storage amount
  # SoilStack$throughfall
  # SoilStack$maxSoilStorageAmount
  # SoilStack$currentSoilStorage
  # SoilStack$currentSoilStorage <- terra::ifel((SoilStack$throughfall + SoilStack$currentSoilStorage) > SoilStack$maxSoilStorageAmount, SoilStack$maxSoilStorageAmount)

  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  # Rate of m/s
  SoilStack$effectiveConductivity <- effectiveConductivity(SoilStack$currentSoilStorage, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepthCM, SoilStack$conductivityAtFieldCapacity)

  # [3b] Lateral flow
  ## The following command uses Darcy's law to rout lateral flow through different land use
  # Lateral flow minimum between Darcy's law and amount of water on the surface

  lateralflow <- lateral_flow(SoilStack$effectiveConductivity, SoilStack$slope, SoilStack$soilDepthCM, SoilStack$storageAmount)
  # #sum(values(SoilStack$storageAmount), na.rm = TRUE)
  # # Adjust the storage amount in each cell from lateral flow
  #return(list(lateralflow, flowStack_file))
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - lateralflow + flowRouting(lateralflow, flowDirectionMap = flowStack_file) # pass flow file location to avoid pointer errors
  gc()
  } else{
    SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall # water not infiltrated
  }

  # Calculates the current storage of the throughfall and current soil storage - adjust for rate of infiltration?
 #becomes surface water

  ## [4] Surface Runoff
  # Calculate the surface runoff for the water present at the surface.

  velocity <- ManningsWideChannelVelocity(SoilStack$mannings_n, SoilStack$surfaceWater, slope = SoilStack$slope, length = gridsize) #m/second
  #velocity <- ManningsWideChannelVelocity(SoilStack$mannings_n, surfaceStorage[[8]], SoilStack$slope, length = 10)
  maxVelocity <- minmax(velocity)[2] # max discharge for base time-step (m/s)
  # plot(SoilStack$mannings_n)
  # plot(SoilStack$surfaceWater)
  # plot(SoilStack$slope)
  # Save maximum velocity

  velocityMax_df <- rbind(velocityMax_df, dfMax(velocity, rename = end_time))

  #velocityFile <- append(velocityFile, maxVelocity)

  print(gridsize)
  # print(simulationTimeSecs)
  #print(maxVelocity)
  maxDistanceFlowed <- maxVelocity * simulationTimeSecs # farthest flow length - meters
  print(maxDistanceFlowed)
  #print(maxDistanceFlowed)
  if(maxDistanceFlowed > gridsize){ # time step is to large - moves farther than 1 cell
    #ratio <- maxDistanceFlow / gridsize # how much further it travels
    adjustmentRatio <- ceiling(maxDistanceFlowed / gridsize) # how much to change the time step by
    #print(paste0("adjustment ratio: ", adjustmentRatio))
    # Add to a list how much it adjusted by how much did it slow down.
    for(dt in 1:adjustmentRatio){
      #print("Decreased Timestep")
      gc()
      timeVelocity <- rbind(timeVelocity, list((beginning_time + dt*simulationTimeSecs / time_step), maxVelocity))
      #timeStorage <- append(timeStorage, simulationTimeSecs/adjustmentRatio + tail(timeStorage, 1))
      percentLengthMoved <- percentLength(velocity, simulationTimeSecs, adjustmentRatio = adjustmentRatio) # calculate percent water moved
      # Add the distance storage
      movementList <- waterMovement(SoilStack$surfaceWater, percentLengthMoved, ModelFolder)
      #runoffDepth <- movementList[[1]]
      runoffDepth <- terra::rast(runoffDepthPath)
      #adjustments <- flowRouting(runoffDepthPath, flowStack_file) - terra::rast(runoffDepthPath)
      SoilStack$surfaceWater <- SoilStack$surfaceWater - terra::rast(runoffDepthPath) + flowRouting(runoffDepthPath, flowStack_file)   # pass flow file location to avoid

    }
  }else{
    #timeStorage <- append(timeStorage, simulationTimeSecs + tail(timeStorage, 1))
    timeVelocity <- rbind(timeVelocity, list(end_time, maxVelocity)) # Add time velocity
    percentLengthMoved <- percentLength(velocity, simulationTimeSecs) # returns percentage of cell distance
    # Add the distance storage
    movementList <- waterMovement(SoilStack$surfaceWater, percentLengthMoved, ModelFolder)
    #movementList <- waterMovement(SoilStack$distStorage, percentLengthMoved, SoilStack$surfaceWater, flowMap = SoilStack$flowMap)
    runoffDepth <- terra::rast(runoffDepthPath)
    # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
    SoilStack$surfaceWater <- SoilStack$surfaceWater - terra::rast(runoffDepthPath) + flowRouting(runoffDepthPath, flowStack_file)   # pass flow file location to avoid
  }
  gc()
  ##---------------- Save step-------------
  #if(counter %% saveRate == 0){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(gif | counter %% saveRate == 0 | t == length(simulation_duration) | store){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep


    # Let's try writing everything to diskspace
    #print(paste0("saved ", counter))
    writeLayer(subsurfacePath, round(SoilStack$currentSoilStorage, 3), layername = end_time)

    writeLayer(surfacePath, round(SoilStack$surfaceWater, 3), layername = end_time)

    writeLayer(velocityPath, round(velocity, 3), layername = end_time)


    # Shapefile saves
    vectCreation(velocityMax_df, saveLoc = ModelFolder, name = "max-velocity.shp", coords = crs(SoilStack))

    # CSV saves
    # Adjust time storage file
    simulationDF$simtime <- end_time
    data.table::fwrite(simulationDF, simulationProgress) # write current simulation time

    # data.table::fwrite(data.table::data.table(timeStorage), file = file.path(ModelFolder, "Time_steps_seconds.csv"))
    #
    # tempvelocityFile <- as.data.frame(velocityFile)
    # data.table::fwrite(data.table::data.table(tempvelocityFile), file = file.path(ModelFolder, "max_velocity_per_time.csv"))
    data.table::fwrite(data.table::data.table(timeVelocity), file = file.path(ModelFolder, "time-velocity.csv"))
    # Save a temporary version of the soil stack
    temporary <- SoilStack + 0 # create temporary Soil Stack
    writeRaster(temporary, filename = tempStorage, overwrite = T)

  }
 ##------------------------------------##
  ### give a name to the current storage based upon iteration
  #print(counter)
  counter <- counter + 1
}
print(paste("The model took: ", as.numeric(Sys.time() - start), "Seconds/Minutes/Hours"))

close(progressBar)

if(store){
  # Saved rasters
  # terra::writeRaster(surfaceStorage, filename = file.path(ModelFolder, "Surface_Storage.tif"), overwrite = T)
  # terra::writeRaster(subsurfaceStorage, filename = file.path(ModelFolder, "Soil_Moisture_percent.tif"), overwrite = T)
  # terra::writeRaster(velocityStorage, filename = file.path(ModelFolder, "Velocities.tif"), overwrite = T)
  # Additional save data


  model_complete <- "Model Complete"
  write.table(model_complete, file = file.path(ModelFolder, "ModelComplete.txt"))
  #file.remove(tempStorage)
}

# print("Got through it!")
#return(surfaceStorage)
}


# Test script
# Soil Stack
# ModelElements <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\UnchangingElements)" # Test location for outputs
# ModelFolder <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Model_10-19-2015)"
# slope_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_slope.tif)"
# land_cover_clip_file <- file.path(ModelElements, "waterholes_lc.tif")
# soilstack_file <- file.path(ModelFolder, "model_soilstack.tif")
# flowStack_file <- file.path(ModelElements, "stack_flow.tif") # must be created with names of layers
# rain_file <- file.path(ModelFolder, "model-rainfall.csv") # must be created in set-up
# ModelFolder <- ModelFolder # Location to save the model output - likely the same as the inputfiles
# time_step <- 1
# simulation_length <- 180


# testflowModel <- flowModel(SoilStack_file = soilstack_file,
#                            flowStack_file = flowStack_file,
#                            landCover_file = land_cover_clip_file,
#                            slope_file = slope_file,
#                            rain = rain_file,
#                            ModelFolder = ModelFolder,
#                            time_step = time_step,
#                            simulation_length = simulation_length,
#                             = F)


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
#   #rain_file <- file.path(ModelFolder, "model-rainfall.csv") # must be created in set-up
#   time_step <- 5
#   simulation_length <- 90
#
#   flowModel(SoilStack_file = soilstack_file,
#             flowStack_file = flowStack_file,
#             landCover_file = land_cover_clip_file,
#             slope_file = slope_file,
#             rain = rain_file,
#             ModelFolder = x,
#             time_step = time_step,
#             simulation_length = simulation_length,
#              = T)
#
#   print(paste0("Finished with ", x, "model"))
# }

# Test
