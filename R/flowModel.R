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

flowModel <- function(ModelFolder,
                      rain_file,
                      time_step = 1,
                      simulation_length = 120,
                      store = T,
                      rainfall_method = "gauges",
                      impervious = F,
                      gif = T,
                      restartModel = F,
                      ...){
  gc()
  print(paste("Time step:", time_step))
  print(paste("Simulation length:", simulation_length))
  print(paste("Estimated run time:", round(simulation_length/ time_step * 25/60), " minutes."))
  # Set up parallel backend
  # num_cores <- parallel::detectCores() - 1  # Use one less core to avoid overloading the system
  # cl <- parallel::makeCluster(num_cores)
  # doParallel::registerDoParallel(cl)

  volumeIn_m3 <- volumeOut_m3 <- NULL
  start_time <- Sys.time()
  # Load soil stack
  SoilStack <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
  flowStackMethod <- file.path(ModelFolder, "stack_flow.tif")
  if(file.exists(flowStackMethod)){
    flowStack <- terra::rast(flowStackMethod)
  }

  activeCells <- cellsWithValues(SoilStack$model_dem)

  gridsize <- 10 # manually set grid-size, based on DEM
  drainCells <- data.table::fread(file.path(ModelFolder, "drainCells.csv"))
  # terra::extract(SoilStack$surfaceWater, drainCells$cell[1])*length^2
  # SoilStack$surfaceWater[drainCells[1,2:3]]
  #keyCells <- getCellNumber(drainCells, SoilStack) # key cells list(empty, discharge)

  # Check and determine the rainfall input
  rainList <- loadRain(rain_file, rainfall_method = rainfall_method)
  rain <- rainList[[1]]
  total_rain_duration <- rainList[[2]]
  #time_step <- 1 # set timestep to one minute for each rainfall period
  # Important set-up variables
  simulation_duration <- seq(0, simulation_length, by = time_step) # minute duration of simulation
  # Create data frame with time information: Simulation length | Time-step | Simulation time
  simulationDF <- data.frame(simlength = simulation_length, timestep = time_step, simtime = 0)

  # Temporary files - for restarts
  simulationProgress <- file.path(ModelFolder, "simulation-progress.csv") # time data
  tempStorage <- file.path(ModelFolder, "tempStorage.tif") # soils data

  # Obtain the discharge for the outflow cell
  out_discharge <- data.table::data.table(time = 0, height_cm = 0, discharge = 0)
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
    model_checks <- data.table::data.table(data.table::fread(file.path(ModelFolder, "model-checks.csv")))
    volumes <- data.table::fread(file.path(ModelFolder, "volumes.csv"))
    # volumeOut <- data.table::fread(file.path(ModelFolder, "volumeOut.csv"))
    # volumeIn <- data.table::fread(file.path(ModelFolder, "volumeIn.csv"))

  }else{
    volumes <- data.table::data.table(Time_min = 0, time_elapsed_s = 0, mean_rain_cm = 0, mean_surface_depth_cm = 0,
                                     mean_infiltration_cm = 0, gauge_height_cm = 0, gauge_velocity_cm_s = 0,
                                     gauge_discharge_m3_s = 0, mean_total_rain_cm = 0, total_volume_out_cm = 0,
                                     volume_difference = 0)
    # volumeOut <- data.table::data.table(timestep = 0, volume = 0, discharge = 0)
    # data.table::fwrite(volumeOut, file.path(ModelFolder, "volumeOut.csv"))
    # volumeIn <- data.table::data.table(timestep = 0, total_rain_cm = 0) # Sum all of the rainfall during a rainfall event
    # data.table::fwrite(volumeIn, file.path(ModelFolder, "volumeIn.csv"))
    # Create initial rasters for outputs
    if(impervious == FALSE){
      subsurfacePath <- initializeRaster(SoilStack$mannings_n, "soilStorage", ModelFolder)
    }
    surfacePath <- initializeRaster(SoilStack$mannings_n*0, "surfaceStorage", ModelFolder)
    velocityPath <- initializeRaster(SoilStack$mannings_n*0, "velocityStorage", ModelFolder)

    # Create distance storage
    SoilStack$distStorage <- SoilStack$mannings_n * 0 # Creates new layer
    distancePath <- initializeRaster(SoilStack$distStorage, "distanceStorage", ModelFolder)

    # Create runoff Depth before loop
    runoffDepth <- SoilStack$distStorage
    runoffDepthPath <- initializeRaster(SoilStack$mannings_n*0, "runoffDepth", ModelFolder)

    # Empty flow map
    flowMapPath <- initializeRaster(SoilStack$distStorage, "flowMap", ModelFolder)
    # Empty Drain Map
    drainMapPath <- initializeRaster(SoilStack$mannings_n*0, "drainMap", ModelFolder)
    #SoilStack$flowMap <- SoilStack$currentSoilStorage * 0

    # Stores the Amount of time elapse for each iteration
    model_checks <- data.table::data.table(time = 0, max_velocity_cm_s = 0, vel_cellnumber = 1, max_height_cm = 0, height_cellnumber = 1)

    # Stores shapefiles for velocities
    velocityMax_df <- dfMax(terra::rast(file.path(ModelFolder, "velocityStorage.tif")), rename = 0)
    # Stores points for maximum surface water depths
    #depthMaxDF <- dfMax(terra::rast(file.path(ModelFolder, "surfaceStorage.tif")), rename = 0)
  }

  saveRate <- time_step
  counter <- saveRate # counter to save rasters on certain intervals
  # Count number of cells
  rain_values <- terra::values(SoilStack$mannings_n)
  active_cells <- length(rain_values[!is.na(rain_values)]) # cells with values
  # Progress Bar
  progressBar <- utils::txtProgressBar(min = 0, max = length(simulation_duration), style = 3)
  start <- Sys.time()

  simulation_values <- 1:(length(simulation_duration)-1)
  # Determine the outflow cell
  outflow_cell <- drainCells$cell[1]
# Loop through time
for(t in simulation_values){
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
  # rainfall in cm / time step - which the time-step should be in minutes
  total_rain_cm <- rainfall_for_timestep * in_to_cm
  # # Same amount of rain per time step
  # Check rainfall extent
  SoilStack$current_rainfall <- terra::ifel(is.finite(SoilStack$model_dem), total_rain_cm, NA) # rainfall distribution map

  # Volume calculations
  totalDepthCM <- sum(terra::values(SoilStack$current_rainfall), na.rm = T) # Sum of all depths
  area <- terra::expanse(SoilStack$current_rainfall, unit = "m")[[2]] # area with non-zeros
  volumeM3 <- totalDepthCM/100 * area # cubic meters
  averageDepthCM <- volumeM3 / (area) * 100 # average depth cm
  #averageDepthCM <- volumeM3 / (area * active_cells) * 100 # average depth cm
  # Saves later in script
  mean_rain_depth_cm <- sumCells(SoilStack$current_rainfall) / activeCells

  # volumeIn <- rbind(volumeIn,
  #                   list(end_time, volumeM3))
  # data.table::fwrite(volumeIn, file.path(ModelFolder, "volumeIn.csv"))
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
    # SoilStack$currentSoilStorage <- subsurfaceFlow(SoilStack, simulationTimeSecs, flowStack_file)

  }else{
    #SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall # water not infiltrated

  # Calculates the current storage of the throughfall and current soil storage - adjust for rate of infiltration?
 #becomes surface water
  ## [4] Surface Runoff
  #print(names(SoilStack))
  # Create surface stack to pass only the important surface variables
  surfaceStack <- c(SoilStack$surfaceWater,
                    SoilStack$mannings_n,
                    SoilStack$throughfall,
                    SoilStack$slope,
                    SoilStack$model_dem,
                    SoilStack$flow_direction)

  runoff_counter <- 0
  time_remaining <- simulationTimeSecs
  while(runoff_counter != simulationTimeSecs){
    # Calculate the time delta
    limits <- time_delta(surfaceStack, gridSize = gridsize, time_step_min = 1, courant_condition = .5, vel = T)
    time_delta_s <- limits[[1]]
    #print(time_delta_s)
    # Calculate the velocity over the timestep
    velocity <- limits[[2]]

    time_remaining <- time_remaining - time_delta_s
    if(time_remaining < 0){
      time_delta_s <- time_remaining + time_delta_s
    }
    # Calculate new surface (cm)
    depth_list <- surfaceRouting(surfaceStack = surfaceStack,
                                 time_delta_s = time_delta_s,
                                 gridSize = gridsize,
                                 rain_step_min = 1)

    surfaceStack$surfaceWater <- SoilStack$surfaceWater <- depth_list[[1]]
    infiltration_depth_cm <- depth_list[[2]]
    rain_depth_cm <- depth_list[[3]]

    # Infiltrated water goes here
    infiltration <- 0

    # Calculate the time in minutes after each time-step
    end_time <- beginning_time + round((runoff_counter + time_delta_s) / 60, 5) # min

    # Write raster to file
    if(!impervious){
      rasterWrite(round(SoilStack$currentSoilStorage,3), ModelFolder, end_time, layername = "soil")
    }
      # Write surface depth for time-step
      rasterWrite(round(surfaceStack$surfaceWater,3), ModelFolder, end_time, layername = "surface")
      maxHeight <- max_in_raster(round(surfaceStack$surfaceWater,3))# max height cm
      # Save the outflow cell
      # Adjust the outflow height -- here in cm
      outflow <- surfaceStack$surfaceWater[outflow_cell][[1]]
      # Add outflow discharge here
      out_discharge <- rbind(out_discharge, list(end_time, outflow, outflow))
      # Write output discharge
      data.table::fwrite(out_discharge, file = file.path(ModelFolder, "out-discharge.csv"))
      # Remove water from outflow cell
      SoilStack$surfaceWater[outflow_cell] <- surfaceStack$surfaceWater[outflow_cell] <- 0

      rasterWrite(round(velocity,3), ModelFolder, end_time, layername = "velocity")
      out_velocity <- velocity[outflow_cell][[1]]
      # Write checks to tables

      # Calculate maximum cell value and location
      maxVel <- max_in_raster(velocity) # returns c(maximum value, cell number)

      model_checks <- rbind(model_checks, list(time = end_time,
                                               max_velocity_cm_s = maxVel[1],
                                                vel_cellnumber = maxVel[2],
                                                max_height_cm = maxHeight[1],
                                                height_cellnumber = maxHeight[2]))

      volumes <- rbind(volumes, list(Time_min = end_time,
                                     time_elapsed_s = time_delta_s,
                                     mean_rain_cm = sumCells(rain_depth_cm)/activeCells,
                                     mean_surface_depth_cm = sumCells(SoilStack$surfaceWater)/activeCells,
                                     mean_infiltration_cm = sumCells(infiltration_depth_cm)/activeCells,
                                     gauge_height_cm = outflow,
                                     gauge_velocity_cm_s = out_velocity,
                                     gauge_discharge_m3_s = round(outflow*10/time_delta_s,4), # hard-coded
                                     mean_total_rain_cm = sum(volumes[, "mean_rain_cm"]),
                                     total_volume_out_cm  = sum(volumes[, "gauge_height_cm"]),
                                     volume_difference = 0))

    # Increment the runoff counter be elapsed time (s)
     runoff_counter <- runoff_counter + round(time_delta_s,4)
     # Adjust the raster depth for the outflow cell and save it
    }

  ##---------------- Save step-------------
  if(counter %% saveRate == 0){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(TRUE | gif | counter %% saveRate == 0 | t == length(simulation_duration)){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep

    # CSV saves
    # Adjust time storage file
    simulationDF$simtime <- end_time
    data.table::fwrite(simulationDF, simulationProgress) # write current simulation time
    #data.table::fwrite(data.table::data.table(tempvelocityFile), file = file.path(ModelFolder, "max_velocity_cm_s_per_time.csv"))
    data.table::fwrite(model_checks, file = file.path(ModelFolder, "model-checks.csv"))
    data.table::fwrite(volumes, file = file.path(ModelFolder, "volumes.csv"))
    # Save a temporary version of the soil stack
    temporary <- SoilStack + 0 # create temporary Soil Stack

    terra::writeRaster(temporary, filename = tempStorage, overwrite = T)
    #terra::writeRaster(flowStack, file.path(ModelFolder, "AdjustedFlowMaps.tif"), overwrite = T)
  }
 ##------------------------------------##
  ### give a name to the current storage based upon iteration
  counter <- counter + 1
  }
}
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

