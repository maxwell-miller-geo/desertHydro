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
                      courant = 0.8,
                      cellsize = NULL,
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
  model_slope <- terra::rast(file.path(ModelFolder, "model_slope.tif"))
  flowStackMethod <- file.path(ModelFolder, "stack_flow.tif")
  if(file.exists(flowStackMethod)){
    flowStack <- terra::rast(flowStackMethod)
  }

  activeCells <- cellsWithValues(SoilStack$model_dem)
  if(is.null(cellsize)){
    cellsize <- grid_size(SoilStack) # based on soil stack size
  }
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
  # Check simulation length for GOES rainfall method
  # if(rainfall_method == "goes"){
  #   # Check rainfall folder for rain-discharge.csv
  #   rain_discharge <- filePresent("rain-discharge.csv", ModelFolder)
  #   simulation_length <- as.numeric(difftime(get_start_end_time(rain_discharge)$end,
  #            as.POSIXlt(names(terra::rast(rain))[1],tz = "MST"),
  #            units = "mins"))
  # }
  simulation_duration <- seq(0, simulation_length, by = time_step) # minute duration of simulation
  # Create data frame with time information: Simulation length | Time-step | Simulation time
  simulationDF <- data.frame(simlength = simulation_length, timestep = time_step, simtime = 0)

  # Temporary files - for restarts
  simulationProgress <- file.path(ModelFolder, "simulation-progress.csv") # time data
  tempStorage <- file.path(ModelFolder, "tempStorage.tif") # soils data

  # Obtain the discharge for the outflow cell
  out_discharge <- data.table::data.table(time = 0, height_cm = 0, discharge_m3_s = 0)
  #write.csv(simulationDF, file = simulationProgress)ra
  #readin <- read.csv(simulationProgress)
  if(file.exists(tempStorage) & file.exists(simulationProgress) & restartModel){ # if model was cutoff during simulation - restart option
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
    surfacePath <- file.path(ModelFolder, "surfaceStorage.tif")
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
                                     gauge_discharge_m3_s = 0, mean_total_rain_cm = 0, total_height_out_cm = 0,
                                     total_infiltrated_water_cm = 0, volume_difference_mean_cm = 0, cumulative_outflow_percent = 0, cumulative_infiltration_percent = 0)
    # volumeOut <- data.table::data.table(timestep = 0, volume = 0, discharge = 0)
    # data.table::fwrite(volumeOut, file.path(ModelFolder, "volumeOut.csv"))
    # volumeIn <- data.table::data.table(timestep = 0, total_rain_cm = 0) # Sum all of the rainfall during a rainfall event
    # data.table::fwrite(volumeIn, file.path(ModelFolder, "volumeIn.csv"))
    # Create initial rasters for outputs
    if(!impervious){
      subsurfacePath <- initializeRaster(SoilStack$mannings_n*0, "soilStorage", ModelFolder, zero = T)
    }
    surfacePath <- initializeRaster(SoilStack$mannings_n*0, "surfaceStorage", ModelFolder, zero = T)
    velocityPath <- initializeRaster(SoilStack$mannings_n*0, "velocityStorage", ModelFolder, zero = T)

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
  # Velocity cell - second to last cell that feeds the outflow cell
  velocity_cell <- drainCells$cell[2]
  # Set infiltration to zero for runoff only
  if(impervious){
    SoilStack$infiltration_cmhr <- 0
  }
  # Adjust manning's based on slope - not re-adjusted through time
  if(FALSE){
    SoilStack$mannings_n <- adjust_mannings(SoilStack$slope)
  }
  # Load in rainfall method first if GOES satellite imagery
  if(tolower(rainfall_method) == "goes"){
    # Load in the rainfall raster
    goes <- terra::rast(rain_file)
  }else{
    goes <- NA
  }
# Loop through time
  #browser()

for(t in simulation_values){
  utils::setTxtProgressBar(progressBar, t)
  beginning_time <- simulation_duration[t]
  end_time <- simulation_duration[t+1]
  timeElapsed <- end_time - beginning_time # time elapsed in minutes
  simulationTimeSecs <- timeElapsed * 60 # time elapse in minutes * seconds

  ## [1] Rainfall
  ## - Calculates the amount of rainfall in a given time step
  if(simulation_duration[t] < total_rain_duration){ # could cut off rainfall if not careful
    rainfall_for_timestep <- rainfallAccum(rain, beginning_time, end_time, rainfall_method = rainfall_method, ModelFolder = ModelFolder, goes = goes)
   if(rainfall_method == "goes" && inherits(rainfall_for_timestep, "SpatRaster")){
     if (!(terra::ext(rainfall_for_timestep) == terra::ext(SoilStack))){
       # Potentially scale up - later on - should be rainfall over a minute
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
  # Memory removal
  rm(rainfall_for_timestep)
  #print(total_rain_cm)
  # # Same amount of rain per time step
  # Check rainfall extent
  SoilStack$current_rainfall <- terra::ifel(is.finite(SoilStack$model_dem), total_rain_cm, NA) # rainfall distribution map

  # Rain volume calculations
  # totalDepthCM <- sum(terra::values(SoilStack$current_rainfall), na.rm = T) # Sum of all depths
  # area <- terra::expanse(SoilStack$current_rainfall, unit = "m")[[2]] # area with non-zeros
  # volumeM3 <- totalDepthCM/100 * area # cubic meters
  # mean_rain_depth_cm <- sumCells(SoilStack$current_rainfall) / activeCells
  #
  # # Not implemented
  ## [2] Canopy
  # Evaluate canopy storage - (current-storage + rainfall)
  # SoilStack$current_canopy_storage <- SoilStack$maxCanopyStorageAmount

  # Calculate throughfall
  SoilStack$throughfall <- SoilStack$current_rainfall
  # Temporary set up - assuming all rainfall is throughfall
  # After the water has made its way through the canopy it is now throughfall
  ## ----------- Not set up ------------------
  ## [3] Subsurface - Surface
  # Subsurface Lateral Flow
  # Lateral flow is based on Darcy's Law, with gradient equal to land slope, and direction maps
  # Calculated from the elevation model - flow partitioned
  # Diversions and tile drains are assumed to effectively divert the lateral flow, so that it goes directly into the stream rather than downhill?? - Not in here
  # Calculate the amount of runoff - based on the difference between the throughfall and current soil storage amount
  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
    # Adjustments
    #SoilStack$effectiveInfiltrationRate <- SoilStack$surfaceWater * .4 # Infiltration rate is 40% of surface water
    # Infiltration rate
    # soilAdjustments <- infiltration(SoilStack, simulationTimeSecs)
    # SoilStack$surfaceWater <- soilAdjustments[[1]]
    # SoilStack$currentSoilStorage <- soilAdjustments[[2]]

    # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
    # Rate of m/s
    # SoilStack$currentSoilStorage <- subsurfaceFlow(SoilStack, simulationTimeSecs, flowStack_file)


    #SoilStack$surfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall # water not infiltrated
  # Calculates the current storage of the throughfall and current soil storage - adjust for rate of infiltration?
 #becomes surface water

  ## [4] Surface Runoff
  #print(names(SoilStack))
  # Create surface stack to pass only the important surface variables
  #browser()
  # Copies a stack

  surfaceStack <- c(SoilStack$surfaceWater,
                    SoilStack$mannings_n,
                    SoilStack$throughfall,
                    SoilStack$slope,
                    SoilStack$model_dem,
                    SoilStack$flow_direction)
  if(!impervious){
    # Add subsurface stack
    surfaceStack <- c(surfaceStack, SoilStack$infiltration_cmhr,
                                     SoilStack$currentSoilStorage,
                                    SoilStack$maxSoilStorageAmount)
  }

  runoff_counter <- 0
  time_remaining <- simulationTimeSecs
  # Loop over 60 seconds
  while(runoff_counter != simulationTimeSecs){
    # # Calculate the time delta
    limits <- time_delta(surfaceStack, cellsize = cellsize, time_step_min = 1, courant_condition = courant, vel = T, impervious = impervious)
    time_delta_s <- limits[[1]]
    if(time_delta_s < 0){
      stop("Error: Negative time step occured, please check input variables")
    }
    # Adjust infiltration rate based upon water infiltrated
    # of remaining water
    # if(!impervious){
    #   # Insert method for infiltration here or earlier
    #   #SoilStack$infiltration_cmhr <- SoilStack$infiltration_cmhr * 1
    #   time_hours <- time_delta_s / 3600
    #   # Look at throughfall
    #   water_infiltrated_cm <- SoilStack$infiltration_cmhr * time_hours
    #   # Hope this works
    #   water_infiltrated_cm <- terra::ifel(water_infiltrated_cm < surfaceStack$throughfall,
    #                               water_infiltrated_cm,
    #                               surfaceStack$throughfall)
    #   # Adjust amount of water stored in the soil
    #   SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage + water_infiltrated_cm
    #   # Check to see if the water exceed storage
    #   difference <- SoilStack$maxSoilStorageAmount - SoilStack$currentSoilStorage
    #   # Calculate the excess water infiltrated - taken care of in routing
    #   excess_water <- terra::ifel(difference < 0, abs(difference), 0)
    #   SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - excess_water
    #   # Adjust soil infiltration rate for next time step -- to-do
    # }

    #print(paste("Time calculate:", time_delta_s))
    # Calculate the velocity over the timestep
    velocity <- limits[[2]]
    #print(paste("Time remaining:", time_remaining))
    time_remaining <- time_remaining - time_delta_s
    #print(paste("Time remaining:", time_remaining))
    if(time_remaining < 0){
      time_delta_s <- time_remaining + time_delta_s
    }

    # Calculate new surface (cm)
    depth_list <- surfaceRouting(surfaceStack = surfaceStack,
                                 time_delta_s = time_delta_s,
                                 velocity = velocity,
                                 cellsize = cellsize,
                                 rain_step_min = 1,
                                 infiltration = !impervious)
    # Save new depth
    surfaceStack$surfaceWater <- SoilStack$surfaceWater <- depth_list[[1]]

    # Adjust the slope for next time-step
    new_dem <- surfaceStack$surfaceWater/100 + surfaceStack$model_dem # assumes meters
    slope_temp <- terra::terrain(new_dem, v = "slope", neighbors = 8, unit = "degrees")
    new_slope <- terra::merge(slope_temp, model_slope) # opened earlier
    #new_slope <- slope_edge(new_dem, slope_temp, cellsize = cellsize)
    names(new_slope) <- "slope"
    surfaceStack$slope <- SoilStack$slope <- new_slope

    # Return infiltration depth
    infiltration_depth_cm <- depth_list[[2]]
    rain_depth_cm <- depth_list[[3]]

    # Add infiltration to subsurface
    surfaceStack$currentSoilStorage <- surfaceStack$currentSoilStorage + infiltration_depth_cm

    # Calculate the time in minutes after each time-step
    end_time <- beginning_time + round((runoff_counter + time_delta_s) / 60, 5) # min

    # Write raster to file at every minute
    if(end_time %% 1 == 0){
      if(!impervious){
        rasterWrite(round(SoilStack$currentSoilStorage,3), ModelFolder, end_time, layername = "soil")
        }
      # Write surface depth for time-step
      rasterWrite(round(surfaceStack$surfaceWater,3), ModelFolder, end_time, layername = "surface")
      # Write velocity for time-step
      rasterWrite(round(velocity,3), ModelFolder, end_time, layername = "velocity")
    }
      maxHeight <- max_in_raster(round(surfaceStack$surfaceWater,3)) # max height cm
      # Save the outflow cell
      # Adjust the outflow height -- here in cm
      outflow <- surfaceStack$surfaceWater[outflow_cell][[1]]
      # Add outflow discharge here
      # Height/100 * cellsize^2 / time elapsed = Q (m3/s)
      outflow_discharge <- outflow/100 * cellsize^2 / time_delta_s
      # Bind data for time-step
      out_discharge <- rbind(out_discharge, list(end_time, outflow, outflow_discharge))
      # Write output discharge
      data.table::fwrite(out_discharge, file = file.path(ModelFolder, "out-discharge.csv"))
      # Remove water from outflow cell
      SoilStack$surfaceWater[outflow_cell] <- surfaceStack$surfaceWater[outflow_cell] <- 0
      # Velocity of outflow -feeder cell
      out_velocity <- velocity[velocity_cell][[1]]

      # Calculate maximum cell value and location
      maxVel <- max_in_raster(velocity) # returns c(maximum value, cell number)

      model_checks <- rbind(model_checks, list(time = end_time,
                                               max_velocity_cm_s = maxVel[1],
                                                vel_cellnumber = maxVel[2],
                                                max_height_cm = maxHeight[1],
                                                height_cellnumber = maxHeight[2]))

      mean_rain_cm <- sumCells(rain_depth_cm)/activeCells
      mean_surface_depth_cm <- sumCells(SoilStack$surfaceWater)/(activeCells)
      mean_infiltration_cm <- sumCells(infiltration_depth_cm)/activeCells
      mean_total_rain_cm <- sum(volumes[, "mean_rain_cm"])+ mean_rain_cm
      total_height_out_cm <- sum(volumes[, "gauge_height_cm"])+ outflow
      total_infiltrated_water_cm <- sum(volumes[, "total_infiltrated_water_cm"]) + mean_infiltration_cm

      volume_difference_mean_cm <- mean_total_rain_cm -
        total_height_out_cm/activeCells +
        total_infiltrated_water_cm +
        mean_surface_depth_cm
      cumulative_outflow_percent <- (total_height_out_cm/activeCells)/mean_total_rain_cm
      cumulative_infiltration_percent <- total_infiltrated_water_cm/mean_total_rain_cm


      volumes <- rbind(volumes,
                       list(Time_min = end_time,
                            time_elapsed_s = time_delta_s,
                            mean_rain_cm = mean_rain_cm,
                            mean_surface_depth_cm = mean_surface_depth_cm,
                            mean_infiltration_cm = mean_infiltration_cm,
                            gauge_height_cm = outflow,
                            gauge_velocity_cm_s = out_velocity,
                            gauge_discharge_m3_s = round(outflow/100*cellsize^2/time_delta_s,4),
                            mean_total_rain_cm = mean_total_rain_cm,
                            total_height_out_cm  = total_height_out_cm,
                            total_infiltrated_water_cm = total_infiltrated_water_cm,
                            volume_difference_mean_cm = volume_difference_mean_cm,
                            cumulative_outflow_percent = cumulative_outflow_percent,
                            cumulative_infiltration_percent = cumulative_infiltration_percent))

    # Increment the runoff counter be elapsed time (s)
     runoff_counter <- runoff_counter + round(time_delta_s,4)
     # Adjust the raster depth for the outflow cell and save it
     #print(paste("Runoff Counter:", runoff_counter))
    }

  ##---------------- Save step-------------
# when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(counter %% 5 == 0 | t == length(simulation_duration)){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
    if(!impervious){
      rasterCompile(ModelFolder, "soil", remove = T, overwrite = F)
    }
    rasterCompile(ModelFolder, "surface", remove = T, overwrite = F)
    rasterCompile(ModelFolder, "velocity", remove = T, overwrite = F)
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
    rm(temporary)
    gc()
    #terra::writeRaster(flowStack, file.path(ModelFolder, "AdjustedFlowMaps.tif"), overwrite = T)
  }
 ##------------------------------------##
  ### give a name to the current storage based upon iteration
  counter <- counter + 1 # End of loop
}

print(paste("The model took: ", paste0(difftime(Sys.time(), start_time))))

# close(progressBar)

  # Saved rasters
  # terra::writeRaster(surfaceStorage, filename = file.path(ModelFolder, "Surface_Storage.tif"), overwrite = T)
  # terra::writeRaster(subsurfaceStorage, filename = file.path(ModelFolder, "Soil_Moisture_percent.tif"), overwrite = T)
  # terra::writeRaster(velocityStorage, filename = file.path(ModelFolder, "Velocities.tif"), overwrite = T)
  # Additional save data

  model_complete <- "Model Complete"
  utils::write.table(model_complete, file = file.path(ModelFolder, "ModelComplete.txt"))
  #file.remove(tempStorage)

  # if(!impervious){
  #   rasterCompile(ModelFolder, "soil", remove = T)
  # }
  # rasterCompile(ModelFolder, "surface", remove = F)
  # rasterCompile(ModelFolder, "velocity", remove = F)

# Save simulation time as text
end_time <- Sys.time()
duration <- difftime(end_time, start_time)
out_duration <- paste("Simulation took", round(duration[[1]], 2),  units(duration), "to run.")
writeLines(out_duration, file.path(ModelFolder, "simulation_time.txt"))
# print("Got through it!")
#return(surfaceStorage)
}

