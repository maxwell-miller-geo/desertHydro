# Flow Model script
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
                      infiltration_method = "green",
                      surface_method = "nlcd",
                      velocity_method = "darcys",
                      adjust_slope = T,
                      ...){
  print(paste("Time step:", time_step))
  print(paste("Simulation length:", simulation_length))
  print(paste("Estimated run time:", round(simulation_length*2.5), " minutes."))
  print(paste("Using velocity method:", velocity_method))
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
  tempStorage <- file.path(ModelFolder, "adjustStack.tif")

  # Obtain the discharge for the outflow cell
  out_discharge <- data.table::data.table(time = 0, height_cm = 0, discharge_m3_s = 0)

  if(file.exists(tempStorage) && file.exists(simulationProgress) && restartModel){ # if model was cutoff during simulation - restart option
    print("Picking up where model left off...")
    # Determine the time it left off - model simulation and overwrite the last step recorded
    simulationDF <- data.table::fread(simulationProgress)
    lastSimulationTime <- simulationDF$simtime
    print(paste0("Starting off at ", lastSimulationTime))
    # Restart model run
    if(lastSimulationTime == simulation_length){
      stop(paste0("The last time of the simulation '", lastSimulationTime, "' equals the simulation length '", simulation_length,"' please
                  adjust the 'simulation_length' or restart the model from scratch by setting restartModel = TRUE." ))
    }
    simulation_duration <- seq(lastSimulationTime, simulation_length, by = simulationDF$timestep)
    timeCheck <- terra::rast(file.path(ModelFolder, "surfaceStorage.tif"))
    lastTime <- as.numeric(terra::tail(names(timeCheck), n = 1))
    # Reado previous step? What if there are partial saves - doesn't check for
    adjustStack <- terra::rast(tempStorage)
    SoilStack <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
    subsurfacePath <- file.path(ModelFolder, "soilStorage.tif")
    surfacePath <- file.path(ModelFolder, "surfaceStorage.tif")
    velocityPath <- file.path(ModelFolder, "velocityStorage.tif")
    #distancePath <- file.path(ModelFolder, "distanceStorage.tif")
    # runoffDepthPath <- file.path(ModelFolder, "runoffDepth.tif")
    #flowMapPath <- file.path(ModelFolder, "flowMap.tif")
    # drainMapPath <- file.path(ModelFolder, "drainMap.tif")
    #velocityMax_df <- as.data.frame(terra::vect(file.path(ModelFolder, "max-velocity.shp")))
    #depthMaxDF <- as.data.frame(terra::vect(file.path(ModelFolder, "max-depth.shp")))
    model_checks <- data.table::data.table(data.table::fread(file.path(ModelFolder, "model-checks.csv")))
    volumes <- data.table::fread(file.path(ModelFolder, "volumes.csv"))
    # volumeOut <- data.table::fread(file.path(ModelFolder, "volumeOut.csv"))
    # volumeIn <- data.table::fread(file.path(ModelFolder, "volumeIn.csv"))

  }else{
    volumes <- data.table::data.table(Time_min = 0, time_elapsed_s = 0, total_rain_cm = 0, total_surface_depth_cm = 0,
                                      total_infiltration_cm = 0, gauge_height_cm = 0, gauge_velocity_cm_s = 0,
                                     gauge_discharge_m3_s = 0, mean_total_rain_cm = 0,
                                     volume_difference_cm = 0, cumulative_infiltrated_water_cm = 0, cumulative_rain_cm = 0,
                                     cumulative_height_out_cm = 0, cumulative_outflow_percent = 0,cumulative_infiltration_percent = 0,
                                     cumulative_surface_percent = 0)

    if(!impervious){
      subsurfacePath <- initializeRaster(SoilStack$mannings_n*0, "soilStorage", ModelFolder, zero = T)
    }
    surfacePath <- initializeRaster(SoilStack$mannings_n*0, "surfaceStorage", ModelFolder, zero = T)
    velocityPath <- initializeRaster(SoilStack$mannings_n*0, "velocityStorage", ModelFolder, zero = T)

    # Stores the Amount of time elapse for each iteration
    model_checks <- data.table::data.table(time = 0, max_velocity_cm_s = 0, vel_cellnumber = 1, max_height_cm = 0, height_cellnumber = 1)

    # Stores shapefiles for velocities
    #velocityMax_df <- dfMax(terra::rast(file.path(ModelFolder, "velocityStorage.tif")), rename = 0)
    # Stores points for maximum surface water depths
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
  # Adjust manning's based on slope - not re-adjusted through time
  if(grepl("slope", surface_method)){
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

# Initialize  size of things beforehand
# Things to initialize
  staticStack <- c(SoilStack$model_dem,
                    SoilStack$flow_direction)

  adjustStack <- c(SoilStack$surfaceWater,
                   SoilStack$slope,
                   SoilStack$mannings_n)

  if(!impervious){
    # Add subsurface stack
    staticStack <- c(staticStack,
                     SoilStack$infiltration_cmhr,
                     SoilStack$maxSoilStorageAmount,
                     SoilStack$saturation_percent,
                     SoilStack$initial_sat_content,
                     SoilStack$Ksat_cm_hr)

    adjustStack <- c(adjustStack,
                     SoilStack$currentSoilStorage,
                     SoilStack$infiltrated_water_cm)

    # Check to make sure maximum soil storage is not exceeded
    adjustStack$currentSoilStorage <- terra::ifel((staticStack$maxSoilStorageAmount - adjustStack$currentSoilStorage) > 0,
                                                  adjustStack$currentSoilStorage,
                                                  staticStack$maxSoilStorageAmount)
  }else{
    # Impervious conditions
    infiltration_cmhr <- SoilStack$infiltration_cmhr*0
    staticStack <- c(staticStack, infiltration_cmhr)
    currentSoilStorage <- infiltration_cmhr # assign soils storage to 0
    names(currentSoilStorage) <- "currentSoilStorage"
    adjustStack <- c(adjustStack, currentSoilStorage, SoilStack$infiltrated_water_cm)
  }

  # Individual layers that will be adjusted
  surfaceWater <- adjustStack$surfaceWater
  slope <- adjustStack$slope
  mannings_n <- adjustStack$mannings_n
  currentSoilStorage <- adjustStack$currentSoilStorage
  infiltrated_water_cm <- adjustStack$infiltrated_water_cm
  # Write the Surface Stack
  terra::writeRaster(adjustStack+0, tempStorage, overwrite = T)
  #browser()
  rm(SoilStack)
for(t in simulation_values){
  # Values to be read in
  #surfaceWater, mannings_n, infiltration_rate_cm_hr, slope, model_dem
  # Modified layers
  # surfaceWater, slope, currentSoilStorage, (mannigns_n)
  terra::tmpFiles(remove=TRUE)  # Clears unused temp raster files

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
     if (!(terra::ext(rainfall_for_timestep) == terra::ext(adjustStack))){
       # Potentially scale up - later on - should be rainfall over a minute
       rainfall_for_timestep <- terra::crop(rainfall_for_timestep, terra::ext(adjustStack))
   }
  }
    }else{
    rainfall_for_timestep <- 0
  }
  # Calculate rainfall for time-step
  in_to_cm <- 2.54
  # rainfall in cm / time step - which the time-step should be in minutes
  total_rain_cm <- rainfall_for_timestep * in_to_cm
  #SoilStack$current_rainfall <- terra::ifel(is.finite(SoilStack$model_dem), total_rain_cm, NA) # rainfall distribution map
  current_rainfall <- terra::ifel(is.finite(surfaceWater), total_rain_cm, NA) # rainfall distribution map
  # Rain volume calculations
  # Calculate throughfall
  throughfall <- current_rainfall
  runoff_counter <- 0
  time_remaining <- simulationTimeSecs

  # Loop over 60 seconds - checking that simulation runs up to 60 seconds
  while(runoff_counter != simulationTimeSecs){
    # # Calculate the time delta
    # Calculate potential velocity
    if(velocity_method == "mannings"){
      velocity <- manningsVelocity(mannings_n, surfaceWater, slope, length = cellsize, units = "cm/s")
    }else if(velocity_method == "darcys"){
      velocity <- darcysVelocity(mannings_n, surfaceWater, slope)
    }
    # Speed limit on water - Can't go faster than 1000 cm/s or 10 m/s
    if(velocity_method == "mannings"){
      if(terra::global(velocity, "max", na.rm = TRUE)[,1] > 1000){
        velocity <- terra::ifel(velocity > 1000, 1000, velocity)
      }
    }

    # Calculate infiltration rate
    if(grepl("green", infiltration_method) && !impervious){
      # Returns infiltration in cm/hr
      staticStack$infiltration_cmhr <- green_ampt_infil(Ksat_cm_hr = staticStack$Ksat_cm_hr,
                                       theta_s = staticStack$saturation_percent,
                                       theta_i = staticStack$initial_sat_content,
                                       F_0 = infiltrated_water_cm)

    }else if(grepl("flat", infiltration_method) && !impervious){
      staticStack$infiltration_cmhr <- staticStack$infiltration_cmhr
    }else{
      staticStack$infiltration_cmhr <- staticStack$infiltration_cmhr
    }

    limits <- time_delta(surfaceWater = surfaceWater,
                         velocity = velocity,
                         throughfall = throughfall,
                         infiltration_rate_cm_hr = staticStack$infiltration_cmhr,
                         cellsize = cellsize, time_step_min = 1,
                         courant_condition = courant, vel = F,
                         impervious = impervious)

    time_delta_s <- limits

    if(time_delta_s < 0){
      stop("Error: Negative time step occured, please check input variables")
    }
     #print(paste("Time calculate:", time_delta_s))
    # Calculate the velocity over the timestep
    # velocity <- limits[[2]]
    #print(paste("Time remaining:", time_remaining))
    time_remaining <- time_remaining - time_delta_s
    #print(paste("Time remaining:", time_remaining))
    if(runoff_counter + time_delta_s > simulationTimeSecs){
      time_delta_s <- simulationTimeSecs - runoff_counter
    }

    # Calculate new surface (cm)
    # mannings_n, surfaceWater, slope, throughfall, infiltration_cmhr, flow_direction, currentSoilStorage, maxSoilStorage
    depth_list <- surfaceRouting(surfaceStack = staticStack,
                                 adjustStack = adjustStack,
                                 throughfall = throughfall,
                                 time_delta_s = time_delta_s,
                                 velocity = velocity,
                                 cellsize = cellsize,
                                 rain_step_min = 1,
                                 infiltration = !impervious)

    # Save new depth to surface water
    #surfaceStack$surfaceWater <- SoilStack$surfaceWater <- depth_list[[1]]
    surfaceWater <- depth_list[[1]]
    # Adjust the slope for next time-step
    #new_dem <- surfaceStack$surfaceWater/100 + surfaceStack$model_dem # assumes meters
    # Adjust elevation model every so often? or never?
    if(adjust_slope){
      new_dem <- surfaceWater/100 + staticStack$model_dem # assumes meters
      slope_temp <- terra::terrain(new_dem, v = "slope", neighbors = 8, unit = "radians")
      new_slope <- terra::merge(slope_temp, model_slope) # opened earlier
      #new_slope <- slope_edge(new_dem, slope_temp, cellsize = cellsize)
      names(new_slope) <- "slope"
      slope <- new_slope
    }else{
      slope <- slope
    }

    # Return infiltration depth
    infiltration_depth_cm <- depth_list[[2]]
    rain_depth_cm <- depth_list[[3]]

    # Add infiltration to subsurface
    #surfaceStack$currentSoilStorage <- surfaceStack$currentSoilStorage + infiltration_depth_cm
    currentSoilStorage <- currentSoilStorage + infiltration_depth_cm
    infiltrated_water_cm <- infiltrated_water_cm + infiltration_depth_cm

    # Calculate the time in minutes after each time-step
    end_time <- beginning_time + round((runoff_counter + time_delta_s) / 60, 5) # min

    # Write raster to file at every minute
    if(end_time %% 1 == 0){
      if(!impervious){
        rasterWrite(round(currentSoilStorage,3), ModelFolder, end_time, layername = "soil")
        }
      # Write surface depth for time-step
      rasterWrite(round(surfaceWater,3), ModelFolder, end_time, layername = "surface")
      # Write velocity for time-step
      rasterWrite(round(velocity,3), ModelFolder, end_time, layername = "velocity")
    }
      #maxHeight <- max_in_raster(round(surfaceStack$surfaceWater,3)) # max height cm
      maxHeight <- max_in_raster(round(surfaceWater,3)) # max height cm
      # Save the outflow cell
      # Adjust the outflow height -- here in cm
      #outflow <- surfaceStack$surfaceWater[outflow_cell][[1]]
      outflow <- surfaceWater[outflow_cell][[1]]
      # Add outflow discharge here
      # Height/100 * cellsize^2 / time elapsed = Q (m3/s)
      outflow_discharge <- outflow/100 * cellsize^2 / time_delta_s
      # Bind data for time-step
      out_discharge <- rbind(out_discharge, list(end_time, outflow, outflow_discharge))
      # Write output discharge
      data.table::fwrite(out_discharge, file = file.path(ModelFolder, "out-discharge.csv"))

      # Remove water from outflow cell
      #SoilStack$surfaceWater[outflow_cell] <- surfaceStack$surfaceWater[outflow_cell] <- 0
      surfaceWater[outflow_cell] <- 0

      # Adjust layers for next time through
      adjustStack[[1]] <- surfaceWater
      adjustStack[[2]] <- slope
      adjustStack[[3]] <- mannings_n
      adjustStack[[4]] <- currentSoilStorage
      adjustStack[[5]] <- infiltrated_water_cm
      # rm(adjustStack)
      # adjustStack <- c(surfaceWater, currentSoilStorage, slope, mannings_n, infiltrated_water_cm)

      # Velocity of outflow -feeder cell
      out_velocity <- velocity[velocity_cell][[1]]

      # Calculate maximum cell value and location
      maxVel <- max_in_raster(velocity) # returns c(maximum value, cell number)

      model_checks <- rbind(model_checks, list(time = end_time,
                                               max_velocity_cm_s = maxVel[1],
                                                vel_cellnumber = maxVel[2],
                                                max_height_cm = maxHeight[1],
                                                height_cellnumber = maxHeight[2]))
      # Model step totals
      total_rain_cm <- sumCells(rain_depth_cm) # current rain totals
      total_infiltration_cm <- sumCells(infiltration_depth_cm) # current infiltration
      total_surface_depth_cm <- sumCells(surfaceWater) # current surface depths - temporary source

      # Cumulative totals
      cumulative_infiltrated_water_cm <- sum(volumes[, "total_infiltration_cm"]) + total_infiltration_cm
      cumulative_rain_cm <- sum(volumes[, "total_rain_cm"])+ total_rain_cm # total rain input
      cumulative_height_out_cm <- sum(volumes[, "gauge_height_cm"]) + outflow

      #mean_rain_cm <- sumCells(rain_depth_cm)/activeCells
      #mean_surface_depth_cm <- sumCells(SoilStack$surfaceWater)/(activeCells)
      #mean_surface_depth_cm <- sumCells(surfaceWater)/(activeCells)
      #mean_infiltration_cm <- sumCells(infiltration_depth_cm)/activeCells
      #mean_total_rain_cm <- sum(volumes[, "mean_rain_cm"])+ mean_rain_cm



      # Determine the volume difference in average cm per cell
      volume_difference_cm <- cumulative_rain_cm -
        (cumulative_height_out_cm +
        cumulative_infiltrated_water_cm +
        total_surface_depth_cm)

      # Determine the volume difference in average cm per cell
      # volume_difference_mean_cm <- mean_total_rain_cm -
      #   total_height_out_cm/activeCells +
      #   total_infiltrated_water_cm +
      #   mean_surface_depth_cm

      cumulative_outflow_percent <- round((cumulative_height_out_cm/cumulative_rain_cm)*100,2)
      cumulative_infiltration_percent <- round((cumulative_infiltrated_water_cm/cumulative_rain_cm)*100,2)
      cumulative_surface_percent <- round((total_surface_depth_cm/cumulative_rain_cm)*100,2)

      volumes <- rbind(volumes,
                       list(Time_min = end_time,
                            time_elapsed_s = time_delta_s,
                            total_rain_cm = total_rain_cm,
                            total_surface_depth_cm = total_surface_depth_cm,
                            total_infiltration_cm = total_infiltration_cm,
                            gauge_height_cm = outflow,
                            gauge_velocity_cm_s = out_velocity,
                            gauge_discharge_m3_s = round(outflow/100*cellsize^2/time_delta_s,4),
                            mean_total_rain_cm = total_rain_cm/activeCells,
                            volume_difference_cm = volume_difference_cm,
                            cumulative_infiltrated_water_cm = cumulative_infiltrated_water_cm,
                            cumulative_rain_cm = cumulative_rain_cm,
                            cumulative_height_out_cm = cumulative_height_out_cm,
                            cumulative_outflow_percent = cumulative_outflow_percent,
                            cumulative_infiltration_percent = cumulative_infiltration_percent,
                            cumulative_surface_percent = cumulative_surface_percent))

    # Increment the runoff counter be elapsed time (s)
     runoff_counter <- runoff_counter + round(time_delta_s,4)
     if(abs(runoff_counter-simulationTimeSecs) < 0.1){
       runoff_counter <- simulationTimeSecs
     }
    }

  ##---------------- Save step-------------
# when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
  if(counter %% 25 == 0 || t == length(simulation_duration) || t == tail(simulation_duration,1)){ # when so save the outputs - saveRate = 3, saves outputs every 3rd timestep
    if(!impervious){
      rasterCompile(ModelFolder, "soil", remove = T, overwrite = F)
    }
    rasterCompile(ModelFolder, "surface", remove = T, overwrite = F)
    rasterCompile(ModelFolder, "velocity", remove = T, overwrite = F)
    # CSV saves
    # Adjust time storage file
    simulationDF$simtime <- end_time
    data.table::fwrite(simulationDF, simulationProgress) # write current simulation time
    data.table::fwrite(model_checks, file = file.path(ModelFolder, "model-checks.csv"))
    data.table::fwrite(volumes, file = file.path(ModelFolder, "volumes.csv"))
    # Save a temporary version of the soil stack
    temporary <- adjustStack + 0 # create temporary Soil Stack
    terra::writeRaster(temporary, filename = tempStorage, overwrite = T)
  }
 ##------------------------------------##
  ### give a name to the current storage based upon iteration
  counter <- counter + 1 # End of loop
}
  print(paste("The model took: ", paste0(difftime(Sys.time(), start_time))))
  model_complete <- "Model Complete"
  utils::write.table(model_complete, file = file.path(ModelFolder, "ModelComplete.txt"))

  # Save simulation time as text
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time)
  out_duration <- paste("Simulation took", round(duration[[1]], 2),  units(duration), "to run.")
  writeLines(out_duration, file.path(ModelFolder, "simulation_time.txt"))
}

