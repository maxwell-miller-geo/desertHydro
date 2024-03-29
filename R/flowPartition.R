# Flow functions necessary for flow partitioning
# DEM -> FlowSum Map ->
# Necessary Packages
#install.packages("terra")
#library(terra)

# Direction = vector value, direction scalar
## Function -------------------------------------
# Calculates the percentage of flow out or in from a given cell
# Utilizes terra adjacent files

## Define a custom function to calculate the difference from the center - essentially flow accumulation within a neighborhood
flowOutSum <- function(x) { # Vector of values

  center <- x[5]
  if(center < 0  | is.na(center)){ # ignore values that are 0 or NA
    return(0)
  }
  # Represent the N,E,W, and S values
  cardinal <- x[c(2,4,6,8)]
  #print(cardinal)
  # Represent the NW, NE, SW, and SE values
  diagonal <- x[c(1,3,7,9)]

  # Take the difference between cardinal values and center cell - vector of elevation differences
  card_difference <- center - cardinal[cardinal > 0]

  # Take the difference between diagonal values and center cell
  adjustment <- 1 / sqrt(2)
  diag_difference <- (center - diagonal[diagonal > 0]) * adjustment

  # Add the values together if they are above 0 (e.i., they flow into the current cell).
  # If the center is higher than a direction, it is not gaining flow from that particular direction
  flowOutElevDifference <- sum(card_difference[card_difference > 0], na.rm = TRUE) +
                sum(diag_difference[diag_difference > 0], na.rm = TRUE)

  return(flowOutElevDifference)
}
# # Test flow out sum
# testV <- c(1,1,1,1,2,1,1,1,1)
# inFlow <- flowOutSum(testV)
# outFlow <- flowOutSum(testV)
## Define a custom function to calculate the difference from the center - essentially flow accumulation within a neighborhood

## ----------------------------- Flow Map
# New better way to make flow function map!
flowMap <- function(dem, outFolder = NA, name = "stack_flow.tif"){
  # Load in DEM
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  # Create flow map based on elevation differences
  # Create Flow sum map
  kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
  dem_flow <- terra::focal3D(dem, w = kernel, fun = flowOutSum, pad = TRUE) # calculate the 'flow accumulation'

  # Shift and create dem maps
  flowMaps <- createFlowMaps(dem, dem_flow)
  if(is.character(outFolder)){
    terra::writeRaster(flowMaps, file.path(outFolder, name = "stack_flow.tif"), overwrite = T)
  }
  return(flowMaps)
}
# Test
#flowMapTest <- flowMap(dem)
## ---------------------------
# Create flow maps 2.0
createFlowMaps <- function(dem, dem_flow){
  xDim <- terra::res(dem)[1]
  yDim <- terra::res(dem)[2]
  diagFlow <- 1 / sqrt(2)
  # The names of the different flow layers
  flowKey <- list("N" = list("north_flow",c(0, 1), 1),
                  "E" = list("east_flow",c(1, 0), 1),
                  "S" = list("south_flow",c(0, -1), 1),
                  "W" = list("west_flow",c(-1, 0), 1),
                  "NW" = list("northwest_flow",c(-1, 1), diagFlow),
                  "NE" = list("northeast_flow",c(1, 1), diagFlow),
                  "SE" = list("southeast_flow",c(1, -1), diagFlow),
                  "SW" = list("southwest_flow", c(-1, -1), diagFlow)
                  )
  mapCalculations <- function(x, dem, dem_flow, xDim, yDim, flowKey){
    # Apply the shift to the dimensions of the raster
    xshift <- flowKey[[x]][[2]][[1]]*xDim
    yshift <- flowKey[[x]][[2]][[2]]*yDim
    # Stack dem and flow direction map
    stack <- c(dem, dem_flow)
    # Shift the raster
    shiftMap <- terra::shift(stack, dx = xshift, dy = yshift)
    # Crop the raster
    mapShift <- terra::crop(shiftMap, dem, snap = "near", extend = TRUE)
    # Adjust maps
    directionFlow <- (mapShift[[1]] - dem) / mapShift[[2]] * flowKey[[x]][[3]]
    # Adjust
    #filtered <- terra::ifel(directionNorth > 0, directionNorth, 0)
    filtered <- terra::subst(directionFlow, NA, 0)
    filtered <- terra::ifel(filtered < 0, 0, filtered)
    names(filtered) <- flowKey[[x]][[1]] # direction of flow
    return(filtered)
  }
  flowList <- lapply(names(flowKey), FUN = mapCalculations, dem, dem_flow, xDim, yDim, flowKey)
  flowMaps <- terra::rast(flowList)
  return(flowMaps)
}
## Flow Partitioning function- Percent flow
outputFlow <- function(values, dir){
  diagFlow <- 1 / sqrt(2)
  directions <- list("NW" = list(1, diagFlow),
                     "N" = list(2, 1),
                     "NE" = list(3, diagFlow),
                     "W" = list(4, 1),
                     "E" = list(6, 1),
                     "SW" = list(7, diagFlow),
                     "S" = list(8, 1),
                     "SE" = list(9, diagFlow))
  # Take input direction and perform the percent calculation -- needs direction list
  flowPosition <- directions[[dir]][[1]] # find the vector value within the direction dictionary (NW = 1)
  scalar <- directions[[dir]][[2]] # find the scaling factor (1 for orthogonal, .707 for diagonal)
  centerElevation <- values[14] # center value of the top layer
  flowElevation <- values[flowPosition + 9]
  # cat(flowElevation, centerElevation, flowElevation)
  # print('Done')
  # Checks if center cell and cell in question have any flow accumulation at all
  #print(paste(values, flowPosition, flowElevation, centerElevation))
  if(is.na(centerElevation) || (values[flowPosition] <= 0 || values[5] <= 0 || flowElevation <= 0 || centerElevation <= 0)){
    return(0)
  }

  else{
    # Calculate the flow percent based on the difference is the elevation values and flow accumulation
    flowPercent <- (flowElevation - centerElevation)/ (values[flowPosition])*scalar
    return(max(flowPercent, 0)) # return only positive values
  }
}

## Test for outputFlow script

## Function that takes the DEM input and brings it all together
# DEM -> Flow Sum Map (x1) -> Flow Partitioned Map(s) (x8) -> Stacked-Output
flow_Partition <- function(dem, file_name_and_path = NA){
  # Create Flow sum map
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
  dem_flow_units <- terra::focal3D(dem, w = kernel, fun = flowOutSum, pad = TRUE) # calculate the 'flow accumulation'

  # Create Flow Partition maps
  dem_flow_stack <- c(dem_flow_units, dem, dem_flow_units*NA) # stack the two rasters + empty raster
  kernel3D <- array(1, dim = c(3,3,3))  # Create a new kernel for 3x3x3 matrix.

  # Calculate the flow from each direction
  north_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "N", fillvalue = 0)
  names(north_flow) <- c("north_flow")
  east_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "E", fillvalue = 0)
  names(east_flow) <- c("east_flow")
  west_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "W", fillvalue = 0)
  names(west_flow) <- c("west_flow")
  south_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "S", fillvalue = 0)
  names(south_flow) <- c("south_flow")
  northeast_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "NE", fillvalue = 0)
  names(northeast_flow) <- c("northeast_flow")
  northwest_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "NW", fillvalue = 0)
  names(northwest_flow) <- c("northwest_flow")
  southeast_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "SE", fillvalue = 0)
  names(southeast_flow) <- c("southeast_flow")
  southwest_flow <- terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = "SW", fillvalue = 0)
  names(southwest_flow) <- c("southwest_flow")

  flowStacked <- c(northwest_flow, north_flow, northeast_flow,
                   west_flow, east_flow, southwest_flow, southeast_flow, south_flow)

  names(flowStacked) <- names(flowStacked)

  # A more concise version, needs a few adjustments
  # flow_direction <- c("N", "E", "W", "S", "NW", "NE", "SE", "SW")
  #
  # for(x in flow_direction){
  #   terra::focal3D(dem_flow_stack, w = kernel3D, fun = outputFlow, dir = x, fillvalue = 0)
  # }
  if(!is.na(file_name_and_path)){
    terra::writeRaster(flowStacked, file_name_and_path, overwrite = TRUE)
  }
  return(flowStacked)
}
# Test for flow direction maps
#flow_Partition(dem_raster, file_name_and_path = flow_file)

## Direction function takes in a vector of values and return a particular directions value
# Direction <- function(values, dir){
#   Position <- directions[[dir]][[1]]
#   return(values[Position])
# }
# Test case
# kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
# kernel <- array(1, dim = c(3,3))
# lat_flow <- SoilStack$lateralFlow
# cardinal_directions <- c("N", "E", "W", "S", "NW", "NE", "SE", "SW")
# flow_direction <- terra::focal(lat_flow, w = kernel, fun = Direction, dir = "N", fillvalue = 0)
# names(flow_direction) <- c("NE_lateraflow")
# plot(flow_direction)

## --------------------------
# Function to initialize and save rasters - returns name of file
initializeRaster <- function(firstLayer, name, outFolder, zero = F){
  temp <- firstLayer
  names(temp) <- c(name)
  #names(temp) <- 0
  outFile <- file.path(outFolder, paste0(name, ".tif"))
  terra::writeRaster(temp, outFile, overwrite = T) # writes the initial raster
  return(outFile)
}
# Test

# Function to route water through the subsurface from lateral flow values and flow direction stack

flowRouting <- function(flowToRoute, flowDirectionMap, time = F){
  # Load in flow to route - not done
  if(is.character(flowToRoute)){
    flowToRoute <- terra::rast(flowToRoute)
  }
  # Load in flow direction raster
  if(is.character(flowDirectionMap)){
    flowDirectionMap <- terra::rast(flowDirectionMap)
  }
  # Get dimensions of flow map
  xDim <- terra::res(flowToRoute)[1]
  yDim <- terra::res(flowToRoute)[2]
  # Create empty raster layer to add all the maps to
  #storage_adjusted <- rast(0, ext(lateralflowMap), resolution=res(lateralflowMap), crs = crs(lateralflowMap))
  # Set up a temporary storage raster
  storage_adjusted <- flowToRoute # create a storage map from flow storage map
  #terra::values(storage_adjusted) <- 0 # create a map with no value
  diagFlow <- 1 / sqrt(2)
  # The names of the different flow layers
  flowKey <- list("N" = list("north_flow",c(0, 1), 1),
                  "E" = list("east_flow",c(1, 0), 1),
                  "S" = list("south_flow",c(0, -1), 1),
                  "W" = list("west_flow",c(-1, 0), 1),
                  "NW" = list("northwest_flow",c(-1, 1), diagFlow),
                  "NE" = list("northeast_flow",c(1, 1), diagFlow),
                  "SE" = list("southeast_flow",c(1, -1), diagFlow),
                  "SW" = list("southwest_flow", c(-1, -1), diagFlow)
                  )

  # Loop through cardinal directions and create shifted storage maps
  #for(x in cardinal_directions){
    #print(paste0("Flow ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
  routeFlow <- function(x, flowToRoute, flowDirectionMap, xDim, yDim, flowKey){
    # Find the appropriate shift direction
    #shiftDir <- shiftValues[x]
    # Match the direction to the directional name within the flowStack
    directionofFlow <- flowKey[[x]][[1]]
    # Apply the shift to the dimensions of the raster
    xshift <- flowKey[[x]][[2]][[1]]*xDim
    yshift <- flowKey[[x]][[2]][[2]]*yDim
    # Shift the flow raster
    shiftStep <- terra::shift(flowToRoute, dx = xshift, dy = yshift)
    # Crop the raster
    flowShifted <- terra::crop(shiftStep, flowDirectionMap, snap = "near", extend = TRUE)
    # Select the appropriate layer
    flowDirection <- terra::subset(flowDirectionMap, subset = c(directionofFlow)) # issues with subsetting should be fixed
    # Determine the amount of water removed
    flowInAmount <- flowDirection * flowShifted
    flowInAmount <- terra::ifel(is.nan(flowInAmount), 0, flowInAmount)
#
    shiftBackStep <- terra::shift(flowInAmount, dx = -xshift, dy = -yshift)
    flowShiftedBack <- terra::crop(shiftBackStep, flowDirectionMap, snap = "near", extend = TRUE)
    flowShiftedBack <- terra::ifel(is.nan(flowShiftedBack), 0, flowShiftedBack)

    #Cumulative percentage
    flowFinal <- flowInAmount - flowShiftedBack
    ## Calculate the flow amount in a cardinal direction
    # Multiply the percent of flow from a direction by the amount of lateral flow storage in given direction
    flowAccumDirection <- terra::ifel(is.nan(flowFinal), 0, flowFinal)
    #print(paste0("Adding flow to temp variable ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
    #storage_adjusted <- storage_adjusted + flowAccumDirection
    #storage_adjusted <- c(storage_adjusted, flowAccumDirection)
    return(flowAccumDirection)
  }
  storage <- lapply(names(flowKey), FUN = routeFlow, flowToRoute, flowDirectionMap, xDim, yDim, flowKey)
  storageRaster <- terra::rast(storage)
  #checkSum <- sum(terra::values(storageRaster))

  # Sum adjusted storage layers
  #flowAmount <- sum(terra::values(flowToRoute))
  storage_adjusted <- round(sum(storageRaster),9) + flowToRoute
  #checkStorage <- sum(terra::values(storage_adjusted))
  #print(paste0("Total time elapsed ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
  return(storage_adjusted)
}
## Test flow routing function
#flowRouting(flowToRoute, flowDirectionMap)
# # # Make a map of 1s
# flowToRoute <- terra::ifel(terra::rast(file.path(WatershedElements, "stack_flow.tif"))[[1]] >= 0, 1, 0)
#
# flowMapPath <- file.path(WatershedElements, "stack_flow.tif")
#z <- flowRouting(flowToRoute, flowMapPath)
# testStorage <- rast(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\breached_clipped_dem.tif)")
# values(testStorage) <- 1.000001 #flowStack <- terra::wrap(flowStack) # the flow stack must be wrapped? before it is read in
# flowStack <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\stack_flow.tif)"
# subsurfaceTest <- flowRouting(flowToRoute = testStorage, flowDirectionMap = flowStack)

# -----------------------
# Function that adjusts the storage of water

# surfaceTest <- flowRouting(runoff, flowStack)
# Manning's Formula
##-----------------------
# Percent of total volume moved for a timestep
percentLength <- function(velocity, timestepSeconds, adjustmentRatio = 1, gridsize = 10){
  lengthMoved <- velocity * timestepSeconds / adjustmentRatio # m/s * seconds / adjustment
  percentMoved <- lengthMoved / gridsize # (m) /  (m)
  return(percentMoved)
}

##--------------------
# Wrapper function to route water in a given timestep
disperseWater <- function(surfaceStorage, runoffDepth, flowStack_file = file.path(WatershedElements, "stack_flow.tif")){
  # Adjusted surface water = previous surface water - amount of runoff + runoff added to cell
  newDepth <- surfaceStorage - runoffDepth + flowRouting(runoffDepth, flowDirectionMap = flowStack_file) # pass flow file location to avoid
  return(newDepth)
}
###---------------------------
# Function that calculates movement aspects of runoff
waterMovement <- function(surfaceStorage,
                          percentLengthMoved,
                          outFolder,
                          method = "percent",
                          distanceStoragePath = file.path(outFolder, "distanceStorage.tif"),
                          flowMapPath = file.path(outFolder, "flowMap.tif"),
                          runoffDepthPath = file.path(outFolder, "runoffDepth.tif"),
                          drainMapPath = file.path(outFolder, "drainMap.tif")
                          ){
  # Let's change the function to read in instead hopefully saving on memory
  tempFlowMap <- terra::rast(flowMapPath)
  tempDrainMap <- terra::rast(drainMapPath)

  #print(paste0("Maximum distance moved ", round(minmax(percentLengthMoved)[2],3)))
  tempDistanceStorage <- terra::rast(distanceStoragePath) + percentLengthMoved # once distance gets above 100% if moves
#plot(tempDistanceStorage)
  # If cells are moving the percentage of movement is based upon the length that has flowed into the next cell
  tempFlowMap <- terra::ifel(tempDistanceStorage >= 1, 1, tempFlowMap) # creates a map with values where move
  terra::writeRaster(tempFlowMap, flowMapPath, overwrite = T)

  # Overflow section - water will move based upon overflow or it has previously flowed and is draining
  #flowPercents <- terra::ifel((tempDistanceStorage -1 > 0), tempDistanceStorage - 1, 0)  # Depths of water that will move

  # Multiple the cells that can flow by their percents
  #amountFlow <- tempFlowMap * tempDistanceStorage
  # runoff <- percentLengthMoved * tempFlowMap


  # Drainage section
  #drainPercents <- terra::ifel(tempFlowMap > 0, percentLengthMoved, flowPercents)
  # tempDrainMap <- terra::ifel(tempDistanceStorage == 1, 1, tempDrainMap)
  # terra::writeRaster(tempDrainMap, drainMapPath, overwrite = T)

  # drainPercents <- tempDrainMap * percentLengthMoved
  #
  # flowPercents <- flowPercents + drainPercents

  # If flow map > 1, calculate the amount of movement
  if(method == "percent"){
    # Runoff depth - creates an parabolic nature
    # surfaceStorage (cm) | flowPercents (ratio < 1) |tempDistanceStorage (length/ ratio)
    runoffDepth <- surfaceStorage * tempFlowMap * percentLengthMoved
    #print(runoffDepth) # normalized to relative percentage
    # Adjusted soil distance storage
    tempDistanceStorage <- tempDistanceStorage - tempFlowMap * percentLengthMoved
    #print(tempDistanceStorage)
    # Overwrite values to disk
    terra::writeRaster(runoffDepth, runoffDepthPath, overwrite = T)
    terra::writeRaster(tempDistanceStorage, distanceStoragePath, overwrite = T)

  }else if(method == "velocity"){ # not used currently -- must update
    # move water if the flow Map overflows
    # runoffDepth <- terra::ifel(flowMap > 1, surfaceStorage * percentLengthMoved, 0)
    # # Adjusted soil distance storage
    # distanceStorage <- distanceStorage - flowPercents
  }
  #return(list(runoffDepth, distanceStorage)) # returns list of depths and distance storage
  # Don't return anything, being written to disk
}
# Test

# velocity <- function(n, depth, slope, length = 10){
#   # assumes metric units (meters)
#   # Q = A * 1/n * Rh ^2/3 * S ^.5
#   # S = Slope = gradient (m/m)
#   # Depth = Depth of surface water (in cm)
#   Area <- depth * length
#   #print(Area)
#   Q <- (Area) * (1/n) * (Area / (2*depth/100 + Area))^(2/3) * slope
#   V <- (1/n) * (Area / (2*depth/100 + length))^(2/3) * slope ^.5
#   return(V)
# }
# Test - discharge
# #light brush = 0.05
# dischargeTest <- velocity(n = 0.05, depth = 0.5, slope = 50)


# Time of concentration - overland flow
# Kerby-Kirpich method
# Overland flow
# Tov = Kov * (N (retardance coefficient)*Lov(overland flow length)^.467 * Slope (m/m))


## ---------------
# Function mannings velocity of a channel
# routeWater <- function(SoilStack,
#                        flowDirectionMap,
#                        throughfall,
#                        time_step = 10,
#                        length = 10){
#
#   if(is.character(SoilStack)){
#     SoilStack <- terra::rast(SoilStack)
#   }
#   # Calculate initial velocity - before rain added
#   #velocityInitial <- manningsVelocity(SoilStack$mannings_n,
#                                       # depth = SoilStack$surfaceWater,
#                                       # slope = SoilStack$slope,
#                                       # length = length)
#
#   velocityFinal <- velocityConverge(SoilStack,
#                                     throughfall = throughfall,
#                                     flowDirectionMap,
#                                     time_step = time_step,
#                                     length = length)
#
#   flowDirectionMap2 <- flowMap(SoilStack$model_dem + depthFinal)
#   # Change flow direction map to new surface elevations
#   flowNew <- flowMap(depthFinal)
#   depthChange <- depthFinal - depth
#   velocityFinal <- manningsVelocity(n, depthFinal, slope, length)
#   distanceFinal <- velocityFinal * time_step
#   depth_adjF <- depthFinal / 100
#   Qfinal <- depth_adjF * length * velocityFinal
#   maxDistanceF <- terra::minmax(distanceFinal)[2]
#   if(maxDistance < length/3){
#     # Calculate discharge
#
#
#     VolumePresent <- depth_adj * length^2
#     volumeLeft <- VolumePresent - VolumeLoss
#     lossPercent <- VolumeLoss/VolumePresent
#     # Move volume into next cells as depth
#     if(terra::minmax(volumeLeft)[1] >= 0){
#       # m^3/m^2 = m <- cm*100
#       depthLoss <- VolumeLoss / (length^2) * 100
#       # Check if the depth adjustment is equivalentg
#       depthTest <- terra::minmax(depth - depthLoss)[1] # minimum loss value
#       if(depthTest >= 0){
#         # Adjust depths
#         depthNew <- depth + flowRouting(flowToRoute = depthLoss, flowDirectionMap = flowDirectionMap)
#         sumCheck <- sum(terra::values(flowRouting(flowToRoute = depthLoss, flowDirectionMap = flowDirectionMap)))
#         runoffDepth <- sum(terra::values(depthLoss))
#       }
#       depthChange <- depth - depthNew
#     }
#   }
#   else{
#     adjustment <- ceiling(maxDistance / (length/3))
#
#   }
  # v0 <- velocity
  # timestep <- floor(terra::minmax(velocity)[2] / length) + 1
  # if(adjustVel){ # Adjust velocity based on time step necessary
  #   timeReduce <- time / dt
  #   for(x in 1:dt){
  #     depth_adj <- (1 - (v0 * timeReduce) / length) * depth_adj
  #     Area <- depth_adj * length # calculate cross sectional area
  #     HydraulicRadius <-  Area / (2* depth_adj + length) # calculate the hydraulic radius
  #     #HydraulicRadius <-  Area / (length) # calculate the hydraulic radius
  #     #latex Rh: R_{h} = \frac{(d_{water}*l_{grid})}{2*d_{water} + l_{grid}}
  #     slope_gradient <- tanpi(slope/180) # convert slope into a gradient (m/m)
  #     # LaTEX V(\frac{m}{s}) = \frac{1}{n}*R_{h}^{\frac{2}{3}} * S^{\frac{1}{2}}_{grad}
  #     v0 <- ((HydraulicRadius^ (2/3)) * (slope_gradient^.5)) / n
  #   }
  #   velocity <- mean(c(velocity, v0))
  # }
#   return(list(velocity, depthNew)) # return total discharge in m^3/s - assuming input units are correct
# }
# ReductionTest <- ManningsWideChannelVelocity(n = 0.05, depth = 100, slope = 30, length = 10)
# # Test
# ManningsWideChannelVelocity(n = 0.05, depth = 600, slope = .05, length = 10, adjustVel = F)
# vel <- ManningsWideChannelVelocity(n = 0.06, depth = 100, slope = 30, length = 10)
# ManningsWideChannelVelocity(n = 0.05, depth = .004, slope = 45, length = 10)
# ----------------------------- Manning's Channel Velocity
manningsVelocity <- function(n, depth, slope, length){
  depth_adj <- depth / 100 # convert depth in cm to meters
  Area <- depth_adj * length # calculate cross sectional area
  HydraulicRadius <-  Area / (2* depth_adj + length) # calculate the hydraulic radius
  #HydraulicRadius <-  Area / (length) # calculate the hydraulic radius
  #latex Rh: R_{h} = \frac{(d_{water}*l_{grid})}{2*d_{water} + l_{grid}}
  slope_gradient <- tanpi(slope/180) # convert slope into a gradient (m/m)
  # LaTEX V(\frac{m}{s}) = \frac{1}{n}*R_{h}^{\frac{2}{3}} * S^{\frac{1}{2}}_{grad}
  velocity <- ((HydraulicRadius^ (2/3)) * (slope_gradient^.5)) / n
  return(velocity)
}

# ---------------------------- Velocity determination
# Function that determines the velocity necessary for a given timeframe
# The function takes velocity variables and surface depth and throughfall
# determines when the differences between velocities are small
routeWater <- function(SoilStack, flowDirectionMap, time_step = 10, length = 10, timeVelocity = list(0,0), ...){
  if(is.character(flowDirectionMap)){
    flowDirectionMap <- terra::rast(flowDirectionMap)
  }
  # Intial variables
  maxTravel <- length/3
  n <- SoilStack$mannings_n
  surface <- SoilStack$surfaceWater # cm
  throughfall <- SoilStack$throughfall
  rainSurface <- surface + throughfall # cm
  slope <- SoilStack$slope
  dem <- SoilStack$model_dem # m

  # Initial velocity
  velocityInitial <- manningsVelocity(n, surface, slope, length = length)
  velocityFinal <- manningsVelocity(n, rainSurface, slope, length = length)
  velocityAverage <- (velocityInitial + velocityFinal) * .5
  velocityStorage <- velocityAverage

  # Time step check - returns max distance traveled and adjusted depth
  distanceDepth <- distanceCheck(velocityAverage, rainSurface, time_step, flowDirectionMap, dem, n, timeVelocity = timeVelocity)
  #print(distanceDepth)
  if(distanceDepth[[1]] > maxTravel){
    print("Reducing timestep")
    # Reduce time step
    timeAdjustment <- ceiling(distanceDepth[[1]] / maxTravel) + 1
    time_step <- time_step / timeAdjustment
    rain_adjust <- throughfall / timeAdjustment
    distanceDepth[[2]] <- surface + rain_adjust
    for(x in 1:timeAdjustment){
      #
      velocityAfterRain <- manningsVelocity(n,
                                            distanceDepth[[2]],
                                            slope,
                                            length = length)

      terra::add(velocityStorage) <- velocityAfterRain # add velocity
      # print(class(velocityAfterRain))
      # print(class(velocityStorage))
      # print(velocityStorage)
      # print(terra::mean(velocityStorage, na.rm = T))
      # print(names(velocityStorage))
      distanceDepth <- distanceCheck(velocityAfterRain, distanceDepth[[2]],
                                 time_step, flowDirectionMap, dem, n)
      if(x < timeAdjustment){
        distanceDepth[[2]] <- distanceDepth[[2]] + rain_adjust
      }
    }
  }
  if(!(terra::nlyr(velocityStorage) == 1)){
    velocityAverage <- terra::mean(velocityStorage, na.rm = T)
  }
  slopeNew <- slopeCalculate(dem + distanceDepth[[2]])
  flowNew <- flowMap(dem + distanceDepth[[2]])
  return(list(distanceDepth[[2]], velocityAverage, slopeNew, flowNew)) # returns the adjusted depth, velocity, new slope, new flowmap
}
# Test
#   #velocityAfterRain <- manningsVelocity(n, surface + throughfall, slope, length = length)
#   # Average velocity before and after rainfall
#   velocityAverage <- (velocityInitial + velocityAfterRain) * .5 # average initial and final velocities
#   depthAfterRain <- depthChange(velocityInitial,
#                                 surface + throughfall,
#                                 flowDirectionMap = flowDirectionMap,
#                                 time_step = time_step,
#                                 length = length)
#   # Adjust slope variable
#   flowMapPost <- flowMap(dem + depthAfterRain/100) # meters
#   slopePost <- slopeCalculate(dem + depthAfterRain/100) # meters
#   velocityPost <- manningsVelocity(n, depthAfterRain, slopePost, length = length)
#   # Determine the maximum distance flowed next time step
#   maxDistancePost <- terra::minmax(velocityPost)[2] * time_step
#   # If the next time step will flow to far without additional rainfall, reduce the time step
#   # for this iteration. This protects against some effects of rapidly filling cells
#   if(maxDistancePost > maxTravel){
#     # Reduce time-step by a factor
#     timeAdjustment <- ceiling(maxDistancePost / maxTravel)
#     time_step <- time_step / timeAdjustment
#     rain_adjust <- throughfall / timeAdjustment
#     velocityAfterRain <- manningsVelocity(SoilStack$mannings_n,
#                                           SoilStack$surfaceWater + rain_adjust,
#                                           SoilStack$slope, length = length)
#     depthAfterRain <- manningsVelocity()
#   }
#   # Route water for no rainfall added
#   depthNoRain <- depthChange(velocityInitial,
#                              SoilStack$surfaceWater,
#                              flowDirectionMap = flowDirectionMap,
#                              time_step = time_step,
#                              length = length)
#   velocityNoRain <- manningsVelocity(SoilStack$mannings_n, depthNoRain,
#                                      slope = SoilStack$slope,length = length)
#   velocityDifference <- velocityNoRain - velocityInitial
#   maxDistanceFinal <- terra::minmax(velocityNoRain)[2] * time_step
#   depth_adjusted <-  SoilStack$surfaceWater
#   maxDistanceFinal <- terra::minmax(velocityAfterRain)[2] * time_step
#   if(maxDistanceFinal > maxTravel){
#
#   }
# }

depthChange <- function(velocity, depth, time_step, flowDirectionMap, length = 10,...){
  depth_m <- depth / 100
  #distance <- velocity * time_step
  Q <- depth_m  * velocity * length
  volumeLoss <- Q * time_step # loss in volume - needs to be smaller than total volume
  volumeTotal <- depth_m * (length^2)
  volumeDiff <- volumeTotal - volumeLoss
  depthLoss <- volumeLoss / (length^2) * 100 # depth loss (cm)
  depthNormalized <- terra::ifel(depthLoss > depth, depth, depthLoss)
  # Move the water - no volume changed
  depthChanges <- depthNormalized - flowRouting(depthNormalized, flowDirectionMap)
  # XY
  secondCell <- drainCells[1,2:3]
  demValues <- values(dem)[secondCell]
  value <- cellFromXY(dem, secondCell)
  drainCells[1,2:3]
  depthFinal <- depth - depthChanges
  return(depthFinal)
}

slopeCalculate <- function(dem){
  slopeInitial <- terra::terrain(dem)
  slopeAdj <- terra::focal(slopeInitial, w = 3, "modal", na.policy = "only", na.rm = F)
  return(slopeAdj)
}

distanceCheck <- function(velocity, depth, time_step, flowDirectionMap, dem, n, length = 10,timeVelocity = data.frame(0,0), ...){
  #
  depth_change <- depthChange(velocity, depth, time_step, flowDirectionMap, length = length)
  #print(depth_change)
  slope_change <- slopeCalculate(depth_change/100 + dem)
  #print(slope_change)
  velocityNew <- manningsVelocity(n, depth_change, slope_change, length = length)
  #print(velocityNew)
  #maxVelocity <- terra::minmax(velocityNew)[2]
  maxVelocity <- terra::global(velocityNew, fun = quantile, probs = c(0.95), na.rm = T)[[1]] # 95% velocities
  # Super max velocity
  distanceTraveled <- maxVelocity * time_step
  timeVelocity <- rbind(timeVelocity,
                        list(tail(timeVelocity, 1)[[1]] + time_step/60,
                        maxVelocity))

  data.table::fwrite(data.table::data.table(timeVelocity), file = file.path(ModelFolder, "time-velocity.csv"))
  return(list(distanceTraveled, depth_change))
  # flow_map_change <- flowMap(depth_change/100 + dem)
  # depth_change_Final <- depthChange(velocityNew, depth_change, time_step, flowDirectionMap, length = length)
  # velocityFinal <- manningsVelocity(n, depth_change_Final, slope_change, length = 10)

  }

## ---------------------------- Carve channel function
# Using a DEM and a flow accumulation map, reduce the elevation of the channel by
# a linear amount
carveDem <- function(dem, flow_accum, depth = 1){
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  if(is.character(flow_accum)){
    flow_accum <- terra::rast(flow_accum)
  }
  # Extents don't match
  if(terra::ext(dem) != terra::ext(flow_accum)){
    flow_accum_adj <- terra::crop(flow_accum, dem)
  }else{
    flow_accum_adj <- flow_accum
  }
  maxAccumulation <- terra::minmax(flow_accum_adj)[2] # Maximum flow accumulation
  stepCarve <- depth/maxAccumulation # fraction to carve
  demCarve <- dem - stepCarve*flow_accum_adj # adjust dem
  return(demCarve)
}

# ## Volume change based on velocity - time step
# timestep <- 10 # time step in seconds
# depth <- 100 # cm
# slope <- 30 # in degrees
# length <- 10 # cell length (m)
# n <- .03 # manning's n
# # adj_length <- length/cos(slope*pi/180)
# # adj_length <- length/cospi(slope/180)
# # calculate velocity
# vel <- ManningsWideChannelVelocity(n, depth, slope, length) # a little high, but doesn't matter
# volume <- (depth / 100) * length^2
# area <-  depth / 100 * length # m^2
# Q <- vel * area
#
# # Let's solve the for the increase in decreasing depth
# depth <- 100:1
# slope <- 80 # in degrees
# length <- 10 # cell length (m)
# n <- .03 # manning's n
# velSave <- c()
# for(x in depth){
#   vel <- ManningsWideChannelVelocity(n, x, slope, length)
#   velSave <- c(velSave, vel)
# }
# ones <- 1:100
# q <- lm(ones ~ velSave)

#------------------ Froude Number
# Calculate the Froude number of a flow
# Fr = V^2 / g * h, where V is velocity (m/s), g = gravity (m/s^2), h = height of wave (m)
froudeNumber <- function(velocity, height){
  gravity <- 9.8 #m/s^2
  return(velocity^2 / (gravity * height))
}
# Test
# Fr <- froudeNumber(5, 3)
#
# # Function 1-D Kinematic Numerical Solution
# b <- 60 # m
# channel_length <- 7200 # m
# Qinit <- 57 #m3/s
# dt <- 180 # s
# deltax <- seq(900,7200,900) #
# So <- 0.01
# n <- 0.035
# alpha <- (n*b^(2/3)/sqrt(So))^.6
# beta <- 0.6
# tmin <- c(0,12,24,36,48,60,72,84,96,108,120)
# Qknown <- c(57,57,85,113,142,170,142,113,85,57,57)
# tint <- seq(0, 120, dt/60)
# Qint <-  approx(Qknown, n = length(tint))
# Q <- as.vector(Qint[[2]])
# Qinitial <- 57
# QDF <- data.frame(time = tint, discharge = Qint[2])
#
# plot(dischargeDF[,1], dischargeDF[,2])
# Qstore <- c()
#
# for(dx in deltax){
#   Qtop <- dt/dx * Q[2:41] + alpha * beta * ((Q[1:40] + Q[2:41])/2)^ (beta-1) * Q[1:40]
#   Qbot <- dt/dx + alpha * beta * ((Q[1:40] + Q[2:41])/2)^ (beta-1)
#   Q <- c(Qinitial, Qtop/Qbot)
#   Qstore <- cbind(Qstore, Q)
# }
# # Qtop <- dt/dx * QDF[2:41,2] + alpha * beta * ((QDF[1:40,2] + QDF[2:41,2])/2)^ (beta-1) * QDF[1:40,2]
# # Qbot <- dt/dx + alpha * beta * ((QDF[1:40,2] + QDF[2:41,2])/2)^ (beta-1)
# Qnext <- Qtop/Qbot
# Qstore <- Qnext
# Qstore <- cbind(tint, Qstore)
# Qstore <- data.frame(Qstore)
# plot
#
# library(reshape2)
# df <- reshape2::melt(Qstore, id.vars = "tint", variable.name = "discharge")
#
# ggplot(df, aes(tint, value)) + geom_line(aes(color = discharge))

## Store tests
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
# test_that("Flow maps are created", {
#   t1 <- c(1,1,1,1,2,1,1,1,1)
#   flow <- terra::rast(matrix(rep(1,9), nrow = 3, ncol = 3))
#   t1rast <- terra::rast(matrix(t1, nrow = 3, ncol = 3))
#   flowMap1 <- flowMap(t1rast)
#   flowNew <- flowRouting(flow, flowMap1)
#   flowAdjusted <- flow + flowNew
#   expect_equal(round(flowOutSum(t1),5), round(6.828427,5))
#   expect_equal(sum(terra::values(flowMap(t1rast)), na.rm = T), 1)
#   expect_equal(sum(values(flowRouting(flow, flowMap1))), 9)
#   expect_equal(flowAdjusted, 18)
#   expect_equal(sum(values(flowNew)), sum(values(flow)))
#
# })

