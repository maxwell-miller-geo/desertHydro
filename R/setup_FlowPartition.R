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
  adjustment <- 1/sqrt(2)
  diag_difference <- (center - diagonal[diagonal > 0]) * adjustment

  # Add the values together if they are above 0 (e.i., they flow into the current cell).
  # If the center is higher than a direction, it is not gaining flow from that particular direction
  flow_total <- sum(card_difference[card_difference > 0], na.rm = TRUE) +
    sum(diag_difference[diag_difference > 0], na.rm = TRUE)

  return(flow_total)
}
# # Test flow out sum
# testV <- c(1,2,3,4,5,6,7,8,9)
# inFlow <- flowOutSum(testV)
# outFlow <- flowOutSum(testV)
## Define a custom function to calculate the difference from the center - essentially flow accumulation within a neighborhood
# flowOutPercent <- function(x) { # Vector of values
#
#   center <- x[5]
#   if(center < 0  | is.na(center)){ # ignore values that are 0 or NA
#     return(0)
#   }
#   # Represent the N,E,W, and S values
#   cardinal <- x[c(2,4,6,8)]
#   #print(cardinal)
#   # Represent the NW, NE, SW, and SE values
#   diagonal <- x[c(1,3,7,9)]
#
#   # Direction key
#   direction <- list("NW" = 1,
#                   "N" = 2,
#                   "NE" = 3,
#                   "W" = 4,
#                   "E" = 6,
#                   "SW" = 7,
#                   "S" = 8,
#                   "SE" = 9)
#
#
#   # Take the difference between cardinal values and center cell - vector of elevation differences
#   card_difference <- center - cardinal[cardinal > 0]
#
#   # Take the difference between diagonal values and center cell
#   adjustment <- 1/sqrt(2)
#   diag_difference <- (center - diagonal[diagonal > 0]) * adjustment
#
#   # Keep the values above a certain threshold
#   # Add the values together if they are above 0 (e.i., they flow into the current cell).
#   # If the center is higher than a direction, it is not gaining flow from that particular direction
#   flow_out_total <- sum(card_difference[card_difference > 0], na.rm = TRUE) +
#     sum(diag_difference[diag_difference > 0], na.rm = TRUE)
#
#   # Normalize the flow out percentage
#   flow_percentage <-
#   return(flow_total)
# }
# flowOutPercent(testV)
## Flow Partitioning function- Percent flow
outputFlow <- function(kernelList, dir){
  directions <- list("NW" = list(1, .7071),
                     "N" = list(2, 1),
                     "NE" = list(3, .7071),
                     "W" = list(4, 1),
                     "E" = list(6, 1),
                     "SW" = list(7, .7071),
                     "S" = list(8, 1),
                     "SE" = list(9, .7071))
  print(list(kernelList))
  #return(values)
  # Take input direction and perform the percent calculation -- needs direction list
  flowPosition <- directions[[dir]][[1]] # find the vector value within the direction dictionary (NW = 1)
  scalar <- directions[[dir]][[2]] # find the scaling factor (1 for orthogonal, .707 for diagonal)
  centerElevation <- kernelList[14] # center value of the top layer
  flowElevation <- kernelList[flowPosition + 9]
  # cat(flowElevation, centerElevation, flowElevation)
  # print('Done')
  # Checks if center cell and cell in question have any flow accumulation at all
  #print(paste(values, flowPosition, flowElevation, centerElevation))
  if(is.na(centerElevation) || kernelList[flowPosition] <= 0 || kernelList[5] <= 0 || flowElevation <= 0 || centerElevation <= 0){
    return(0)
  }
  else{
    # Calculate the flow percent based on the difference is the elevation values and flow accumulation
    flowPercent <- (flowElevation - centerElevation)/ (kernelList[flowPosition])*scalar
    return(max(flowPercent, 0)) # return only positive values
  }
}

## Test for outputFlow script

## Function that takes the DEM input and brings it all together
# DEM -> Flow Sum Map (x1) -> Flow Partitioned Map(s) (x8) -> Stacked-Output
flow_Partition <- function(clipped_adj_dem, file_name_and_path = NA){
  # Create Flow sum map
  dem <- terra::rast(clipped_adj_dem)
  kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
  dem_flow_units <- terra::focal3D(dem, w = kernel, fun = flowOutSum, pad = TRUE) # calculate the 'flow accumulation'
  plot(dem)
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
    terra::writeRaster(flowStacked, file_name_and_path, overwrite = FALSE)
  }
  return(flowStacked)
}
# # Test for flow direction maps
exampleRast <- (matrix(c(5,2,1,5,3,2,6,4,3), ncol = 3))
plot(flow_Partition(exampleRast))
#flow_Partition(dem_raster, file_name_and_path = flow_file)

## Direction function takes in a vector of values and return a particular directions value
Direction <- function(values, dir){
  Position <- directions[[dir]][[1]]
  return(values[Position])
}
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
initializeRaster <- function(firstLayer, name, ModelFolder, zero = F){
  temp <- firstLayer
  names(temp) <- c(name)
  outFile <- file.path(ModelFolder, paste0(name, ".tif"))
  terra::writeRaster(temp, outFile, overwrite = T) # writes the initial raster
  return(outFile)
}
# Test

##---------------
# # Adding the water to the storage - very slow method
# subsurfaceFlow <- function(lateralFlow){
#   lat_flow <- lateralFlow$lateralFlow # grabbing flow layer from raster stack
#   kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
#   cardinal_directions <- c("N", "E", "W", "S", "NW", "NE", "SE", "SW")
#   outStack <- c() # create outputstack
#   for(x in cardinal_directions){
#     flow_direction <- terra::focal3D(lat_flow, w = kernel, fun = Direction, dir = x, fillvalue = 0)
#     names(flow_direction) <- c(paste0(x,"_lat_flow"))
#     outStack <- c(outStack, flow_direction)
#   }
#   return(outStack)
# }
# # Test of subsurface flow
# s_t <- Sys.time()
# lateral_flow_test <- subsurfaceFlow(SoilStack)
# tot_time <- Sys.time() - s_t
# tot_time

# Function to route water through the subsurface from lateral flow values and flow direction stack

flowRouting <- function(flowToRoute, flowDirectionMap, time = F){
  # Start time variable
  start <- Sys.time()
  # Load in flow direction raster
  flowDirectionMap <- terra::rast(flowDirectionMap)
  # Load in flow to route - not done
  if(is.character(flowToRoute)){
    flowToRoute <- terra::rast(flowToRoute)
  }


  # Get dimensions of flow map
  xDim <- res(flowToRoute)[1]
  yDim <- res(flowToRoute)[2]
  # Create empty raster layer to add all the maps to
  #storage_adjusted <- rast(0, ext(lateralflowMap), resolution=res(lateralflowMap), crs = crs(lateralflowMap))
  # Set up a temporary storage raster
  storage_adjusted <- flowToRoute # create a storage map from flow storage map
  terra::values(storage_adjusted) <- 0 # create a map with no values
  # Create mini function to multiple layers

  # Shift dictionary/list - shift the map in the opposite direction of intended

  shiftValues <- list( # c(xshift, yshift)
                      "N" = c(0, -1),
                      "E" = c(-1, 0),
                      "S" = c(0, 1),
                      "W" = c(1, 0),
                      "NW" = c(1, -1),
                      "NE" = c(-1, -1),
                      "SE" = c(-1, 1),
                      "SW" = c(1, 1)
  )
  # The names of the different flow layers
  flowKey <- list("N" = "north_flow",
                  "E" = "east_flow",
                  "S" = "south_flow",
                  "W" = "west_flow",
                  "NW" = "northwest_flow",
                  "NE" = "northeast_flow",
                  "SE" = "southeast_flow",
                  "SW" = "southwest_flow"
                  )

  cardinal_directions <- c("N", "E", "S", "W", "NW", "NE", "SE", "SW")
  # Loop through cardinal directions and create shifted storage maps
  for(x in cardinal_directions){
    #print(paste0("Flow ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
    # Find the appropriate shift direction
    shiftDir <- shiftValues[x]
    # Match the direction to the directional name within the flowStack
    directionofFlow <- flowKey[[x]]
    # Apply the shift to the dimensions of the raster
    xshift <- shiftDir[[1]][1]*xDim
    yshift <- shiftDir[[1]][2]*yDim

    # Shift the raster
    shiftStep <- terra::shift(flowToRoute, dx = xshift, dy = yshift)
    # Crop the raster
    flowShifted <- terra::crop(shiftStep, flowToRoute, snap = "near", extend = TRUE)
    # Select the appropriate layer
    flowDirection <- terra::subset(flowDirectionMap, subset = c(directionofFlow)) # issues with subsetting should be fixed

    # Then multiply rasters with lapp
    #flowPercentage <- terra::lapp(stack_Rasters, fun = function(x,y){return(x*y)}) # Not faster..
    #flowPercentage1 <- stack_Rasters[[1]] * stack_Rasters[[2]]
    # Determine the amount of water added
    flowPercentage <- flowDirection * flowShifted

    ## Calculate the flow amount in a cardinal direction
    # Multiply the percent of flow from a direction by the amount of lateral flow storage in given direction
    flowAccumDirection <- terra::ifel(is.na(flowPercentage), 0, flowPercentage)
    #print(paste0("Adding flow to temp variable ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
    #storage_adjusted <- storage_adjusted + flowAccumDirection
    storage_adjusted <- c(storage_adjusted, flowAccumDirection)
  }
  # Sum adjusted storage layers
  storage_adjusted <- sum(storage_adjusted)
  if(time){
    print(paste("Time to shift water", as.numeric(Sys.time() - start)))
  }
  #print(paste0("Total time elapsed ", x, ": time delta: ", round(as.numeric(Sys.time() - start),2)))
  return(storage_adjusted)
}
## Test flow routing function
# # Make a map of 1s
# flowToRoute <- file.path(ModelFolder, "slope.tif")
# flowMapPath <- file.path(WatershedElements, "stack_flow.tif")
# flowRouting(flowToRoute, flowMapPath)
# testStorage <- rast(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\breached_clipped_dem.tif)")
# values(testStorage) <- 1.000001 #flowStack <- terra::wrap(flowStack) # the flow stack must be wrapped? before it is read in
# flowStack <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\stack_flow.tif)"
# subsurfaceTest <- flowRouting(flowToRoute = testStorage, flowDirectionMap = flowStack)


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
  #return(tempDistanceStorage)
  # If cells are moving the percentage of movement is based upon the length that has flowed into the next cell
  tempFlowMap <- terra::ifel(tempDistanceStorage > 1, tempFlowMap + 1, tempFlowMap) # creates a map with values where move
  terra::writeRaster(tempFlowMap, flowMapPath, overwrite = T)
  # Overflow section - water will move based upon overflow or it has previously flowed and is draining
  flowPercents <- terra::ifel(tempDistanceStorage > 1, tempDistanceStorage - 1, 0)  # Depths of water that will move

  # Drainage section
  #drainPercents <- terra::ifel(tempFlowMap > 0, percentLengthMoved, flowPercents)
  tempDrainMap <- terra::ifel(tempDistanceStorage == 1, 1, tempDrainMap)
  terra::writeRaster(tempDrainMap, drainMapPath, overwrite = T)

  drainPercents <- tempDrainMap * percentLengthMoved
  flowPercents <- flowPercents + drainPercents
  # If flow map > 1, calculate the amount of movement
  if(method == "percent"){
    # Runoff depth - creates an parabolic nature
    runoffDepth <- flowPercents / tempDistanceStorage * surfaceStorage / 10# normalized to relative percentage
    # Adjusted soil distance storage
    tempDistanceStorage <- tempDistanceStorage - flowPercents
    # Overwrite values to disk
    terra::writeRaster(runoffDepth, runoffDepthPath, overwrite = T)
    terra::writeRaster(tempDistanceStorage, distanceStoragePath, overwrite = T)

  }else if(method == "velocity"){ # not used currently -- must update
    # move water if the flow Map overflows
    runoffDepth <- terra::ifel(flowMap > 1, surfaceStorage * percentLengthMoved, 0)
    # Adjusted soil distance storage
    distanceStorage <- distanceStorage - flowPercents
  }
  #return(list(runoffDepth, distanceStorage)) # returns list of depths and distance storage
  # Don't return anything, being written to disk
}
# Test

velocity <- function(n, depth, slope, length = 10){
  # assumes metric units (meters)
  # Q = A * 1/n * Rh ^2/3 * S ^.5
  # S = Slope = gradient (m/m)
  # Depth = Depth of surface water (in cm)
  Area <- depth * length
  #print(Area)
  Q <- (Area) * (1/n) * (Area / (2*depth/100 + Area))^(2/3) * slope
  V <- (1/n) * (Area / (2*depth/100 + length))^(2/3) * slope ^.5
  return(V)
}
# Test - discharge
# #light brush = 0.05
# dischargeTest <- velocity(n = 0.05, depth = 0.5, slope = 50)


# Time of concentration - overland flow
# Kerby-Kirpich method
# Overland flow
# Tov = Kov * (N (retardance coefficient)*Lov(overland flow length)^.467 * Slope (m/m))


## ---------------
# Function mannings velocity of a channel
ManningsWideChannelVelocity <- function(n, depth, slope, length){
  # If a channel is wide - 20x the flow depth R = y
  depth_adj <- depth / 100 # convert depth in cm to meters
  Area <- depth_adj * length # calculate cross sectional area
  HydraulicRadius <-  Area / (2* depth_adj + length) # calculate the hydraulic radius
  #latex Rh: R_{h} = \frac{(d_{water}*l_{grid})}{2*d_{water} + l_{grid}}
  slope_gradient <- tanpi(slope/180) # convert slope into a gradient (m/m)
  # LaTEX V(\frac{m}{s}) = \frac{1}{n}*R_{h}^{\frac{2}{3}} * S^{\frac{1}{2}}_{grad}
  velocity <- ((HydraulicRadius^ (2/3)) * (slope_gradient^.5)) / n # R^2/3 * S^1/2 / n * Area = V * A
  #discharge <- (HydraulicRadius^(2/3)) * (slope_gradient^.5) / n * Area # R^2/3 * S^1/2 / n * Area = V * A
  return(velocity) # return total discharge in m^3/s - assuming input units are correct
}
# Test
#ManningsWideChannelVelocity(n = 0.03, depth = 1, slope = 80, length = 10)
# ManningsWideChannelVelocity(n = 0.06, depth = 10, slope = 49, length = 10)
# ManningsWideChannelVelocity(n = 0.05, depth = .004, slope = 45, length = 10)


dischargeCalc <- function(velocity, depth, length){
  return(velocity*depth*length)
}

