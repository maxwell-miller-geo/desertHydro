# Utility
# This script contains functions that contain functions that I don't know where else
# to put

## ---------------------------------- Wrapper finder function
stringMatch <- function(dataset, guessName = "Discharge", string = T){
  matchLocations <- grep(guessName, colnames(dataset), ignore.case = T) # index of guesses
  if(length(matchLocations) == 1){
    stringName <- colnames(dataset)[matchLocations]
  }else if(length(matchLocations > 1)){
    print("Found multiple matches with data sheet, using first match")
    stringName <- colnames(dataset)[matchLocations][1]
  }else{
    print("Could not find matches for input string: Please check input string")
    stringName <- matchLocations <- NA
  }
  if(string){
    return(stringName) # index of matching column
  }
  return(matchLocations[[1]])
}
# Test
# dataset <- streams
# columnName <- stringMatch(dataset, "Discharge") #
# reduced <- dataset[get(columnName) > 0]
# data.table::fwrite(reduced, file = file.path(WatershedElements, "example_discharge.csv"))

# Function that creates and plots linear model
quadratic_lm <- function(X, Y, poly = T){
  ones <- matrix(1, nrow = length(Y))
  Xd <- cbind(ones, X, X^2) # Polynomial regression
  # Solve Linear Regression with Matrices
  XX <- t(Xd) %*% Xd
  XY <- t(Xd) %*% Y
  B <- solve(XX) %*% XY
  Yhat <- Xd %*% B
  Yplot <- B[1] + B[2]*X + B[3]*X^2
  #ggplot()+geom_point(aes(X1,Y), color = "black")+ geom_line(aes(X1, Yplot), color = "red")
  return(B) # returns coefficients
}

# # Test function
# X <- rain_discharge$height
# Y <- rain_discharge$discharge
# quad_test <- quadratic_lm(X,Y)

# ---------------------------------
# Function that finds maximum values in layer, creates dataframe
dfMax <- function(raster, rename = NA, max = T, rank = 2){
  if(max){
    cellNumber <- terra::where.max(raster)[2] # gets max value cell number
    pos <-  terra::xyFromCell(raster, cellNumber) # gets xy
    maxValue <- terra::minmax(raster)[2] # maximum value of cell
  }else{
    cellValue <- sort(terra::values(raster), decreasing = T)[rank]
    cellNumber <- terra::cells(raster, cellValue)[[1]]
    pos <- terra::xyFromCell(raster, cellNumber)
    maxValue <- cellValue
  }

  if(is.na(rename)){
    name <- names(raster)
  }else{
    name <- rename
  }
  return(data.frame(time = name, x = pos[1], y = pos[2], value = maxValue, cell = cellNumber))
}
# Test
# raster <- surfaceStorage[[5]]
# dfMax(raster)

# ---------------------------- Function to find 1st and 2nd values
# and output table

firstSecond <- function(spatraster, dem, Outfolder = NA, name = ""){
  if(is.character(spatraster)){
    spatraster <- terra::rast(spatraster)
  }
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  maxCell <- dfMax(spatraster, rename = "max")
  secondCell <- dfMax(spatraster, rename = "second", max = F, rank = 2)
  combinedObject <- rbind(maxCell, secondCell)

  # Get the maximum elevation for each cell
  maxElevations <- terra::extract(dem, combinedObject[,2:3])
  outputDF <- cbind(combinedObject, maxElevations)
  # secondValue <- sort(terra::values(spatraster), decreasing = T, na.last = T)[2]
  # secondCellLocation <- terra::cells(spatraster, secondValue)[[1]] # should only be one flow accumulation
  # secondXYLocation <- terra::xyFromCell(spatraster, secondCellLocation)
  # second <- c("second", secondXYLocation[1], secondXYLocation[2], secondValue)

  #xy <- cbind(as.numeric(combinedObject[1,2:3]))
  # Get digital elevation values

  if(!is.na(Outfolder)){
    data.table::fwrite(outputDF, file = file.path(Outfolder, paste0(name,".csv")))
  }
  return(outputDF)
}
## ------------------------------ Retrive cell number
# Function to retrieve cell number with a dataframe - rigid
getCellNumber <- function(df, raster){
  emptyXY <- df[1, 2:3]
  emptyCell <- terra::cellFromXY(raster, emptyXY)
  dischargeXY <- df[2, 2:3]
  dischargeCell <- terra::cellFromXY(raster, dischargeXY)
  return(list(emptyCell, dischargeCell))
}
# ---------------------------------
# Function that creates vector file from data.frame
vectCreation <- function(df, saveLoc, name, coords){ # df must contain xy
  shapefile <- terra::vect(df, geom = c("x", "y"), crs = terra::crs(coords), keepgeom = T)
  terra::writeVector(shapefile, filename = file.path(saveLoc, name), overwrite = T)
  return(shapefile)
}

# Test
# vectName <- "depth_max.shp"
# vector_shapefile <- terra::vect(data_out, geom = c("x", "y"), crs = crs(SoilStack))
# writeVector(vector_shapefile, filename = file.path(ModelFolder, vectName))

#----------------------------------
# Function that reads vectors and turns them into DF see vectCreation
# Returns format: time | x | y | value
vectRead <- function(vect){

}
##-------------------------------
# Attach a layer and write to disk
writeLayer <- function(rasterStackPath, layer, layername){
  stack <- terra::rast(rasterStackPath)
  names(layer) <- layername
  terra::writeRaster(layer, rasterStackPath, gdal = "APPEND_SUBDATASET=YES")
}

# Test
# rasterStackPath <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\2012-07-15-full-t\Surface_Storage.tif)"
# layer <- terra::rast(rasterStackPath)[[2]]
# writeDisk(rasterStackPath, layer)


## # ------------------ Layer write
# Take a single layer - save with time variable in name
# Chunks the writing process of outputs - recompiles with another function
rasterWrite <- function(raster, ModelFolder, end_time, layername = "surface"){
  # raster layer
  time <- gsub("[.]", "-", c(end_time))
  name <- paste0(layername,"-time-",time,".tif")
  names(raster) <- end_time
  rasterPath <- file.path(ModelFolder, name)
  terra::writeRaster(raster, filename = rasterPath, overwrite = T)
  return(rasterPath)
}
# Test
# raster <- surface[[2]]
# end_time <- 1
# layername = "surface"
# rasterPath <- rasterWrite(raster, ModelFolder, end_time, layername = "surface")
# rasterPath2 <- rasterWrite(raster, ModelFolder, end_time+0.5, layername = "surface")
#
# raster1 <- rast(rasterPath)
# raster2 <- rast(rasterPath2)
## -------------------------- Combine rasters
# Function that takes a bunch of rasters and combined them by name
rasterCompile <- function(ModelFolder, layername, remove = F, time = T){ # layername must be present in folder
  # Find files with names and combined them by time
  tifFiles <- list.files(ModelFolder, pattern = "*.tif")
  # Layer files present
  if(time){
    layerFiles <- grep(paste0(layername,"-time-"), tifFiles, value = T)
  }else{
    layerFiles <- grep(paste0(layername,"-"), tifFiles, value = T)
  }

  # Converts strings into ordered dataframe
  orderedDF <- convert_string(layerFiles)
  #
  #print(orderedDF)
  # combine layers and save as combined stack
  layerStack <- terra::rast(file.path(ModelFolder, names(orderedDF)[1]))

  for(x in 2:length(orderedDF)){
    terra::add(layerStack) <- terra::rast(file.path(ModelFolder, names(orderedDF)[x]))
  }

  outStack <- file.path(ModelFolder, paste0(layername, "Storage.tif"))
  # Check if previously created stack
  if(file.exists(outStack)){
    if(terra::nlyr(terra::rast(outStack)) > 2){
      # Append the layer stack
      combined <- c(layerStack, terra::rast(outStack)+0)
      terra::writeRaster(combined, filename = outStack, overwrite = T)
    }
  }else{
    terra::writeRaster(layerStack, filename = outStack, overwrite = T)
  }
  terra::writeRaster(layerStack, filename = outStack, overwrite = T)
  print(paste0("Created ", layername, "Storage.tif"))
  # Remove temporary files
  fullPaths <- lapply(layerFiles, FUN = function(x)(file.path(ModelFolder, x)))
  # Remove temp files
  if(remove){
    lapply(fullPaths, file.remove)
  }
  return(outStack)
}
# Test
# testRaster <- rasterCompile(ModelFolder, "surface")
# Define a function to convert the strings
convert_string <- function(x) {
  # Use regular expression to find pattern "-number-number."
  pattern <- "([^e.]+)\\."
  # Times
  times <- stringr::str_extract(x, pattern)
  # Define pattern for matching
  pattern <- "-(\\d+)-?(\\d*)."

  # Replace matches using gsub
  timeDecimal <- as.numeric(gsub(pattern, "\\1.\\2", times))

  # Create a named list or vector to store the mapping
  converted_dict <- stats::setNames(timeDecimal, x)

  # Convert the dictionary to a data frame
  df <- data.frame(original_string = names(converted_dict),
                   converted_value = unname(converted_dict))
  # Sort the data frame based on the converted values
  df <- df[order(df$converted_value), ]
  # Convert the data frame back to a named vector
  sorted_dict <- stats::setNames(df$converted_value, df$original_string)
  return(sorted_dict)
}

## -----------------------------
# Function to check if file is present - returns combined string if file exists
filePresent <- function(filename, Folder){
  combined_path <- file.path(Folder, filename)
  if(!file.exists(combined_path)){
    stop(paste0("Could not locate '", filename, "' in: ",combined_path))
  }else{
    return(combined_path)
  }
}

### -------------------------------- Rainfall method
# Function to check for the rainfall file using rainfall method
rainfallMethodCheck <- function(ModelFolder, rainfall_method = "", rainfall_string = NA){
  if(!is.na(rainfall_string)){
    if(file.exists(rainfall_string)){
      rainfall <- rainfall_string
    }
  }else if(rainfall_method == "gauges" | rainfall_method == "synthetic"){
    rainfall <- filePresent("Model-Rainfall.csv", ModelFolder)
  }else if(rainfall_method == "spatial"){
    rainfall <- filePresent("Model-Spatial-Rainfall.csv", ModelFolder)
  }else if(rainfall_method == "goes"){
    rainfall <- filePresent("Goes-Summary.csv", ModelFolder)
  }
  return(rainfall)
}

## ------------------------Discharge of a cell
# Function that gets the depth of a cell (cm) and time step returns the discharge
# What about smaller time steps

## -------------------------------------- Check Watershed Elements Folder
#
#' Function to check and adjust the WatershedElements Folder
#' Wrapper function of system.file to check if the folder containing the model
#' elements exists and returns the path to the folder. Will default to package
#' elements if no file is found.
#' @param folder Folder path to check
#' @param example_file Default "dem.tif'. File name + extension to check for within folder.
#' @param demo boolean. If TRUE, will use demo elements folder if input folder is not found
#'
#' @return Directory or folder path. Demo folder if demo = T.
#' @export
#'
#' @examples
#' \dontrun{
#' folderPath <- "./WatershedElements"
#' WatershedElements <- folderCheck(folderPath, "dem.tif", demo = T)
#' WatershedElements
#' }
#' # Returns demo folder
folderCheck <- function(folder, example_file = "dem.tif", demo = T){
  firstFolder <- file.exists(file.path(folder, example_file))
  if(firstFolder){
    return(folder)
  }else if(file.exists(system.file("extdata", example_file, package = "desertHydro")) & demo){
    print(paste0("Could not find file in folder: ", folder))
    print("Using /extdata folder in desertHydro for WatershedElements.")
      exampleDir <- system.file("extdata", example_file, package = "desertHydro")
        folder <- dirname(exampleDir)
        return(folder)
  }else{
      stop(paste0("Could not find the '", example_file ,"' in the: '", folder,"' path"))
    }
}

# Create an object that contains all the information needed to perform a model run

