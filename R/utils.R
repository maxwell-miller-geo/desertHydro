# Utility
# This script contains functions that contain functions that I don't know where else
# to put

## ---------------------------------- Wrapper finder function
stringMatch <- function(dataset, guessName = "Discharge", string = T, multi = F){
  matchLocations <- grep(guessName, colnames(dataset), ignore.case = T) # index of guesses
  if(length(matchLocations) == 1){
    stringName <- colnames(dataset)[matchLocations]
  }else if(length(matchLocations > 1)){
    if(multi){
      stringName <- colnames(dataset)[matchLocations]
      return(stringName)
    }
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

outFlowCells <- function(dem, flow_accum, Outfolder = NA, name = ""){
  if(is.character(flow_accum)){
    flow_accum <- terra::rast(flow_accum)
  }
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  maxCell <- dfMax(flow_accum, rename = "max")
  secondCell <- dfMax(flow_accum, rename = "second", max = F, rank = 2)
  combinedObject <- rbind(maxCell, secondCell)

  # Get the maximum elevation for each cell
  maxElevations <- terra::extract(dem, combinedObject[,2:3])
  outputDF <- cbind(combinedObject, maxElevations)
  # secondValue <- sort(terra::values(flow_accum), decreasing = T, na.last = T)[2]
  # secondCellLocation <- terra::cells(flow_accum, secondValue)[[1]] # should only be one flow accumulation
  # secondXYLocation <- terra::xyFromCell(flow_accum, secondCellLocation)
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
# Chunks the writing process of outputs - recompile with another function
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
#
#' Function that takes a bunch of rasters and combined them by name and time
#'
#' @param ModelFolder path to folder containing files
#' @param layername string of layername that will combine files. e.g. "velocity"
#' @param remove T/F. If True will remove all of the separate files
#' @param time T/F. Default True will sort files in order of time.
#' Expects format "layername-time-time.tif"
#'
#' @return Returns combined raster stack of named files
#' @export
#'
#' @examples \dontrun{
#' # Format of function
#' rasterStack <- rasterCompile("Test-Folder", "velocity", remove = T)
#' }
rasterCompile <- function(ModelFolder, layername, remove = T, time = T, overwrite = T, end = T){ # layername must be present in folder
  # Find files with names and combined them by time
  tifFiles <- list.files(ModelFolder, pattern = "*.tif")
  # Layer files present
  if(time){
    layerFiles <- grep(paste0(layername,"-time-"), tifFiles, value = T)
  }else{
    layerFiles <- grep(paste0(layername,"-"), tifFiles, value = T)
  }
  # if(length(layerFiles) == 1){
  #   if(file.exists(outT))
  #   stop("Only 1 layer file found. Please check the duration of the simulation.")
  #}
  # Converts strings into ordered dataframe
  orderedDF <- convert_string(layerFiles)
  #print(orderedDF)
  # combine layers and save as combined stack
  layerStack <- terra::rast(file.path(ModelFolder, names(orderedDF)[1]))
  if(length(orderedDF) > 1){
    for(x in 2:length(orderedDF)){
      terra::add(layerStack) <- terra::rast(file.path(ModelFolder, names(orderedDF)[x]))
    }
  }

  # Check previously created stack
  outStack <- file.path(ModelFolder, paste0(layername, "Storage.tif"))
  if(file.exists(outStack)){
      # Append the layer stack - not cleverly
      combined <- c(terra::rast(outStack) + 0, layerStack)
      terra::writeRaster(combined, filename = outStack, overwrite = T)
  }else{
    terra::writeRaster(layerStack, filename = outStack, overwrite = T)
  }

  #print(paste0("Created ", layername, "Storage.tif"))
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
    # Look for goes
    guesses <- grep("goes.tif", list.files(ModelFolder), ignore.case = T, value = T)
    if(length(guesses) != 0){
      rainfall <- guesses[which.min(nchar(guesses))] # Select the shortest file.
    }else{
      stop(cat("Could not find the GOES tif file in the Model Folder."))
    }
    rainfall <- filePresent(rainfall, ModelFolder)
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
# TBD

# Check that all files are present to run the model
# Model dem, model soil stack, drainCells,
file_check <- function(file_list){
  filesCheck <- sapply(file_list, FUN = file.exists)
  if(!all(filesCheck)){
    print("All files are not found. Here is a list of the files present.")
    print(filesCheck)
    stop("Please check input files are present.")
  }else{
    return(NULL)
  }
}


## Polygonize using terra
#' Wrapper function to dissolve and create vector of raster layer
#'
#' @param raster_name Raster name, e.i. "dem.tif"
#' @param path file path to raster
#'
#' @return return vector string and saves output to raster path
#' @export
#'
#'
polygonize <- function(raster_name, path){
  poly <- terra::aggregate(terra::as.polygons(terra::rast(file.path(path, raster_name))))
  poly_name <- paste0(strsplit(raster_name, ".", fixed = TRUE)[[1]][[1]], "-poly.shp")
  poly_path <- file.path(path, poly_name)
  terra::writeVector(poly, poly_path, overwrite = T)
  return(poly_name)
}

file_removal <- function(path, overwrite){
  if(file.exists(path) & overwrite){
    print(paste("Overwritting", path))
    file.remove(path)
  }
}


# Fill a cell on the edge given cell number and raster
fill_edge <- function(raster, cellsToFill, dontFillCells, fill_amount = .0001){
  for(cellnumber in cellsToFill){
    cellnumber <- as.numeric(cellnumber)
    if(!(cellnumber %in% dontFillCells)){ # fill all cells but "dontFillCells"
    # xy location
    xy <- terra::xyFromCell(raster, cellnumber)
    column <- terra::colFromCell(raster, cellnumber)
    row <- terra::rowFromCell(raster, cellnumber)
    dem_value <- raster[cellnumber]
    x <- terra::res(raster)[1]
    y <- terra::res(raster)[1]

    # Find all values around cellnumber
    flowKey <- list("N" = list(0, 1),
                    "E" = list(1, 0),
                    "S" = list(0, -1),
                    "W" = list(-1, 0),
                    "NW" = list(-1, 1),
                    "NE" = list(1, 1),
                    "SE" = list(1, -1),
                    "SW" = list(-1, -1)
    )
    df <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("cell", "value"))))
    # Loop over values
    for(z in flowKey){
      # Subtract from one value
      newCell <- matrix(c(xy[1] + x*z[[1]], xy[2] + y*z[[2]]), nrow = 1)
      # Get cell
      newCellNumber <- terra::cellFromXY(raster, newCell)
      # Cell value
      newRasterValue <- raster[newCellNumber]
      if(!is.na(newCellNumber)){
        newRow <- data.frame(cell = newCellNumber, value = newRasterValue[1])
        df <- rbind(df, newRow)
      }
    }
    new_elevation <- min(df[,2], na.rm = T) + fill_amount
    raster[row, column] <- new_elevation
      }
    }
  return(raster)
}

# Determine the coordinate system of a tif file
get_crs <- function(raster_path, raster = T){
  if(raster & is.character(raster_path)){
      return(paste0("epsg:",terra::crs(terra::rast(raster_path), describe = T)[[3]]))
  }else if(!raster & is.character(raster_path)){
    return(paste0("epsg:",terra::crs(terra::vect(raster_path), describe = T)[[3]]))
  }else{
    return(paste0("epsg:",terra::crs(raster_path, describe = T)[[3]]))
  }
}

# Estimate slope based on elevation and pre-existing slope map
slope_edge <- function(dem, slope, cellsize, cpp = F){
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  if(cpp){
    gradient_edge <- terra::focalCpp(dem, w = c(3,3), fun = gradientCpp, fillvalue = NA)
  }else{
    #gradient_edge <- terra::focal(dem, fun = gradient, fillvalue = NA)
    gradient_edge <- terra::focal(dem, fun = function(x) gradient(x, cellsize), fillvalue = NA)

  }

  slope_edges <- gradient_edge / cellsize
  slope_total <- terra::merge(slope, slope_edges, first = T)
  return(slope_total)
}
gradient <- function(x, cellsize){
  center <- x[5]
  if(is.na(center)) return(NA)

  min_val <- min(x, na.rm = TRUE)

  if(min_val == center){
    min_val <- max(x, na.rm = TRUE)
    dh <- min_val - center
  } else {
    dh <- center - min_val
  }

  index <- which(x %in% min_val)[1]

  # Correct: use real-world dx based on direction
  if(index %% 2 == 0){
    dx <- cellsize            # direct neighbors
  } else {
    dx <- sqrt(2) * cellsize  # diagonal neighbors
  }

  return(dh / dx)
}
# function to calculate the unitless gradient from minimum direction
# gradient <- function(x){
#   # Center cell
#   center <- x[5]
#   if(is.na(center)){
#     return(NA)
#   }
#   # passes 9 numbers from top left to bottom right
#   minimum_elevation <- min(x, na.rm = T)
#
#   if(minimum_elevation == center){ # find middle number in center
#     minimum_elevation <- max(x, na.rm = T)
#     dh <- minimum_elevation - center
#   }else{
#     dh <- center - minimum_elevation
#   }
#   # if(is.infinite(dh) | is.na(dh)){
#   #   return(NA)
#   # }
#   index <- which(x %in% minimum_elevation)[[1]]
#   # if(length(which(x %in% minimum)) > 1){
#   #   # sample on direction for slope if two minimums found
#   #   index <- sample(minimum, 1)
#   # }else{
#   #   index <- which(x %in% minimum)
#   # }
#
#   if(index %% 2 == 0){
#     gradient <- dh / 1
#   }else{
#     gradient <- dh / sqrt(2)
#   }
#   return(gradient)
# }

Rcpp::cppFunction('
  NumericVector gradientCpp(NumericVector x, size_t ni, size_t nw) {
    NumericVector grad(ni); // one gradient per center cell

    for (size_t i = 0; i < ni; i++) {
      size_t start = i * nw;
      double center = x[start + 4]; // center of 3x3 window

      if (NumericVector::is_na(center)) {
        grad[i] = NA_REAL;
        continue;
      }

      // Find minimum value
      double minVal = R_PosInf;
      for (size_t j = 0; j < nw; j++) {
        double val = x[start + j];
        if (!NumericVector::is_na(val) && val < minVal) {
          minVal = val;
        }
      }

      // Compute dh
      double dh;
      if (minVal == center) {
        // Switch to max if center is min
        double maxVal = R_NegInf;
        for (size_t j = 0; j < nw; j++) {
          double val = x[start + j];
          if (!NumericVector::is_na(val) && val > maxVal) {
            maxVal = val;
          }
        }
        dh = maxVal - center;
      } else {
        dh = center - minVal;
      }

      // Get index of first occurrence of minVal
      int index = 0;
      for (size_t j = 0; j < nw; j++) {
        if (x[start + j] == minVal) {
          index = j;
          break;
        }
      }

      // Compute gradient
      grad[i] = (index % 2 == 0) ? (dh / sqrt(2.0)) : (dh / 1.0);
    }

    return grad;
  }
')


# Rcpp::cppFunction('
#   NumericVector gradientCpp(NumericVector x, size_t ni, size_t nw) {
#     Rcout << "The is NA: " << NumericVector::is_na(x[4]) << std::endl;
#     if (NumericVector::is_na(x[4])) return NA_REAL;  // center of 3x3 window
#
#     double center = x[4];
#     double minVal = R_PosInf;
#     int minIndex = -1;
#     int countMin = 0;
#     Rcout << "The NumericVector is: " << x << std::endl;
#     Rcout << "The size_t is ni: " << ni << std::endl;
#     Rcout << "The size_t nw is: " << nw << std::endl;
#
#     // Find minimum value in the window
#     for (int i = 0; i < nw; i++) {
#       if (!NumericVector::is_na(x[i]) && x[i] < minVal) {
#         minVal = x[i];
#         minIndex = i;
#         countMin = 1;
#       } else if (!NumericVector::is_na(x[i]) && x[i] == minVal) {
#         countMin++;
#       }
#     }
#
#     double dh;
#     if (minVal == center) {
#       // Center is minimum — switch to max
#       double maxVal = R_NegInf;
#       for (int i = 0; i < nw; i++) {
#         if (!NumericVector::is_na(x[i]) && x[i] > maxVal) {
#           maxVal = x[i];
#           minIndex = i;
#         }
#       }
#       dh = maxVal - center;
#     } else {
#       dh = center - minVal;
#     }
#
#     if (!R_finite(dh) || NumericVector::is_na(dh)) return NA_REAL;
#
#     // Deterministic min selection (first match)
#     if (countMin > 1 && minVal != center) {
#       for (int i = 0; i < nw; i++) {
#         if (x[i] == minVal) {
#           minIndex = i;
#           break;
#         }
#       }
#     }
#
#     // Distance: diagonal = sqrt(2), edge = 1
#     double dist = (minIndex % 2 == 0) ? sqrt(2.0) : 1.0;
#     return dh / dist;
#   }
# ')

# Sum the values of all of the cells within a raster layer
sumCells <- function(raster){
  return(terra::global(raster, "sum", na.rm = TRUE)$sum)
  #return(sum(terra::values(raster, na.rm = T)))
}

# Determine the number of cells with values
cellsWithValues <- function(raster){
  n <- terra::ncell(raster)
  na <- sum(terra::values(anyNA(raster)))
  return(n - na)
}
# Extracts the time from a raster stack names - assumes minutes
extract_time <- function(rasterStack, time = "mins"){
  numeric_vector <- as.numeric(names(rasterStack))
  # Calculate difference
  time_difference <- diff(numeric_vector)*60 # minutes to seconds
  first_time <- time_difference[1] # makes the assumption 1st time-step = 2nd
  return(c(first_time, time_difference))
}

# Find the location of maximum cell value in a raster and the cell number - return as vector
max_in_raster <- function(raster){
  cellNumber <- terra::where.max(raster)[2] # gets max value cell number
  maxValue <- terra::minmax(raster)[2] # maximum value of cell
  # return max value followed by cell value
  return(c(maxValue, cellNumber))
}

#' Find the cell numbers in a raster from a given shapefile/vector.
#' This function is basically a wrapper for "cellFromXY" within the terra package
#'
#' @param vect shapefile
#' @param raster raster
#'
#' @return cell numbers for points within shapefile
#' @export
#'
#' @examples /dontrun{
#' cell_numbers <- getCellCoords(vector, raster)
#' }
#'
getCellCoords <- function(vect, raster){
  coords <- matrix(terra::geom(vect)[,3:4], ncol = 2)
  return(terra::cellFromXY(raster, coords))
}

# Mass create gifs
get_folders <- function(parent_folder){
  folders <- list.dirs(parent_folder)
  return(folders[2:length(folders)])
}

# Get the dates within a csv file
get_dates <- function(dataframe, date_key = "time"){
  # Read in as data.table
  if(is.character(dataframe)){
    dt <- data.table::fread(dataframe)
  }else{
    dt <- dataframe
  }
  # Get names of column headers
  time_index <- grep(date_key, colnames(dt), value = T) # Returns first column name with "time"
  date_vector <- unique(as.Date(dt[[time_index]]))
  return(date_vector)
}
# Check the path of the file in input folders
check_path <- function(filepath, path, path2 = ""){
  new_path <- file.path(filepath)
  # Checks if file exists in 3 input paths
  if(!file.exists(filepath)){
    new_path <- file.path(path, filepath)
    if(!file.exists(new_path)){
      new_path <- file.path(path2, filepath)
      if(!file.exists(new_path)){
        stop(paste("Could not find file path: ", filepath,"in \n",path,"\n",path2))
      }
    }
  }
  return(new_path)
}

# Find the difference between to digital elevation models
rast_diff <- function(x, raster){
  if(class(x) != "SpatRaster"){
    x <- terra::rast(x)
  }
  if(class(raster) != "SpatRaster"){
    raster <- terra::rast(raster)
  }
  elev_adj <- x - raster
  values_adj <- terra::values(elev_adj, na.rm = T)
  dem_adjustments <- values_adj[! values_adj %in% 0]
  cells_adjusted <- length(dem_adjustments)
  percent_total <- round(cells_adjusted / length(values_adj)*100,3)
  range <- range(dem_adjustments)
  majority_qs <- quantile(dem_adjustments, c(0.1, .5, .9))
  #histor <- hist(dem_adjustments)
  cat("Total Cells adjusted:", cells_adjusted, "\nPercent of Cells adjusted (%):", percent_total, "\n")
  cat("Elevation adjustment range (m):\n", "Carved:", range[1], "\n", "Filled:", range[2],"\n")
  cat("dem_adjustments by Quantile: \n")
  print(paste(majority_qs, "\n"))

  hist(dem_adjustments, main = paste("Histogram of elevation adjustments (m)"), xlab = "Adjustments (m)")
  return(elev_adj)
}

# Remove null values from a list
remove_nulls <- function(my_list){
  cleaned_list <- my_list[!sapply(my_list, function(x) length(x) == 0 || is.null(x))]
  return(cleaned_list)
}

beautify <- function(string){
  remove <- stringr::str_replace_all(string, "_", " ")
  capitalize <- capwords(remove)
  ## and the better, more sophisticated version:
  return(capitalize)
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

get_start_end_time <- function(table, time_col = "time", data_col = "Total_in", timezone = "America/Phoenix"){
  if(is.character(table)){
    table <- data.table::fread(table)
  }
  if(is.null(table)){
    return(NULL)
  }
  if(inherits(table, "SpatRaster")){
    # Assumes time proceeds forwards
    starts <- as.POSIXct(names(table)[1], tz = timezone)
    ends <- as.POSIXct(names(table)[terra::nlyr(table)], tz = timezone)
    return(data.frame(start = starts, end = ends))
  }
  timeIndex <- grep(time_col, colnames(table), ignore.case = T)[[1]]
  dataIndex <- grep(data_col, colnames(table), ignore.case = T)[[1]]

  start <- head(which(table[[dataIndex]] != 0), 1)
  end <- tail(which(table[[dataIndex]] != 0), 1)

  start_t <- lubridate::force_tz(table[[timeIndex]][start], timezone)
  end_t <- lubridate::force_tz(table[[timeIndex]][end], timezone)

  # Return start and end time as list
  start_end <- data.frame(start = start_t,
                    end = end_t)
  return(start_end)
}

order_by_time <- function(file_list) {
  # Extract the observation start time (YYYYDDDHHMMSSS)
  extract_start_time <- function(filename) {
    matches <- regmatches(filename, regexpr("s\\d{4}\\d{3}\\d{6}", filename))
    sub("s", "", matches) # Remove the 's' prefix
  }

  # Extract start times for all files
  start_times <- sapply(file_list, extract_start_time)

  # Convert start times to a sortable format
  sortable_times <- as.numeric(start_times)

  # Order the file list by start times
  ordered_list <- file_list[order(sortable_times)]

  return(ordered_list)
}

copy_if_exists <- function(source_file, destination_folder) {
  # Check if the file exists
  if (file.exists(source_file)) {
    # Create the destination folder if it doesn't exist
    if (!dir.exists(destination_folder)) {
      dir.create(destination_folder, recursive = TRUE)
    }

    # Define the destination path for the file
    destination_file <- file.path(destination_folder, basename(source_file))

    # Copy file over
    file.copy(source_file, destination_folder, overwrite = T)
    return(T)
  }else{
    return(F)
  }
}

# Folder path
find_completed_models <- function(parent_folder = NULL, file = "ModelComplete.txt"){
  if(is.null(parent_folder)){
    parent_folder <- getwd()
  }
  folders <- list.dirs(parent_folder)
  boolean <- sapply(folders, function(x) file.exists(file.path(x, file)))
  return(folders[boolean])
}

# Check the dem projection
get_utm_epsg <- function(raster, datum = "NAD83") {
  if (is.character(raster)) {
    raster <- terra::rast(raster)
  }

  # Get the current CRS of the raster
  current_crs <- terra::crs(raster, proj=T)[1]
  #current_crs <- terra::crs(raster, describe = T)[[3]]
  # Check if it is projected in a dirty way
  check_projected <- substr(current_crs,7,9)
  # Check if the CRS is already projected
  if (check_projected == "utm") {
    message("The raster is already in a projected coordinate system.")
    # Optionally, return the current EPSG code if available
    epsg_code <- terra::crs(raster, describe = T)[[3]]
    return(paste0("EPSG:", epsg_code))
  }

  # If the raster is not projected, proceed to determine the appropriate UTM zone
  ext <- terra::ext(raster)
  lon_center <- mean(c(ext$xmin, ext$xmax))
  lat_center <- mean(c(ext$ymin, ext$ymax))

  # Determine UTM zone
  zone <- floor((lon_center + 180) / 6) + 1

  # Determine hemisphere
  hemisphere <- if (lat_center >= 0) "north" else "south"

  # Determine EPSG code based on datum and hemisphere
  if (datum == "NAD83") {
    if (hemisphere == "north") {
      epsg <- 26900 + zone  # NAD83 UTM North zones: EPSG 26901–26960
    } else {
      stop("NAD83 is not defined for southern hemisphere UTM zones.")
    }
  } else if (datum == "WGS84") {
    if (hemisphere == "north") {
      epsg <- 32600 + zone  # WGS84 UTM North
    } else {
      epsg <- 32700 + zone  # WGS84 UTM South
    }
  } else {
    stop("Unsupported datum. Use 'NAD83' or 'WGS84'.")
  }

  return(paste0("EPSG:", epsg))
}

debuggy <- function(var, start_time, end_time, note = ""){
  if(is.numeric(var) || is.character(var)){
    return(print(paste(note,":", var)))
  }else if(inherits(var, "SpatRaster") || inherits(var, "SpatVector")) {
    #terra::plot(var, main = paste(note, ": Time", start_time, "-", end_time))
    mm <- terra::minmax(var)
    message(paste0(note, ": Time ", start_time, " to ", end_time,
                   " | Min: ", mm[1], " | Max: ", mm[2]))
  }
}
