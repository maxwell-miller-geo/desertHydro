# Stream analysis processsing
## ------------------------ Function to take a collection of vector
# streams lines and burn in the elevation profiles into the model
streamBurn <- function(stream, demPath, outpath = NULL){
  # WatershedElements <- r"(C:\PackageDev\desertHydro\inst\extdata)"
  # stream <- file.path(WatershedElements, "stream_analysis.shp")
  #dem <- file.path(WatershedElements, "dem.tif")
  # Note dem must be a path
  require(whitebox)
  require(data.table)
  require(terra)
  WatershedElements <- outpath
  if(class(demPath) == "SpatRaster"){
    stop("Please input dem as a path, not a spatial raster")
  }
  mod_dem <- file.path(tempdir(), "mod_dem.tif")
  if(file.exists(mod_dem)){
    file.remove(mod_dem) # remove file if present
  }

  # Perform flow accumulation
  whitebox::wbt_breach_depressions_least_cost(demPath, mod_dem, dist = 100, flat_increment = .01)
  crsAssign(mod_dem)

  # Load in stream vector
  if(is.character(stream)){
    stream <- terra::vect(stream)
  }
  mod_dem
  if(is.character(mod_dem)){
    dem <- terra::rast(mod_dem)
  }
  print(mod_dem)
  print(dem)
  # Extract DEM values
  extractedDEM <- terra::extract(dem, stream, cells = T, xy = T)
  extractedDF <- data.table::data.table(extractedDEM)
  # return(extractedDF)
  # Count unique ID's
  nUnique <- length(unique(extractedDF$ID))
  # fill <- zoo::na.approx(extractedDF[ID == x, mod_dem])
  # emptyDF[ID==x, mod_dem := fill]
  #"ID" %in% extractedDF
  #emptyDF <- extractedDF
  #return(emptyDF)
  # Loop through the segments
  for(x in 1:nUnique){
    if(NA %in% extractedDF[ID == x, mod_dem]){
      fill <- zoo::na.approx(extractedDF[ID == x, mod_dem])
      extractedDF[ID==x, mod_dem := smoothVector(fill)]
    }
    extractedDF[ID==x, mod_dem := smoothVector(extractedDF[ID == x, mod_dem])]
  }

  points <-
  # # Put DF back into original
  # for(i in 1:nrow(extractedDF)){
  #   dem[extractedDF[[i,3]]] <- extractedDF[i,2]
  # }

  smoothDEM <- dem
  if(!is.null(WatershedElements)){
    terra::writeRaster(smoothDEM, file.path(WatershedElements, "smooth_dem.tif"))
  }

  # selectSegment <- sapply(x = 1:nUnique, FUN = extractedDF[ID == x, mod_dem])
  # selectIdOne <- extractedDF[ID == 1, mod_dem] # ID = 1, dem column
  # selectId2 <- extractedDF[ID == 2, mod_dem]# ID = 1, dem column
  return(smoothDEM)
}

# Function for difference recursion
# Takes vector of values (dem) as inputs
# does not change the output of the first and last points
smoothVector <- function(x, n = 100){
  # Take initial difference
  i <- 1
  xDiff <- diff(x)
  while(sum(xDiff < 0) > 0){
    diffAdd <- c(0, ifelse(xDiff <= 0, xDiff-0.01, 0))
    x <- x - diffAdd
    i <- i + 1
    xDiff <- diff(x)
    if(i == n){
      break
    }
  }
  #print(paste0("Number of iterations to smooth: ", i))
  return(x)
}
