# Stream analysis processsing
## ------------------------ Function to take a collection of vector
# streams lines and burn in the elevation profiles into the model
streamBurn <- function(stream, dem, flow_accumulation){
  WatershedElements <- r"(C:\PackageDev\desertHydro\inst\extdata)"
  stream <- file.path(WatershedElements, "stream_analysis.shp")
  dem <- file.path(WatershedElements, "dem.tif")
  mod_dem <- file.path(WatershedElements, "mod_dem.tif")

  # Perform flow accumulation
  whitebox::wbt_breach_depressions_least_cost(dem, mod_dem, dist = 100, flat_increment = .01)
  crsAssign(mod_dem)

  # Load in stream vector
  if(is.character(stream)){
    stream <- terra::vect(stream)
  }
  dem <- mod_dem
  if(is.character(dem)){
    dem <- terra::rast(dem)
  }
  #streamDF <- as.data.frame(stream)
  # Extract DEM values
  extractedDEM <- terra::extract(dem, stream, cells = T)
  extractedDF <- data.table::data.table(extractedDEM)
  # Count unique ID's
  nUnique <- length(unique(extracedDF$ID))
  # fill <- zoo::na.approx(extractedDF[ID == x, mod_dem])
  # emptyDF[ID==x, mod_dem := fill]
  emptyDF <- extractedDF
  # Loop through the segments
  for(x in 1:nUnique){
    print(x)
    if(NA %in% emptyDF[ID == x, mod_dem]){
      fill <- zoo::na.approx(emptyDF[ID == x, mod_dem])
      emptyDF[ID==x, mod_dem := smoothVector(fill)]
    }
    emptyDF[ID==x, mod_dem := smoothVector(emptyDF[ID == x, mod_dem])]
  }

  # Put DF back into original
  for(i in 1:nrow(emptyDF)){
    dem[emptyDF[[i,3]]] <- emptyDF[i,2]
  }

  smoothDEM <- dem
  terra::writeRaster(smoothDEM, file.path(WatershedElements, "smooth_dem.tif"))
  # selectSegment <- sapply(x = 1:nUnique, FUN = extractedDF[ID == x, mod_dem])
  # selectIdOne <- extractedDF[ID == 1, mod_dem] # ID = 1, dem column
  # selectId2 <- extractedDF[ID == 2, mod_dem]# ID = 1, dem column

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
