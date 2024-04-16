# Stream analysis processsing
## ------------------------
#
#' Function to take a collection of vector
#' streams lines and burn in the elevation profiles into the model
#'
#' @param stream SpatVector or file path containing the stream lines
#' @param demPath File path to SpatRaster
#' @param outpath Optional: Output path. Will be saved as "smooth_dem.tif"
#'
#' @return SpatRaster with stream network burned in.
#' @export
#'
#' @examples \dontrun{
#' smooth_dem <- smoothStream(streamVector, demPath)
#' }
#'
smoothStream <- function(stream, demPath, outpath = NULL){
  # WatershedElements <- r"(C:\PackageDev\desertHydro\inst\extdata)"
  # stream <- file.path(WatershedElements, "stream_analysis.shp")
  #dem <- file.path(WatershedElements, "dem.tif")
  # Note dem must be a path
  #ID <- NULL
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

  if(is.character(mod_dem)){
    dem <- terra::rast(mod_dem)
  }
  # print(mod_dem)
  # print(dem)
  # Extract DEM values
  extractedDEM <- terra::extract(dem, stream, cells = T, xy = T)
  extractedDF <- data.table::data.table(extractedDEM)
  # Change the names
  data.table::setnames(extractedDF, "mod_dem", "elev")
  # return(extractedDF)
  # Count unique ID's
  #nUnique <- length(unique(extractedDF$ID))
  #data.table::is.data.table(extractedDF)
  #return(extractedDF)
  # fill <- zoo::na.approx(extractedDF[ID == x, mod_dem])
  # emptyDF[ID==x, mod_dem := fill]
  #"ID" %in% extractedDF
  #emptyDF <- extractedDF
  #return(emptyDF)
  # Loop through the segments
    # x <- as.numeric(x)
    # if(NA %in% extractedDF[extractedDF$ID == x, elev]){
    #   fill <- zoo::na.approx(extractedDF[ID == x, elev])
    #   extractedDF[, elev := smoothVector(fill), by = ID]
    # }
  #return(extractedDF)
  #extractedDF[, elev, by = ID]
  extractedDF[, elev := smoothVector(elev), by = ID]

  # Get coordinate system
  coords <- paste0("epsg:",terra::crs(dem, describe = T)[[3]])
  burn <- terra::vect(extractedDF, geom = c("x", "y"), crs= coords)
  # # Put DF back into original
  # for(i in 1:nrow(extractedDF)){
  #   dem[extractedDF[[i,3]]] <- extractedDF[i,2]
  # }

  smoothDEM <- terra::rasterize(x = burn, y = dem, field = "elev")
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
#' Adjust stream channel elevations to remove pits
#'
#' @param x vector of elevation values
#' @param n maximum number of iterations to fix elevations
#' @param h_adj additional increment to the smoothed locations Default
#' 0.01.
#' @return Smoothed vector of elevations
#' @export
#'
#' @examples
#' example_stream <- c(1,2,3,5,3,2,6)
#' smooth <- smoothVector(example_stream)
#'
#' # If NA's are present
#' s2 <- c(1,2,4,5,NA,6)
#' smooth <- smoothVector(s2)
smoothVector <- function(x, n = 1000, h_adj = .01){
  # Find NA values if present
  if(NA %in% x){
    x <- zoo::na.approx(x)
  }
  i <- 1
  xDiff <- diff(x)
  while(sum(xDiff < 0) > 0){
    diffAdd <- c(0, ifelse(xDiff <= 0, xDiff-h_adj, 0))
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


