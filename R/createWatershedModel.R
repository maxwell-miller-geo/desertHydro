# library(readxl)
# #library(methods)
# library(terra)
# library(tidyverse)

# Create the land cover stacked map with soil characteristics
# createSoilRasters <- function(ClassMap, soilTable){
#   landtypes <- unique(terra::values(ClassMap, na.rm = TRUE))
#   outStack <- c() # creates empty vector
#   for(x in 1:length(soilTable)){
#
#     if(is.character(soilTable[[x]])){
#       next # Breaks if the value in the table is a character (names)
#     }
#     c_matrix <- matrix(cbind(as.numeric(soilTable$NLCD_Key), c(soilTable[[x]])), ncol = 2) # Create classification matrix
#     temp <- terra::classify(ClassMap, c_matrix) # classify ClassMap based on matrix
#     names(temp) <- names(soilTable[x]) # Assign a name to the Raster Layer
#     outStack <- append(outStack, temp) # Append raster layer to raster 'brick'
#   }
#   return(outStack)
# }

# Function to read in the land cover map - assumes NLCD - crops and resamples
# to computational watershed
resizeShape <- function(spatialObject, extent_raster, watershedboundary, key = "MUSYM", save = FALSE, save_name = "", save_location = ""){
  if(class(spatialObject)[1] == "SpatVector"){
    land_cover_proj <- terra::project(spatialObject, extent_raster) # for categorical data only
  }else{
    land_cover_proj <- terra::project(spatialObject, extent_raster, method = "near") # for categorical data only
  }
  # crop the landcover to the extend boundary
  if(!is.na(watershedboundary)){
    print("Clipping to watershed boundary.")
    if(class(land_cover_proj)[1] == "SpatVector"){
      print("Cropping land cover vector.")
      land_cover_crop <- terra::crop(land_cover_proj, terra::vect(watershedboundary), ext = FALSE)
      # Find KEY within names
      # if(key %in% names(land_cover_crop)){
      print("Rasterizing land cover.")
      land_cover_adj <- terra::rasterize(land_cover_crop, extent_raster, field = key, touches = T)
      return(land_cover_adj)
      # }
    }else{
      land_cover_adj <- terra::crop(land_cover_proj, terra::vect(watershedboundary), ext = FALSE, mask = TRUE)
    }

  }else{
    land_cover_adj <- terra::crop(land_cover_proj, terra::rast(extent_raster), ext = TRUE)
  }

  if(save){
    # write the raster into the saved location
    outpath <- file.path(save_location, save_name)
    terra::writeRaster(land_cover_adj, outpath, overwrite = TRUE)
  }
  return(land_cover_adj)
  #plot(land_cover)
}
# Check
# Function to set the initial storage amount based upon table values
storage_amount <- function(landCoverTable){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  landCoverTable$maximumStorageAmount <- (1- landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepth
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(landCoverTable$maximumStorageAmount)
}

