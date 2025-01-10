# Function to read in the land cover map - assumes NLCD - crops and resamples
# to computational watershed
#' Resizes and reprojects the shape of a spatial object based on a pair
#'
#' @param spatialObject SpatVector or SpatRaster to be resized and/or reprojected
#' @param extent_raster Spatial extent raster to reproject into
#' @param boundary_path Optional shapefile path location to crop raster to
#' @param key Key to land cover type
#' @param save T/F If T, saves adjust land cover raster
#' @param save_name If save = T, file name to save adjusted land cover raster -
#' "~/adjusted_land_cover.tif"
#' @param save_location If save = T, location to save adjusted land cover raster
#'
#' @return Returns a SpatRaster with reprojected and cropped to input extents
#' dimenstions
#' @export
#'
#' @examples \dontrun{
#' spatialObject <- terra::rast()
#' extent_raster <- terra::rast()
#' boundary_path <- terra::vect()
#' key <- "MUSYM"
#' new_land_cover <- resizeShape(spatialObject, extent_raster, boundary_path, key = key)
#' }
resizeShape <- function(spatialObject, extent_raster, boundary_path = NA, key = "MUSYM", save = FALSE, save_name = "", save_location = ""){
  # Check class of spatial object
  if(class(spatialObject)[1] == "SpatVector"){
    land_cover_proj <- terra::project(spatialObject, extent_raster) # for categorical data only
  }else{
    land_cover_proj <- terra::project(spatialObject, extent_raster, method = "near") # for categorical data only
  }
  # crop the landcover to the extend boundary
  if(!is.na(boundary_path)){
    print("Clipping to watershed boundary.")
    # Procedure for spatial vector land cover files
    if(class(land_cover_proj)[1] == "SpatVector"){
      print("Cropping land cover vector.")
      land_cover_crop <- terra::crop(land_cover_proj, terra::vect(boundary_path), ext = FALSE)
      # Find KEY within names
      # if(key %in% names(land_cover_crop)){
      print("Rasterizing land cover.")
      browser()
      # Check key is inside land cover - try additional key
      if(!(key %in% names(land_cover_crop))){
        new_key <- "ID" # try another key
        if(!(new_key %in% names(land_cover_crop))){
          stop(cat("Could not find key match for land cover. Input:", key, "in available attributes of file:", terra::sources(spatialObject),"\n"))
        }else{
          key <- new_key
        }
      }
      land_cover_adj <- terra::rasterize(land_cover_crop, extent_raster, field = key, touches = T)
      return(land_cover_adj)
      # }
    }else{
      land_cover_adj <- terra::crop(land_cover_proj, terra::vect(boundary_path), ext = FALSE, mask = TRUE)
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
  landCoverTable$maximumStorageAmount <- (1-landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepth
  # Create a map??
  return(landCoverTable$maximumStorageAmount)
}

