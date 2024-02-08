#install.packages('readxl')
# library(readxl)
# #library(methods)
# library(terra)
# library(tidyverse)

# Create the land cover stacked map with soil characteristics
createSoilRasters <- function(ClassMap, soilTable){
  landtypes <- unique(terra::values(ClassMap, na.rm = TRUE))
  outStack <- c() # creates empty vector
  for(x in 1:length(soilTable)){

    if(is.character(soilTable[[x]])){
      next # Breaks if the value in the table is a character (names)
    }
    c_matrix <- matrix(cbind(as.numeric(soilTable$NLCD_Key), c(soilTable[[x]])), ncol = 2) # Create classification matrix
    temp <- terra::classify(ClassMap, c_matrix) # classify ClassMap based on matrix
    names(temp) <- names(soilTable[x]) # Assign a name to the Raster Layer
    outStack <- append(outStack, temp) # Append raster layer to raster 'brick'
  }
  return(outStack)
}

# Function to read in the land cover map - assumes NLCD - crops and resamples
# to computational watershed
resize_raster <- function(raster_to_resize, extent_raster, watershedboundary, save = FALSE, save_name = "", save_location = ""){
  land_cover_proj <- terra::project(raster_to_resize, extent_raster)
  # crop the landcover to the extend boundary
  if(!is.na(watershedboundary)){
    land_cover_crop <- terra::crop(land_cover_proj, terra::vect(watershedboundary), ext = FALSE, mask = TRUE)
  }else{
    land_cover_crop <- terra::crop(land_cover_proj, terra::rast(extent_raster), ext = TRUE)
  }

  if(save){
    # write the raster into the saved location
    outpath <- file.path(save_location, save_name)
    writeRaster(land_cover, outpath, overwrite = FALSE)
  }
  return(land_cover_crop)
  #plot(land_cover)
}

# Function to set the initial storage amount based upon table values
storage_amount <- function(landCoverTable){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  landCoverTable$maximumStorageAmount <- (1- landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepth
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(landCoverTable$maximumStorageAmount)
}

#View(LandCoverCharacteristics)
# Creates a list of the read in excel file - not sure I actually need it, but its here.
# read_LandCover_characteristics <- function(localFilePath, type = "Excel"){
#   if(type == "Excel"){
#     LandCover <- read_excel(localFilePath)
#   }
#
#   # Create a dictionary based upon the excel spreadsheet
#   landcover_dictionary <- apply(LandCover, 1, function(row) {
#     as.list(row)})
#
#   # Create Land Cover Class Type
#   setClass('LandCover',
#            representation(
#              NLCD_Key = "numeric",
#              Name = 'character',
#              residualMoistureContent = 'numeric',
#              porosity = 'numeric',
#              fieldCapacityMoistureContent = 'numeric',
#              rockPercent = 'numeric',
#              soilDepth = 'numeric',
#              wiltingPointMoistureContent = 'numeric',
#              maxCanopyStorageAmount = 'numeric',
#              saturatedMoistureContent = 'numeric',
#              saturatedHydraulicMatrix = 'numeric',
#              saturatedHydraulicConductivityMacropore = 'numeric',
#              verticalHydraulicConductivity = 'numeric',
#              channelMaxCanopyStorageAmount = 'numeric',
#              storageAmount = "numeric"
#            ))
#
#   landcoverTypes <- lapply(landcover_dictionary, function(dict) {
#     #name <- ifelse(is.null(dict$Name), NA, as.character(dict$Name))
#     new('LandCover',
#         # All classes must be present, no default currently set.
#         Name = dict$Name,
#         NLCD_Key = as.numeric(dict$NLCD_Key),
#         residualMoistureContent = as.numeric(dict$residualMoistureContent),
#         porosity = as.numeric(dict$porosity),
#         fieldCapacityMoistureContent = as.numeric(dict$fieldCapacityMoistureContent),
#         rockPercent = as.numeric(dict$rockPercent),
#         soilDepth = as.numeric(dict$soilDepth),
#         wiltingPointMoistureContent = as.numeric(dict$wiltingPointMoistureContent),
#         maxCanopyStorageAmount = as.numeric(dict$maxCanopyStorageAmount),
#         saturatedMoistureContent = as.numeric(dict$saturatedMoistureContent),
#         saturatedHydraulicMatrix = as.numeric(dict$saturatedHydraulicMatrix),
#         saturatedHydraulicConductivityMacropore = as.numeric(dict$saturatedHydraulicConductivityMacropore),
#         verticalHydraulicConductivity = as.numeric(dict$verticalHydraulicConductivity),
#         channelMaxCanopyStorageAmount = as.numeric(dict$channelMaxCanopyStorageAmount),
#         storageAmount = as.numeric(dict$storageAmount)
#     )
#   })
#   return(landcoverTypes)
# }
# Test case
# LocalFilePath <- "LandCoverCharacteristics.xlsx"
# landCover_example <- read_LandCover_characteristics(LocalFilePath)







# Test case
#resize_raster()


