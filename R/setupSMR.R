## -----------------------------------------------------------------------
#install.packages('readxl')
# library(readxl)
# library(methods)
# library(terra)
#
# LandCoverCharacteristics <- read_excel("LandCoverCharacteristics.xlsx")
#View(LandCoverCharacteristics)

read_LandCover <- function(localFilePath, type = "Excel"){
  if(type == "Excel"){
    LandCover <- read_excel(localFilePath)
  }

  # Create a dictionary based upon the excel spreadsheet
  landcover_dictionary <- apply(LandCover, 1, function(row) {
  as.list(row)})

  landcoverTypes <- lapply(landcover_dictionary, function(dict) {
  #name <- ifelse(is.null(dict$Name), NA, as.character(dict$Name))
  new('LandCover',
      # All classes must be present, no default currently set.
      Name = dict$Name,
      residualMoistureContent = as.numeric(dict$residualMoistureContent),
      porosity = as.numeric(dict$porosity),
      fieldCapacityMoistureContent = as.numeric(dict$fieldCapacityMoistureContent),
      rockPercent = as.numeric(dict$rockPercent),
      soilDepth = as.numeric(dict$soilDepth),
      wiltingPointMoistureContent = as.numeric(dict$wiltingPointMoistureContent),
      maxCanopyStorageAmount = as.numeric(dict$maxCanopyStorageAmount),
      saturatedMoistureContent = as.numeric(dict$saturatedMoistureContent),
      saturatedHydraulicMatrix = as.numeric(dict$saturatedHydraulicMatrix),
      saturatedHydraulicConductivityMacropore = as.numeric(dict$saturatedHydraulicConductivityMacropore),
      verticalHydraulicConductivity = as.numeric(dict$verticalHydraulicConductivity),
      channelMaxCanopyStorageAmount = as.numeric(dict$channelMaxCanopyStorageAmount)
      )
      })
  return(landcoverTypes)
}

# # Test case
# LocalFilePath <- "LandCoverCharacteristics.xlsx"
# landCover_example <- read_LandCover(LocalFilePath)

hydraulicFieldConductivity <- function(Ksat, SaturatedMoistureContent, FieldCapacity, SoilDepth){
  Kfc <- Ksat*exp((-13.0/SaturatedMoistureContent)*(SaturatedMoistureContent-FieldCapacity/SoilDepth))
  return(ifelse(is.na(Kfc), 0, Kfc))
}
# setClass('LandCover',
#          representation(
#            Name = 'character',
#            residualMoistureContent = 'numeric',
#            porosity = 'numeric',
#            fieldCapacityMoistureContent = 'numeric',
#            rockPercent = 'numeric',
#            soilDepth = 'numeric',
#            wiltingPointMoistureContent = 'numeric',
#            maxCanopyStorageAmount = 'numeric',
#            saturatedMoistureContent = 'numeric',
#            saturatedHydraulicMatrix = 'numeric',
#            saturatedHydraulicConductivityMacropore = 'numeric',
#            verticalHydraulicConductivity = 'numeric',
#            channelMaxCanopyStorageAmount = 'numeric'
#          ))


