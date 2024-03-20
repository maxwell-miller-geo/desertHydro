# Initial Soil Conditions
# The functions in this script serve to create soil conditions for the land cover
# classifications. Some functions are meant to adjust the initial soil conditions
# for sensitivity analysis.

# Necessary Libraries
# library(readxl)
# library(terra)
# library(tidyverse)

# For readability, Land Cover Characteristics = LCC
hydraulicFieldConductivity <- function(Ksat, SaturatedMoistureContent, FieldCapacity, SoilDepth, raster = F){
  Kfc <- Ksat*exp((-13.0/SaturatedMoistureContent)*(SaturatedMoistureContent-FieldCapacity/SoilDepth))
  if(raster){
    return(Kfc)
  }
  return(ifelse(is.na(Kfc), 0, Kfc))
}

# Function to set the initial storage amount based upon table values
storage_amount <- function(landCoverTable){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  landCoverTable$maximumStorageAmount <- 
    (1-landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepthCM
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(landCoverTable$maximumStorageAmount)
}

# Create the land cover stacked map with soil characteristics
createSoilRasters <- function(ClassMap, soilTable, key = "MUKEY"){
  # Load in the class map
  ClassMap <- terra::rast(ClassMap)
  ClassMapNumeric <- terra::catalyze(ClassMap)
  landtypes <- unique(terra::values(ClassMap, na.rm = TRUE))
  outStack <- c() # creates empty vector
  for(x in 1:length(soilTable)){
    
    if(is.character(soilTable[[x]])){
      next # Breaks if the value in the table is a character (names)
    }
    c_matrix <- matrix(cbind(soilTable[[key]], soilTable[[x]]), ncol = 2) # Create classification matrix
    temp <- terra::classify(x = ClassMapNumeric, rcl = c_matrix) # classify ClassMap based on matrix
    #temp <- terra::subst(ClassMap, as.numeric(soilTable[[key]]), round(as.numeric(soilTable[[x]]),3)) # classify ClassMap based on matrix
    names(temp) <- names(soilTable[x]) # Assign a name to the Raster Layer
    outStack <- append(outStack, temp) # Append raster layer to raster 'brick'
  }
  return(outStack)
}
# # Test - create soil rasters
# LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_lc.tif)"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\landcover_soil.tif)"
# soilRaster <- createSoilRasters(ClassificationMap, LandCoverCharacteristics)


initial_soil_conditions <- function(LandCoverCharacteristics, ClassificationMap, DEM, ModelOutputs, key = "NLCD_Key", depthAdj = T, saturatedPercentage = 0.2, overwrite = F){
  
  soilstack_file <- file.path(ModelOutputs, "model_soil_stack.tif")
  if(file.exists(soilstack_file)){
    return("Found model soil file..")
  }
  LCC <- readxl::read_xlsx(LandCoverCharacteristics) # reads excel file with soil characteristics
  
  #day_to_min <- 1 * 24 * 60 # adjust day to minutes
  hour_to_sec <- 1 * 60 * 60 # adjust conductivity rates to cm/second

  # Adjust Hydraulic conductivity layers from day - minute, not very DRY
  LCC$saturatedHydraulicMatrix <- LCC$saturatedHydraulicMatrix / hour_to_sec
  LCC$verticalHydraulicConductivity <- LCC$verticalHydraulicConductivity / hour_to_sec
  LCC$hydraulicConductivityRestrictiveLayer <- LCC$hydraulicConductivityRestrictiveLayer / hour_to_sec
  
  LCC$fieldCapacityAmount <- LCC$soilDepthCM * (LCC$fieldCapacityMoistureContent - LCC$residualMoistureContent)
  
  # Calculate starting canopy storage amount
  LCC$currentCanopyStorage <- 0.0
  
  LCC$fieldCapacityAmount <- LCC$soilDepthCM * (LCC$fieldCapacityMoistureContent - LCC$residualMoistureContent)
  
  # Create conductivity at field capacity (Kfc)
  # Field Capacity Conductivity = Ksat * exp((-13.0/Sat_mc)*(sat_mc_1 -fieldcapt_amt/soilDepth))
  LCC$conductivityAtFieldCapacity <- hydraulicFieldConductivity(
    LCC$saturatedHydraulicMatrix,
    LCC$saturatedMoistureContent,
    LCC$fieldCapacityAmount,
    LCC$soilDepthCM)
  # Calculate maximum storage amount
  LCC$maxSoilStorageAmount <- storage_amount(LCC)
  # Calculate starting soil storage amount
  LCC$currentSoilStorage <- saturatedPercentage * LCC$maxSoilStorageAmount
  
  # Adjust the filed capacity amount if less than zero
  LCC$fieldCapacityAmount[LCC$fieldCapacityAmount < 0] <- 0
  
  LCC$wiltingPointAmount <- LCC$wiltingPointMoistureContent * LCC$soilDepthCM
  
  LCC$ET_Reduction <- LCC$fieldCapacityAmount * 0.8 / LCC$soilDepthCM
  
  SoilStack <- createSoilRasters(ClassificationMap, LCC, key = key)
  
  
  # Attach the slope map to the land cover stack
  dem <- terra::rast(DEM)
  slopeInitial <- terra::terrain(dem, v="slope", neighbors=8, unit="degrees")
  # Adjust zero slopes to 0.01 degrees to prevent pooling of water
  slopeAdj <- terra::ifel(slopeInitial < 0.01, 0.01, slopeInitial)
  slopeName <- file.path(ModelOutputs, "slope.tif")
  print(ext(slopeAdj))
  print(ext(SoilStack))
  SoilStack$slope <- slopeAdj
  names(SoilStack$slope) <- "slope"
  terra::writeRaster(SoilStack$slope, slopeName, overwrite = T)
  
  
  
  SoilStack$surfaceWater <- SoilStack$slope * 0
  names(SoilStack$surfaceWater) <- "surfaceWater"
  # From - to classification
  reclassTable <- c(0, 10, 1,
                    10, 20, .98,
                    20, 30, .95,
                    30, 40, .80,
                    40, 45, .50,
                    45, 90, .01)
  reclassMatrix <- matrix(reclassTable, ncol = 3, byrow = T)
  depthModifier <- terra::classify(SoilStack$slope, reclassMatrix, include.lowest = T)
  
  if(depthAdj){
    SoilStack$soilDepthCM <- SoilStack$soilDepthCM * depthModifier
    # Adjust certain characteristics base upon slope
    # Calculate Field capacity amount
    SoilStack$fieldCapacityAmount <- SoilStack$soilDepthCM * (SoilStack$fieldCapacityMoistureContent - SoilStack$residualMoistureContent)
    # Field Capacity Conductivity = Ksat * exp((-13.0/Sat_mc)*(sat_mc_1 -fieldcapt_amt/soilDepth))
    SoilStack$conductivityAtFieldCapacity <- hydraulicFieldConductivity(
      SoilStack$saturatedHydraulicMatrix,
      SoilStack$saturatedMoistureContent,
      SoilStack$fieldCapacityAmount,
      SoilStack$soilDepthCM, raster = T)
    # Calculate maximum storage amount
    SoilStack$maxSoilStorageAmount <- storage_amount(SoilStack)
    # Calculate starting soil storage amount
    SoilStack$currentSoilStorage <- saturatedPercentage * SoilStack$maxSoilStorageAmount
    
    # Adjust the filed capacity amount if less than zero
    SoilStack$fieldCapacityAmount[LCC$fieldCapacityAmount < 0] <- 0
    
    SoilStack$wiltingPointAmount <- SoilStack$wiltingPointMoistureContent * SoilStack$soilDepthCM
    
    SoilStack$ET_Reduction <- SoilStack$fieldCapacityAmount * 0.8 / SoilStack$soilDepthCM
  }
  # Stream extraction - adjust Manning's n in stream channel by 0.01
  streamPath <- file.path(WatershedElements, "stream_extracted.tif")
  if(file.exists(streamPath)){
    print("Stream adjustments")
    stream_extracted <- terra::rast(file.path(WatershedElements, "stream_extracted.tif"))
    if(terra::ext(stream_extracted) != terra::ext(SoilStack$slope)){
      stream_extracted <- terra::crop(stream_extracted, SoilStack$slope)
    }
    extracted <- terra::ifel(is.nan(stream_extracted), 0, stream_extracted)
    SoilStack$mannings_n <- SoilStack$mannings_n - extracted*.01
  }
  
  
  
  # Save the starting soil characteristic layers
  readr::write_csv(LCC, file.path(ModelOutputs, "Starting_Soil_Characteristics.csv"))
  # # startingSoil <- write.csv(LCC, file.path(DataStorage, "Starting_LandCover.csv"), overwrite = TRUE)
  
  terra::writeRaster(SoilStack, soilstack_file, overwrite = T)
  print("Model soil stack created...")
}

# Test - Initial soil conditions
# LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx"
# LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\landcover_soil.tif)"
# DEM <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\model_dem.tif)"
# ModelOutputs <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel)"
# #
# test <- initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
#                         ClassificationMap = ClassificationMap,
#                         DEM = DEM,
#                         ModelOutputs = ModelOutputs,
#                         key = "MUKEY")

# # Attach the flow direction stack to the land cover stack
# # Careful can cause extra stacks, but shouldn't matter because they will be duplicates
# SoilStack <- c(SoilStack, flowStack)
