# Initial Soil Conditions
# The functions in this script serve to create soil conditions for the land cover
# classifications. Some functions are meant to adjust the initial soil conditions
# for sensitivity analysis.

# Necessary Libraries
# library(readxl)
# library(terra)
# library(tidyverse)

# For readability, Land Cover Characteristics = LCC
hydraulicFieldConductivity <- function(Ksat, SaturatedMoistureContent, FieldCapacity, SoilDepth){
  Kfc <- Ksat*exp((-13.0/SaturatedMoistureContent)*(SaturatedMoistureContent-FieldCapacity/SoilDepth))
  return(ifelse(is.na(Kfc), 0, Kfc))
}

# Function to set the initial storage amount based upon table values
storage_amount <- function(landCoverTable){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  landCoverTable$maximumStorageAmount <- (1- landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepth
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(landCoverTable$maximumStorageAmount)
}

# Create the land cover stacked map with soil characteristics
createSoilRasters <- function(ClassMap, soilTable){
  # Load in the class map
  ClassMap <- terra::rast(ClassMap)
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
# # Test - create soil rasters
# LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_lc.tif)"
# soilRaster <- createSoilRasters(ClassificationMap, LandCoverCharacteristics)

initial_soil_conditions <- function(LandCoverCharacteristics, ClassificationMap, DEM, ModelOutputs,saturatedPercentage = 0.2){
  LCC <- readxl::read_xlsx(LandCoverCharacteristics) # reads excel file with soil characteristics

  day_to_min <- 1 * 24 * 60 # adjust day to minutes

  # Adjust Hydraulic conductivity layers from day - minute, not very DRY
  LCC$saturatedHydraulicMatrix <- LCC$saturatedHydraulicMatrix / day_to_min
  LCC$verticalHydraulicConductivity <- LCC$verticalHydraulicConductivity / day_to_min
  LCC$hydraulicConductivityRestrictiveLayer <- LCC$hydraulicConductivityRestrictiveLayer / day_to_min

  # Create conductivity at field capacity (Kfc)
  # Field Capacity Conductivity = Ksat * exp((-13.0/Sat_mc)*(sat_mc_1 -fieldcapt_amt/soilDepth))
  LCC$conductivityAtFieldCapacity <- hydraulicFieldConductivity(
    LCC$saturatedHydraulicMatrix,
    LCC$saturatedMoistureContent,
    LCC$fieldCapacityMoistureContent,
    LCC$soilDepth)
  # Calculate maximum storage amount
  LCC$maxSoilStorageAmount <- storage_amount(LCC)

  # Calculate starting soil storage amount
  LCC$currentSoilStorage <- saturatedPercentage * LCC$maxSoilStorageAmount

  # Calculate starting canopy storage amount
  LCC$currentCanopyStorage <- 0.0

  # Calculate Field capacity amount
  LCC$fieldCapacityAmount <- LCC$soilDepth * (LCC$fieldCapacityMoistureContent - LCC$residualMoistureContent)

  # Adjust the filed capacity amount if less than zero
  LCC$fieldCapacityAmount[LCC$fieldCapacityAmount < 0] <- 0

  LCC$wiltingPointAmount <- LCC$wiltingPointMoistureContent * LCC$soilDepth

  LCC$ET_Reduction <- LCC$fieldCapacityAmount * 0.8 / LCC$soilDepth

  SoilStack <- createSoilRasters(ClassificationMap, LCC)

  # Attach the slope map to the land cover stack
  dem <- terra::rast(DEM)
  SoilStack$slope <- terra::terrain(dem, v="slope", neighbors=8, unit="degrees",
                                    filename = file.path(ModelOutputs, "slope.tif"),
                                    overwrite = T)
  names(SoilStack$slope) <- "slope"

  # # Create variable to store subsurface soil storage through time
  subsurfaceStorage <- SoilStack$currentSoilStorage
  ## Create variable to store surface storage in through time
  surfaceStorage <- SoilStack$currentSoilStorage * 0
  # Create variable to store velocities through time
  velocityStorage <- SoilStack$currentSoilStorage * 0

  # Save the starting soil characteristic layers
  write_csv(LCC, file.path(ModelOutputs, "Starting_Soil_Characteristics.csv"))
  # # startingSoil <- write.csv(LCC, file.path(DataStorage, "Starting_LandCover.csv"), overwrite = TRUE)
  soilstack_file <- file.path(ModelOutputs, "model_soil_stack.tif")

  writeRaster(SoilStack, soilstack_file, overwrite = T)
}

# Test - Initial soil conditions
# LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_lc.tif)"
# DEM <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\clipped_dem.tif)"
# ModelOutputs <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel)"
# #
# initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
#                         ClassificationMap = ClassificationMap,
#                         DEM = DEM,
#                         ModelOutputs = ModelOutputs)

# # Attach the flow direction stack to the land cover stack
# # Careful can cause extra stacks, but shouldn't matter because they will be duplicates
# SoilStack <- c(SoilStack, flowStack)
