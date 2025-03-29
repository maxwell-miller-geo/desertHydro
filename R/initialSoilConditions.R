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
storage_amount <- function(soil_depth_cm, saturated_moisture_content, rock_percent){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  maxStorageDepth <- (1-rock_percent) * saturated_moisture_content * soil_depth_cm
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(maxStorageDepth)
}

# Create the land cover stacked map with soil characteristics

createSoilRasters <- function(ClassMapFile, soilTable, key = "MUSYM"){
  #requireNamespace("terra")
  # Load in the class map
  if(is.character(ClassMapFile)){
    ClassMap <- terra::rast(ClassMapFile)
  }else{
    ClassMap <- ClassMapFile
  }
  # Categories <- terra::catalyze(ClassMap)
  # Categories <- terra::as.factor(ClassMap)
  # Simplest form where only manning's n is present - 2nd column is ID values
  if(key == "ID"){
    joinDF <- soilTable # Ignore first column; use second column as ID
    # getID <- terra::unique(ClassMap)[[1]]
    # joinDF <- dplyr::left_join(getID, soilTable, by = key)
  }
  else if(key == "GEOFNT24K"){
    getLevels <- terra::levels(ClassMap)[[1]]
    joinDF <- dplyr::left_join(getLevels, soilTable, by = key) # join by matching key
  } else{
    # Convert the levels of the categorical map to match the key
    selectColumn <- terra::levels(ClassMap)[[1]][key]
    selectColumn[,1] <- as.numeric(selectColumn[,1])
    # print(selectColumn)
    # getLevels <- data.frame(key = sapply(terra::levels(ClassMap)[[1]][key], as.double)) # not pretty
    #print(getLevels)
    joinDF <- dplyr::left_join(selectColumn, soilTable, by = key)
    ClassMap <- terra::catalyze(ClassMap)
  }
  outStack <- c() # creates empty vector
  for(x in 1:length(joinDF)){
    if(is.character(joinDF[[x]])){
      next # Breaks if the value in the table is a character (names)
    }
    #c_matrix <- matrix(cbind(soilTable[[key]], as.numeric(soilTable[[x]])), ncol = 2) # Create classification matrix
    c_matrix <- matrix(cbind(joinDF[[1]], as.numeric(joinDF[[x]])), ncol = 2) # Create classification matrix - assumes key is in first column header...
    if(key != "GEOFNT24K"){
      if(x == 1){
        next
      }
      temp <- terra::subst(ClassMap, as.numeric(soilTable[[key]]), round(as.numeric(soilTable[[x]]),3))# classify ClassMap based on matrix
      if(key == "ID"){
        # Classification matrix
        c_matrix <- matrix(cbind(soilTable[[key]], as.numeric(joinDF[[x]])), ncol = 2)
        # Assign values
        temp <- terra::subst(ClassMap, as.integer(soilTable[[key]]), round(as.numeric(soilTable[[x]]),3))
      }
    }else{
      temp <- terra::classify(x = ClassMap, rcl = c_matrix) # classify ClassMap based on matrix
    }
    #temp <- terra::subst(ClassMap, as.numeric(soilTable[[key]]), round(as.numeric(soilTable[[x]]),3)) # classify ClassMap based on matrix
    names(temp) <- names(joinDF[x]) # Assign a name to the Raster Layer
    outStack <- append(outStack, temp) # Append raster layer to raster 'brick'
  }
  return(outStack)
}
# # Test - create soil rasters
# LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_lc.tif)"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\landcover_soil.tif)"
# soilRaster <- createSoilRasters(ClassificationMap, LandCoverCharacteristics)


initial_soil_conditions <- function(LandCoverCharacteristics,
                                    ClassificationMap,
                                    WatershedStack,
                                    ModelFolder,
                                    WatershedElements = "",
                                    key = "NLCD_Key",
                                    outline = "",
                                    depth_adjusted = "slope",
                                    surface_method = "soils+stream",
                                    infiltration_method = "soils+geo",
                                    initial_soil_conditions = "normal",
                                    surface_adj = 1,
                                    infiltration_adj = 1,
                                    overwrite = T,
                                    ...){
  # Things that can be adjusted
  #----------------------------
  # Soil Depth based on slope
  # Surface based upon stream
  # Soil conditions: normal | dry (initial_soil_conditions)
  # Infiltration methods: soils | geo
  # Surface methods: stream | geo | soils (default)

  soilstack_file <- file.path(ModelFolder, "model_soil_stack.tif")
  if(file.exists(soilstack_file) & overwrite == F){
    print("Found model soil file..")
    return(0)
  }

  #LandCoverCharacteristics <- "C:/Thesis/desertHydro/inst/extdata/LandCoverCharacteristics_Soils.xlsx"
  LCC <- as.data.frame(readxl::read_xlsx(LandCoverCharacteristics)) # reads excel file with soil characteristics

  # Determine infiltration characteristics
  LCC$wilting_percent <- LCC[, grep("wilting_", names(LCC))] / 100
  # Extract field capacity in decimal form
  LCC$field_capacity_percent <- LCC[,grep("field_", names(LCC))]/100 # initial saturation often
  # Extract saturation percentage
  LCC$saturation_percent <- LCC[,grep("saturation_per", names(LCC))]/ 100 # maximum saturation
  # Built on data from SPAW - USDA Infiltration calculator based on soil texture
  LCC$Ksat_cm_hr <- LCC[,grep("Ksat_in_hr", names(LCC))]* 2.54
  LCC$soil_depth_cm <- LCC[,grep("soil_depth", names(LCC))]* 2.54
  # Set initial conditions of soil saturation
  if(initial_soil_conditions == "normal"){
    LCC$initial_sat_content <- LCC$field_capacity_percent
  }
  if(initial_soil_conditions == "dry"){
    LCC$initial_sat_content <- LCC$wiltingPointMoistureContent
  }

  # if(grepl("nlcd", infiltration_method)){
  # POTENTIAL nlcd iniltration style infiltration
  # }
  if(key != "ID" && grepl("soils", infiltration_method)){ # ID is the key for nlcd only maps - runoff only no soil char.{
  # Get the Max Soil Depth,
  #day_to_min <- 1 * 24 * 60 # adjust day to minutes
  hour_to_sec <- 1 * 60 * 60 # adjust conductivity rates to cm/second
  # # Calculate maximum storage amount
  LCC$maxSoilStorageAmount <- storage_amount(soil_depth_cm = LCC$soil_depth_cm,
                                        saturated_moisture_content = LCC$saturation_percent,
                                        rock_percent = LCC$rockPercent)

  # # Calculate starting soil storage amount - variable
  saturatedPercentage <- LCC$initial_sat_content
  # Current soil storage
  LCC$currentSoilStorage <- saturatedPercentage * LCC$maxSoilStorageAmount
  }
  # Convert Soil table into Stacked raster
  SoilStack <- createSoilRasters(ClassMapFile = ClassificationMap, soilTable = LCC, key = key)
  # Select necesary Soil Stack Layers
  # Don't combine flow stacks
  SoilStack <- c(SoilStack, WatershedStack) # combine watershed and flow stacks

  # Adjust slope based on elevation differences
  SoilStack$slope <- terra::ifel(SoilStack$slope < 0.01, 0.01, SoilStack$slope)
  slopeName <- file.path(ModelFolder, "model_slope.tif")
  terra::writeRaster(SoilStack$slope, slopeName, overwrite = T)

  # Create surface water layer
  SoilStack$surfaceWater <- SoilStack$slope * 0
  names(SoilStack$surfaceWater) <- "surfaceWater"
  # Create velocity layer
  SoilStack$velocity <- SoilStack$slope * 0
  names(SoilStack$velocity) <- "velocity"
  # Create throughfall layer
  SoilStack$throughfall <- SoilStack$slope * 0
  names(SoilStack$throughfall) <- "throughfall"
  # Create total infiltration layer
  SoilStack$infiltrated_water_cm <- SoilStack$slope * 0
  names(SoilStack$infiltrated_water_cm) <- "infiltrated_water_cm"

  # Adjust certain characteristics base upon slope
  if(grepl("slope", depth_adjusted) && key != "ID"){
    # From - to classification of soil depth from slope
    reclassTable <- c(0, 10, 1,
                      10, 20, .98,
                      20, 30, .95,
                      30, 40, .80,
                      40, 45, .50,
                      45, 90, .01)

    reclassMatrix <- matrix(reclassTable, ncol = 3, byrow = T)
    depthModifier <- terra::classify(SoilStack$slope, reclassMatrix, include.lowest = T)
    SoilStack$soil_depth_cm <- SoilStack$soil_depth_cm * depthModifier
  }

  # Geo map adjustment
  geologic_map <- file.path(WatershedElements, "geo_soils.shp")
  # Adjust soil characteristics based upon geology
  if(file.exists(geologic_map) && (grepl("geo", infiltration_method) || grepl("geo", surface_method))){
    print("Found geologic map: Adjusting hydraulic parameters")
    adjustmentMaps <- geologyProcess(geologic_map, SoilStack, WatershedElements, ModelFolder)
    # Adjust the manning's N coefficients
    if(grepl("geo", surface_method)){
      SoilStack$mannings_n <- SoilStack$mannings_n * adjustmentMaps$mannings_n
    }
    if(grepl("geo", infiltration_method)){
      # Adjust the soil depths (cm)
      SoilStack$soil_depth_cm <- SoilStack$soil_depth_cm * adjustmentMaps$SoilDepthAdj
      # Adjust the max storage amount
      SoilStack$maxSoilStorageAmount <- storage_amount(SoilStack$soil_depth_cm,
                                                       SoilStack$saturation_percent,
                                                       SoilStack$rockPercent)
      # Adjust the hydraulic conductivity
      SoilStack$Ksat_cm_hr <- SoilStack$Ksat_cm_hr *adjustmentMaps$saturatedHydraulicMatrix
    }
  }

  # Calculate Field capacity amount
  SoilStack$fieldCapacityAmount <- SoilStack$soil_depth_cm *
    (SoilStack$fieldCapacityMoistureContent -   SoilStack$residualMoistureContent)
  # Field Capacity Conductivity = Ksat * exp((-13.0/Sat_mc)*(sat_mc_1 -fieldcapt_amt/soilDepth))
  # NOT CURRENTLY USED
  SoilStack$conductivityAtFieldCapacity <- hydraulicFieldConductivity(
    SoilStack$saturatedHydraulicMatrix,
    SoilStack$saturation_percent,
    SoilStack$fieldCapacityAmount,
    SoilStack$soil_depth_cm, raster = T)

  # Calculate starting soil storage amount
  SoilStack$currentSoilStorage <- SoilStack$initial_sat_content * SoilStack$maxSoilStorageAmount

  # Adjust the filed capacity amount if less than zero
  SoilStack$fieldCapacityAmount[LCC$fieldCapacityAmount < 0] <- 0

  SoilStack$wiltingPointAmount <- SoilStack$wiltingPointMoistureContent * SoilStack$soil_depth_cm

  SoilStack$ET_Reduction <- SoilStack$fieldCapacityAmount * 0.8 / SoilStack$soil_depth_cm

  # Stream extraction - adjust Manning's n in stream channel by 0.01
  streamPath <- file.path(ModelFolder, "stream_extracted.tif")
  # Increase the speed of water within stream network
  if(file.exists(streamPath) && grepl("stream", surface_method)){
    print("Stream adjustments")
    stream_extracted <- terra::rast(streamPath)
    if(terra::ext(stream_extracted) != terra::ext(SoilStack$slope)){
      stream_extracted <- terra::crop(stream_extracted, SoilStack$slope)
    }
    extracted <- terra::ifel(is.nan(stream_extracted), 0, stream_extracted)
    new_stream <- abs(SoilStack$mannings_n - .0025*extracted)
    SoilStack$mannings_n <- new_stream
  }

  # Final adjustments based upon surface and infiltration
  # Adjust surface roughness
  SoilStack$mannings_n <- SoilStack$mannings_n * surface_adj
  # Adjust infiltration rate
  SoilStack$Ksat_cm_hr <- SoilStack$Ksat_cm_hr * infiltration_adj

  # Adjust the maximum saturation amount (cm)
  SoilStack$maxSoilStorageAmount <- storage_amount(SoilStack$soil_depth_cm,
                                                   SoilStack$saturation_percent,
                                                   SoilStack$rockPercent)
  # Adjust the current soil storage
  SoilStack$currentSoilStorage <- SoilStack$initial_sat_content * SoilStack$maxSoilStorageAmount

  # Save the starting soil characteristic layers
  readr::write_csv(LCC, file.path(ModelFolder, "Starting_Soil_Characteristics.csv"))
  # # startingSoil <- write.csv(LCC, file.path(DataStorage, "Starting_LandCover.csv"), overwrite = TRUE)
  terra::writeRaster(SoilStack, soilstack_file, overwrite = overwrite)
  print("Model soil stack created...")
  return(SoilStack)
}

# Test - Initial soil conditions
# LandCoverCharacteristics <- "LandCoverCharacteristics.xlsx"
# LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
# ClassificationMap <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\landcover_soil.tif)"
# DEM <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\model_dem.tif)"
# ModelFolder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel)"
# #
# test <- initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
#                         ClassificationMap = ClassificationMap,
#                         DEM = DEM,
#                         ModelFolder = ModelFolder,
#                         key = "MUKEY")

# # Attach the flow direction stack to the land cover stack
# # Careful can cause extra stacks, but shouldn't matter because they will be duplicates
# SoilStack <- c(SoilStack, flowStack)
estimate_suction_head <- function(soil_texture) {
  suction_values <- list(
    "Sand" = 2.41,
    "Loamy Sand" = 3.50,
    "Sandy Loam" = 5.86,
    "Loam" = 9.83,
    "Silt Loam" = 13.19,
    "Clay Loam" = 14.17,
    "Silty Clay" = 15.75,
    "Clay" = 18.82
  )

  return(suction_values[[soil_texture]])
}
# Function to estimate residual moisture content (θ_r) based on soil texture
estimate_residual_moisture <- function(soil_texture) {

  # Residual moisture content values (θ_r) from literature (cm³/cm³)
  residual_moisture_table <- list(
    "Sand" = 0.03,
    "Loamy Sand" = 0.05,
    "Sandy Loam" = 0.07,
    "Loam" = 0.10,
    "Silt Loam" = 0.12,
    "Clay Loam" = 0.15,
    "Silty Clay" = 0.18,
    "Clay" = 0.20
  )

  # Convert input to match dictionary keys
  soil_texture <- tolower(soil_texture)
  names(residual_moisture_table) <- tolower(names(residual_moisture_table))

  # Check if soil type exists in the table
  if (soil_texture %in% names(residual_moisture_table)) {
    return(residual_moisture_table[[soil_texture]])
  } else {
    stop("Soil texture not recognized. Please enter a valid soil type.")
  }
}
# Function to retrieve soil hydraulic properties based on soil texture
get_soil_hydraulic_properties <- function(soil_texture) {

  # Create a lookup table as a data frame
  soil_data <- data.frame(
    Soil_Texture = c("Sand", "Loamy Sand", "Sandy Loam", "Loam", "Silt Loam",
                     "Sandy Clay Loam", "Clay Loam", "Silty Clay Loam", "Sandy Clay",
                     "Silty Clay", "Clay"),
    Ks_cm_h = c(21.0, 6.11, 2.59, 1.32, 0.68, 0.43, 0.23, 0.15, 0.12, 0.09, 0.06),
    Porosity_percent = c(43.7, 43.7, 45.3, 46.3, 50.1, 39.8, 46.4, 47.1, 43.0, 47.9, 47.5),
    Theta_r_percent = c(2.0, 3.5, 4.1, 2.7, 1.5, 6.8, 7.5, 10.2, 10.9, 5.6, 9.0),
    Psi_b_cm = c(-7.26, -8.69, -14.66, -11.15, -20.76, -28.08, -25.89, -32.56, -29.17, -34.19, -37.30),
    b_value = c(1.44, 1.81, 2.65, 3.97, 4.27, 3.13, 4.13, 5.45, 4.48, 6.67, 6.06),
    stringsAsFactors = FALSE
  )

  # Convert input to title case for consistency
  soil_texture <- tolower(soil_texture)
  soil_data$Soil_Texture <- tolower(soil_data$Soil_Texture)

  # Retrieve row matching soil texture
  result <- soil_data[soil_data$Soil_Texture == soil_texture, ]

  # Check if a valid soil type was provided
  if (nrow(result) == 0) {
    stop("Soil texture not recognized. Please enter a valid soil type.")
  }

  return(result)
}
# Function to calculate the infiltration rate I(t) based on the Green and Ampt model
green_ampt_infil <- function(Ksat_cm_hr, theta_s, theta_i, F_0) {
  # Calculate the change in volumetric water content (Delta theta)
  # theta_s = saturated water content %
  # theta_i = initial saturated water content, either wilting point or field capacity
  delta_theta <- theta_s - theta_i

  # Example dataframe for wetting front suction head values (psi) in cm
  wetting_front_suction <- data.frame(
    soil_texture = c("Sand", "Loamy Sand", "Sandy Loam", "Loam", "Silt Loam",
                     "Sandy Clay Loam", "Clay Loam", "Silty Clay Loam", "Sandy Clay",
                     "Silty Clay", "Clay"),
    psi_cm = c(4.95, 6.13, 11.01, 8.89, 16.68, 21.85, 20.88, 27.3, 23.9, 29.22, 31.63)
  )

  # Calculate the infiltration rate I(t)
  I_t <- Ksat_cm_hr / delta_theta * (1 + F_0 / delta_theta)^(-1) # cm/hr

  return(I_t)
}
