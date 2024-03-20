# Main functions for creating maps or performing calculations on storage and flow
# Calculations done in the main function
#library(terra)
# Reclassify function
# Takes vector of categorical and returns a matrix used for reclassification based upon the table value.
# createReClassMatrix <- function(categories, characteristic){
#   return()
#   for(x in categories){
#     temp <- subset(table, table$NLCD_Key == x)
#     
#   }
# }


storage_amount <- function(landCoverTable){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  landCoverTable$storageAmount <- (1- landCoverTable$rockPercent) * landCoverTable$saturatedMoistureContent * landCoverTable$soilDepth
  # Create a map??
  #return(cbind(c(landCoverTable$NLCD_Key),c(landCoverTable$storageAmount)))
  return(landCoverTable$storageAmount)
}


# Calculates the hydraulic conductivity
hydraulicFieldConductivity <- function(KsatMatrix, SaturatedMC, FieldCapAmt, SoilDepth){
  Kfc <- KsatMatrix * exp((-13.0/SaturatedMC) * (SaturatedMC - FieldCapAmt/SoilDepth))
  return(ifelse(is.na(Kfc), 0, Kfc))
}

# Calculates the effective hydrualic conductivity for subsurface flow
# LaTEX version
# K _{mat} ^{\frac{-13}{ Sat_{mc}} * Sat_{mc} -  \frac{W_{surf}}{d_{soil}}}} for surface water below field capacity
# If surface water contains more water than the maximum soil moisture content * Soil Depth
# K_{macro}
# For values inbetween those use
# (K_{macro} -  K_{FieldCond}) *  \frac{(\frac{W_{surf}-Field_{cap}} { d_{soil}})}{ (\frac{Sat_{mc} - Field_{cap}}{d_{soil} + K_FieldCond})
effectiveConductivity <- function(surfaceWaterStorage, FieldCapacityAmount, KsatMatrix, SatMoistureContent, KsatMacropore, SoilDepth, KFieldConductivity){
  effK <- ifel(surfaceWaterStorage < FieldCapacityAmount, 
               (KsatMatrix * exp((-13.0/SatMoistureContent) * (SatMoistureContent - surfaceWaterStorage/SoilDepth))), 
               ifel(surfaceWaterStorage >= (SatMoistureContent*SoilDepth), 
                    KsatMacropore, 
                    (KsatMacropore - KFieldConductivity) * ((surfaceWaterStorage - FieldCapacityAmount) / SoilDepth) /  (SatMoistureContent - FieldCapacityAmount / SoilDepth) + KFieldConductivity))
  return(effK)
}


#storage_amount(LandCoverCharacteristics)
## storage function to be eventually deleted
# storage_amount_y <- function(table){
#   # Storage amount (y) calculated from total available pore space in soil
#   # Default storage = 20%
#   Available_poreSpace <- 0.20 # 20%
#   #table$storageAmountY <- Available_poreSpace * table$
# }

# Effective conductivty is calculated with Bresler's formula for unsaturated conductivity. Kfc is conductivity at field capacity, effective conductivity combines saturated and unsaturated
# Units of effective conductivity

# Lateral flow minimum between Darcy's law and amount of water on the surface
# LaTEX definition
# min( (K_{eff}*\frac{slope}{100}*\frac{d_{soil}}{10*100}) |  Soil_{storage})
lateral_flow <- function(effectiveK, slope, soilDepth, waterStorage){
  lateral_flow <- min(effectiveK*(slope/100) * soilDepth / (10*100), waterStorage) # check units
  return(lateral_flow)
}

## ------------------------
# Infiltration
# Infiltration via Darcy's law
infiltration <- function(SoilStack, simulationTimeSecs){
  # Depth of surface water after rain has accumulated
  tempSurfaceWater <- SoilStack$surfaceWater + SoilStack$throughfall
  
  # Infiltration - amount
  SoilStack$effectiveInfiltrationRate <- effectiveConductivity(tempSurfaceWater, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepthCM, SoilStack$conductivityAtFieldCapacity) * simulationTimeSecs
  
  # Ponding of water - 
  SoilStack$pondedWater <- terra::ifel((tempSurfaceWater > SoilStack$effectiveInfiltrationRate),
                                       tempSurfaceWater - SoilStack$effectiveInfiltrationRate, 0) # if
  
  # Calculate the soil storage after infiltration rate is calculated         
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage + SoilStack$effectiveInfiltrationRate
  
  # Calculate the excess moisture in soil as saturation excess
  throughflow <- terra::ifel(SoilStack$currentSoilStorage > SoilStack$maxSoilStorageAmount, 
                             SoilStack$currentSoilStorage - SoilStack$maxSoilStorageAmount, 0)
  # Adjust the soil storage for the throughflow that returned to surface
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - throughflow
  
  # Calculate the surface depth of water
  SoilStack$surfaceWater <- tempSurfaceWater + throughflow - SoilStack$effectiveInfiltrationRate# water stored on surface
  
  return(list(SoilStack$surfaceWater, SoilStack$currentSoilStorage))
}
  # Adjust the soil storage - keeping it under the maximum soil storage amount
  # SoilStack$throughfall
  # SoilStack$maxSoilStorageAmount
  # SoilStack$currentSoilStorage
  # SoilStack$currentSoilStorage <- terra::ifel((SoilStack$throughfall + SoilStack$currentSoilStorage) > SoilStack$maxSoilStorageAmount, SoilStack$maxSoilStorageAmount)

subsurfaceFlow <- function(SoilStack, simulationTimeSecs, flowStack_file){
  # Effective conductivity function for subsurface flow for unsaturated, saturated, and at fields capacity.
  # Rate of m/s
  SoilStack$effectiveConductivity <- effectiveConductivity(SoilStack$currentSoilStorage, SoilStack$fieldCapacityAmount, SoilStack$saturatedHydraulicMatrix, SoilStack$saturatedMoistureContent, SoilStack$saturatedHydraulicConductivityMacropore, SoilStack$soilDepthCM, SoilStack$conductivityAtFieldCapacity) * simulationTimeSecs
  
  # [3b] Lateral flow
  ## The following command uses Darcy's law to rout lateral flow through different land use
  # Lateral flow minimum between Darcy's law and amount of water on the surface
  lateralflow <- lateral_flow(SoilStack$effectiveConductivity, SoilStack$slope, SoilStack$soilDepthCM, SoilStack$storageAmount)
  # #sum(values(SoilStack$storageAmount), na.rm = TRUE)
  # # Adjust the storage amount in each cell from lateral flow
  #return(list(lateralflow, flowStack_file))
  SoilStack$currentSoilStorage <- SoilStack$currentSoilStorage - lateralflow + flowRouting(lateralflow, flowDirectionMap = flowStack_file)
  return(SoilStack$currentSoilStorage)
}


# Test
#storage_amount(LandCoverCharacteristics)

