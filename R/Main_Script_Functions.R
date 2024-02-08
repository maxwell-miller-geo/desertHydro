# Main functions for creating maps or performing calculations on storage and flow
# Calculations done in the main function
# library(terra)
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
                    (KsatMacropore-KFieldConductivity) * ((surfaceWaterStorage - FieldCapacityAmount) / SoilDepth) /  (SatMoistureContent - FieldCapacityAmount / SoilDepth) + KFieldConductivity))
  return(effK)
}

# Given input raster maps, update the storage raster map
storage_amount_raster <- function(rockPercent, saturatedMoistureContent, soilDepth){
  # Function changes the table values and returns two column list with NLCD land type and corresponding storage
  UpdatedstorageAmount <- (1- rockPercent) * saturatedMoistureContent * soilDepth
  # Create a map
  return(UpdatedstorageAmount)
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
effectiveConductivity <- function(surfaceWaterStorage, FieldCapacityAmount, KsatMatrix, SatMoistureContent, KsatMacropore, SoilDepth, KFieldConductivity){
  effK <- ifel(surfaceWaterStorage < FieldCapacityAmount,
               (KsatMatrix * exp((-13.0/SatMoistureContent) * (SatMoistureContent - surfaceWaterStorage / SoilDepth))),
               ifel(surfaceWaterStorage >= (SatMoistureContent * SoilDepth),
                    KsatMacropore,
                    (KsatMacropore - KFieldConductivity) * ((surfaceWaterStorage - FieldCapacityAmount) / SoilDepth) /  (SatMoistureContent - FieldCapacityAmount / SoilDepth) + KFieldConductivity))
  return(effK)
}

# Test

# Lateral flow minimum between Darcy's law and amount of water on the surface
# LaTEX definition
# min( (K_{eff}*\frac{slope}{100}*\frac{d_{soil}}{10*100}) |  Soil_{storage})
lateral_flow <- function(effectiveK, slope, soilDepth, waterStorage){
  lateral_flow <- min(effectiveK*(slope/100) * soilDepth / (10*100), waterStorage) # check units
  return(lateral_flow)
}

# Test
#storage_amount(LandCoverCharacteristics)

