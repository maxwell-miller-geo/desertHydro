# Utility
# This script contains functions that contain functions that I don't know where else
# to put

# Function that creates and plots linear model
quadratic_lm <- function(X, Y, poly = T){
  ones <- matrix(1, nrow = length(Y))
  Xd <- cbind(ones, X, X^2) # Polynomial regression
  # Solve Linear Regression with Matrices
  XX <- t(Xd) %*% Xd
  XY <- t(Xd) %*% Y
  B <- solve(XX) %*% XY
  Yhat <- Xd %*% B
  Yplot <- B[1] + B[2]*X + B[3]*X^2
  #ggplot()+geom_point(aes(X1,Y), color = "black")+ geom_line(aes(X1, Yplot), color = "red")
  return(B) # returns coefficients
}

# # Test function
# X <- rain_discharge$height
# Y <- rain_discharge$discharge
# quad_test <- quadratic_lm(X,Y)

# ---------------------------------
# Function that finds maximum values in layer, creates dataframe
dfMax <- function(raster){
  idx <- terra::where.max(raster)[2] # gets max value cell number
  pos <-  terra::xyFromCell(raster,idx) # gets xy
  maxValue <- terra::minmax(raster)[2]
  name <- names(raster)
  return(data.frame(time = name, x = pos[1], y = pos[2], max = maxValue))
}
# Test
# raster <- surfaceStorage[[5]]
# dfMax(raster)
# ---------------------------------
# Function that creates vector file from data.frame
vectCreation <- function(df, saveLoc, name, coords){ # df must contain xy
  shapefile <- terra::vect(df, geom = c("x", "y"), crs = crs(coords))
  writeVector(shapefile, filename = file.path(ModelFolder, name), overwrite =T)
  return(shapefile)
}

# Test
# vectName <- "depth_max.shp"
# vector_shapefile <- terra::vect(data_out, geom = c("x", "y"), crs = crs(SoilStack)) 
# writeVector(vector_shapefile, filename = file.path(ModelFolder, vectName))



