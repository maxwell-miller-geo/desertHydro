# Create demo watershed
# This script contains the details are processes to create a demo
# 10 x 10 digital elevation model to testing functions for the main flow model

# library(terra)
## ---------------------------
# Creates demo raster
# test <- terra::rast(r"(C:\Thesis\Arid-Land-Hydrology\R\test_water_rast.tif)")
# # Create 10 x 10 matrix example matrix
# grid <- mapply(base::rep, 100:91, 10) # create matrix 100 - 91
# grid[5,] <- grid2[5,] - 1  # Carve the center channel
# demo <- terra::rast(grid) # create temporary raster
# values(test) <- values(demo) # assign values to real raster extent
# # Save raster
# outfile <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\DemoElements)"
# terra::writeRaster(test, filename = file.path(outfile, "demo_dem.tif"), overwrite = T)

