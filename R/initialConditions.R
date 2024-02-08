# Watershed Model Initial Conditions
# The default initial conditions for a given watershed.
# Note: Some conditions may not be utilized for a given scenario.

# Necessary packages
# library(terra)


initial_conditions <- function(ModelOutputs, model_dem, gridsize = 10.0){
  # Load in the raster version
  local_dem <- terra::rast(model_dem)
  # Constants for model
  minimum_elevation <- terra::minmax(local_dem)[1] # meters
  maximum_elevation <- terra::minmax(local_dem)[2] # meters
  basin_area_m <- terra::expanse(local_dem, unit = "m")[[2]] # basin area meters^2
  basin_area_hec <- terra::expanse(local_dem, unit = "ha")[[2]]# basin area hectares
  gridsize <- gridsize # meters

  recession_con_a <- 75 #input for perc2runoff non-linear percolation reservoir model -- higher value flattens regression curve and peaks, lower value heightens initial value and rapidly drops
  reces_con_b <- 0.2 #input for perc2runoff non-linear percolation reservoir model -- higher value flattens regression curve and peaks, lower value raises peaks (0.1-0.9) and rapidly drops
  #balance decreases in b with increases in a; decreases in b cause large changes that can be tempered be increases in a => about 30 in a = 0.1 in b but parabola curvature is different
  #for slight changes in peaky response, change a

  reces_con_c  <- 0.02 #input for perc2runoff linear percolation reservoir model
  runoff <- 0.0 #initial run placeholder
  new_runoff <- 0.0 #initial run placeholder
  runoff_total <- 0.0 #initial run placeholder
  canopy_storage_amount <- 0.0 # initial storage
  cum_perc <- 0.0 # initial value -- greatly affects discharge
  perc2runoff <- 0.0

  date <- Sys.Date()
  time <- Sys.time()

  # Assign conditions into a variable list
  variable_list <- data.frame(date, time, minimum_elevation, maximum_elevation, basin_area_m,
                              basin_area_hec, gridsize, recession_con_a,
                              reces_con_b, reces_con_c, runoff,
                              runoff_total, canopy_storage_amount, cum_perc,
                              perc2runoff)

  # save the initial constants for a particular run and the initial soil conditions
  write.csv(variable_list, file.path(ModelOutputs, "set_up_variables.csv"), row.names = F)
  print(paste("Initial conditions can be found at", file.path(ModelOutputs, "set_up_variables.csv")))
}

# Test
# ModelOutputs <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)"
