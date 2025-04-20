# test_that("Create flow maps", {
#   a <- model()
#   dem_path <- file.path(a@watershedPath,"dem-test.tif")
#   discharge <- terra::rast(dem_path)/terra::rast(dem_path)
#   flowMaps <- flowMap1D(discharge, dem_path = dem_path)
#   expect_equal(terra::nlyr(flowMaps), 9)
#
#   # Test with larger map area
#   dem_path <- file.path(a@watershedPath,"dem.tif")
#   flow_d8 <- file.path(tempdir(), "temp-flow.tif")
#   whitebox::wbt_d8_pointer(dem_path, output = flow_d8, esri_pntr = T)
#   crsAssign(flow_d8)
#   discharge <- terra::rast(dem_path)/terra::rast(dem_path)
#   flowMaps <- flowMap1D(discharge, terra::rast(flow_d8))
#   expect_equal(terra::nlyr(flowMaps), 9)
#   rm(flow_d8)
# })
#
# test_that("1D flow is accounted for", {
#   a <- model()
#   dem_path <- file.path(a@watershedPath,"dem-test.tif")
#   discharge <- terra::rast(dem_path)/terra::rast(dem_path)
#   flowMaps <- flowMap1D(discharge, dem_path = dem_path)
#   flow_sum <- sum(terra::values(sum(flowMaps, na.rm = T), na.rm = T))
#   # Count how many holes in dem
#   hole_temp <- file.path(tempdir(), "temp-holes.tif")
#   whitebox::wbt_find_no_flow_cells(dem_path, hole_temp)
#   holes <- terra::rast(hole_temp)
#   h_count <- sum(terra::values(holes, na.rm = T))
#   # discharge count
#   d_sum <- sum(terra::values(discharge, na.rm = T))
#   # Compare flow values
#   expect_equal(flow_sum, d_sum)
# })
#
# test_that("Water is flowing in all directions", {
#   a <- model()
#   ModelFolder <- tempdir()
#   dem_path <- file.path(a@watershedPath,"dem-test.tif")
#   model_dem <- file.path(ModelFolder, "model_dem.tif")
#   # Adjust the dem
#   dem_adjustment(dem_path, model_dem)
#
#   flow_d8 <- file.path(tempdir(), "fd8.tif")
#   whitebox::wbt_d8_pointer(model_dem, flow_d8, esri_pntr = T)
#   crsAssign(flow_d8, get_crs(model_dem))
#   flow <- terra::rast(flow_d8)
#   example_discharge <- terra::rast(model_dem) / terra::rast(model_dem)
#   sum_discharge <- sum(terra::values(example_discharge, na.rm = T))
#
#   # Create flow maps
#   flowMaps <- flowMap1D(example_discharge, dem_path = model_dem)
#   s <- sum(flowMaps, na.rm = T)
#   # Get value of the outflow cell in map
#   sum_out <- as.numeric(example_discharge[get_out_cell(dem_path = model_dem)])
#   flow_sum <- sum(terra::values(s, na.rm = T))
#   # problems <- terra::ifel(s == 0, 1, 0)
#   # d <- problems * flow
#   # Check if all discharge is accounted for
#   expect_equal(flow_sum, sum_discharge)
# })
#
# test_that("Mini-flow map works", {
#   #v <- c(2,3,4,1,3,5,2,3,5)
#   v <- c(1,2,2,2)
#   dem <- terra::rast(matrix(v, nrow = 2), crs = "epsg:26911")
#   names(dem) <- "model_dem"
#   dem_path <- file.path(tempdir(), "dem.tif")
#   terra::writeRaster(dem, dem_path, overwrite=T)
#   # D8 flow direction
#   d8_flow <- file.path(tempdir(), "d8_flow.tif")
#   whitebox::wbt_d8_pointer(dem_path, d8_flow, esri_pntr = T)
#   crsAssign(d8_flow, "epsg:26911")
#   flow <- terra::rast(d8_flow)
#   names(flow) <- "flow_direction"
#   # Calculate slope
#   slope_og <- terra::terrain(dem)
#   cellsize <- 1
#   slope <- slope_edge(dem, slope_og, cellsize = cellsize)
#   # Mannings n
#   mannings <- slope/slope * .1
#   names(mannings) <- "mannings_n"
#   throughfall <- slope/slope * .0254
#   names(throughfall) <- "throughfall"
#   surface <- slope/slope * 0
#   names(surface) <- "surfaceWater"
#   surfaceStack <- c(dem, flow, slope, mannings, throughfall, surface)
#
#   time_delta_s <- time_delta(surfaceStack, gridSize = cellsize, courant_condition = .9)
#   runoffList <- surfaceRouting(surfaceStack, time_delta_s = time_delta_s, gridSize = cellsize)
#   surfaceStack$surfaceWater <- runoffList
#   ## Remove water
#   # Remove water from most downstream cell - how much water?
#   outFlowCell <- 1 # needs to be adjusted for real simulation
#   qOut <- manningsVelocity(surfaceStack$mannings_n[outFlowCell], surfaceStack$surfaceWater[outFlowCell],slope = surfaceStack$slope[1], length = cellsize) * surfaceStack$surfaceWater[outFlowCell]
#
#   # Assign new height to outflow cell
#   surfaceStack$surfaceWater[outFlowCell] <- surfaceStack$surfaceWater[outFlowCell] - surfaceStack$surfaceWater[outFlowCell]
#
#   # Run 1 more time to check
#   time_delta_s <- time_delta(surfaceStack, gridSize = cellsize, courant_condition = .9)
#   runoffList <- surfaceRouting(surfaceStack, time_delta_s = time_delta_s, gridSize = cellsize)
#   #surfaceStack$surfaceWater <- runoffList[[1]]
#   rain_added <- throughfall/60 *time_delta_s
#   # Where is the water missing from?
#   missing_water <- surfaceStack$surfaceWater + rain_added - runoffList
#   expect_equal(sumCells(surfaceStack$surfaceWater) + sumCells(rain_added), sumCells(runoffList))
# })
