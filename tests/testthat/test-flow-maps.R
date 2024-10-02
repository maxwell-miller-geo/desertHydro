test_that("Create flow maps", {
  a <- model()
  dem_path <- file.path(a@watershedPath,"dem-test.tif")
  discharge <- terra::rast(dem_path)/terra::rast(dem_path)
  flowMaps <- flowMap1D(discharge, dem_path = dem_path)
  expect_equal(terra::nlyr(flowMaps), 8)

  # Test with larger map area
  dem_path <- file.path(a@watershedPath,"dem.tif")
  flow_d8 <- file.path(tempdir(), "temp-flow.tif")
  whitebox::wbt_d8_pointer(dem_path, output = flow_d8)
  crsAssign(flow_d8)
  discharge <- terra::rast(dem_path)/terra::rast(dem_path)
  flowMaps <- flowMap1D(discharge, terra::rast(flow_d8))
  expect_equal(terra::nlyr(flowMaps), 8)
  rm(flow_d8)
})

test_that("1D flow is accounted for", {
  a <- model()
  dem_path <- file.path(a@watershedPath,"dem-test.tif")
  discharge <- terra::rast(dem_path)/terra::rast(dem_path)
  flowMaps <- flowMap1D(discharge, dem_path = dem_path)
  flow_sum <- sum(values(sum(flowMaps, na.rm = T), na.rm = T))
  # Count how many holes in dem
  hole_temp <- file.path(tempdir(), "temp-holes.tif")
  whitebox::wbt_find_no_flow_cells(dem_path, hole_temp)
  holes <- terra::rast(hole_temp)
  h_count <- sum(values(holes, na.rm = T))
  # discharge count
  d_sum <- sum(values(discharge, na.rm = T))
  # Compare flow values
  expect_equal(h_count + flow_sum, d_sum)
})

test_that("Water is flowing in all directions ,"{
  a <- model()
  ModelFolder <- tempdir()
  dem_path <- file.path(a@watershedPath,"dem-test.tif")
  model_dem <- file.path(ModelFolder, "model_dem.tif")
  # Adjust the dem
  dem_adjustment(dem_path, model_dem)
  flow_d8 <- file.path(tempdir(), "fd8.tif")
  whitebox::wbt_d8_pointer(model_dem, flow_d8)
  crsAssign(flow_d8, get_crs(model_dem))
  flow <- terra::rast(flow_d8)
  example_discharge <- terra::rast(model_dem) / terra::rast(model_dem)
  flowMaps <- flowMap1D(example_discharge, dem_path = model_dem)
  s <- sum(flowMaps, na.rm = T)

  problems <- terra::ifel(s == 0, 1, 0)
  d <- problems * flow
})
