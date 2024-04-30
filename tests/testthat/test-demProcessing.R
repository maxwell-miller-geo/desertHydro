test_that("Whitebox tool check", {
  dem_file <- system.file("extdata", "dem.tif", package = "desertHydro")
  flow_direction <- file.path(tempdir(), "flow.tif")
  whitebox::wbt_d8_pointer(dem_file, flow_direction)
  testthat::expect_equal(TRUE, file.exists(flow_direction))
})
