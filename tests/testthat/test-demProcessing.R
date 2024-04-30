test_that("File presence check", {
  dem_file <- system.file("extdata", "dem.tif", package = "desertHydro")
  flow_direction <- file.path(tempdir(), "flow.tif")
  expect_equal(TRUE, file.exists(dem_file))
  # whitebox::wbt_d8_pointer(dem_file, flow_direction)
  # testthat::expect_equal(TRUE, file.exists(flow_direction))
})
