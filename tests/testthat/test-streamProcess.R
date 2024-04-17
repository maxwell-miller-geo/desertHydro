test_that("stream cuts - class", {
  requireNamespace("terra")
  stream <- system.file("extdata", "stream_analysis.shp", package = "desertHydro")
  dem <- system.file("extdata", "dem.tif", package = "desertHydro")
  expect_equal(file.exists(stream), file.exists(dem))
  #expect_equal(class(terra::rast(dem)), class(smoothStream(stream, dem)))
})
