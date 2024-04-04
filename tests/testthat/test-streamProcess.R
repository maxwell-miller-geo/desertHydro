test_that("stream cuts", {
  require(terra)
  stream <- system.file("extdata", "stream_analysis.shp", package = "desertHydro")
  dem <- system.file("extdata", "dem.tif", package = "desertHydro")
  streamBurn(stream, dem)

})
