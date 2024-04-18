test_that("stream cuts - class", {
  requireNamespace("terra")
  stream <- system.file("extdata", "stream_analysis.shp", package = "desertHydro")
  dem <- system.file("extdata", "dem.tif", package = "desertHydro")
  expect_equal(file.exists(stream), file.exists(dem))
  #expect_equal(class(terra::rast(dem)), class(smoothStream(stream, dem)))
})

test_that("Smooth vector check", {
  negs <- c(1,2,3,4,5,4,6)
  sm <- smoothVector(negs)
  sm_diff <- diff(sm)
  sm_final <- c(sm_diff, sm_diff[1])
  expect_equal(abs(sum(sign(sm_final))), length(sm))
  poss <- c(6,4,5,3,2,1)
  sl <- smoothVector(poss)
  sl_diff <- diff(sl)
  sl_final <- c(sl_diff, sl_diff[1])
  expect_equal(abs(sum(sign(sl_final))), length(sl)) # all signs should be the same

})
