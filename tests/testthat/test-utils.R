test_that("Locate cell values", {
  set.seed(1)
  test_raster <- terra::rast(matrix(runif(100), nrow = 10))
  sortedValues <- sort(terra::values(test_raster), decreasing = T)[2]
  # Obtain values
  cellLocation <- terra::cells(test_raster, sortedValues)[[1]]
  expect_equal(sortedValues, test_raster[cellLocation][[1]])
})

test_that("Convert string", {

})
