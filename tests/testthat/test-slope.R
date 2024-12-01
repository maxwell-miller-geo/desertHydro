test_that("test-slope-function", {
  model1 <- model()
  dem <- terra::rast(file.path(model1@watershedPath, model1@demFile))
  slope <- slopeCalculate(dem)
  new_slope <- slope_edge(slope, dem, cellsize = 10,cpp = F)
  cpp_slope <- slope_edge(slope, dem, cellsize = 10,cpp = T)
})
