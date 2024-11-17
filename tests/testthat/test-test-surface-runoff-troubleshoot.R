test_that("Surface routing trouble shootings", {
  ModelFolder <- "ws-test-2"
  elements <- template_watershed(ModelFolder = ModelFolder, dem = "dem-test.tif")
  rain_file <- elements[[2]]
  discharge <- terra::rast(file.path(ModelFolder, "model_dem.tif"))/terra::rast(file.path(ModelFolder, "model_dem.tif"))
  library(microbenchmark)
  flow_d8 <- terra::terrain(terra::rast(file.path(ModelFolder, "model_dem.tif")), v = "flowdir")
  flowMap1D(discharge, flow_d8, ModelFolder)
  microbenchmark(flowMap1D(discharge, flow_d8, ModelFolder), times = 10)
  plot(rast(file.path(ModelFolder, "model_dem.tif")))
  plot(x)
  flowModel(ModelFolder, rain_file, simulation_length = 3,impervious = T)

})

test_that("Surface routing optimization", {
  library(terra)
  set.seed(0)
  numbers <- sample.int(1000, size = 1000000, replace = T)
  m <- matrix(numbers, nrow = 1000)
  r <- terra::rast(m)
  d8 <- terra::terrain(r, v = "flowdir")
  discharge <- d8/d8
  flowMaps <- flowMap1D(discharge, flow_d8 = d8)
  terra::plot(flowMaps)

})
