test_that("Surface routing trouble shootings", {
  ModelFolder <- "ws-test-2"
  elements <- template_watershed(ModelFolder = ModelFolder, dem = "dem.tif")
  rain_file <- elements[[2]]

  flowModel(ModelFolder, rain_file, simulation_length = 3,impervious = T)
})
