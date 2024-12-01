test_that("Memory test", {
  ModelFolder <- "Results/test-watershed"
  elements <- template_watershed(ModelFolder = ModelFolder, dem = "dem.tif")
  rain_file <- elements[[2]]
  library(profvis)
  profvis(flowModel(ModelFolder, rain_file, simulation_length = 3))

})
