test_that("Watershed-Creation works", {
  a <- model() # generic model
  watershedPath <- a@watershedPath
  WatershedShape <- desertHydro::polygonize( "dem-test.tif", a@watershedPath)

  watershedElementsCreate(ModelFolder = "watershed2", WatershedElements = watershedPath,
                          WatershedShape = file.path(watershedPath, WatershedShape),
                          DEM = file.path(watershedPath, "dem-test.tif"),
                          landCoverFile = file.path(watershedPath, a@landCoverFile),
                          LandCoverCharacteristics = file.path(watershedPath, a@LandCoverCharacteristics),
                          key = a@key
                          )
  save_folder <- file.path(getwd(), "watershed2")
  files <- list.files(save_folder)
  files_needed <- c("model_dem.tif", "model_slope.tif", "stack_flow.tif", "model_soil_stack.tif",
                    "stream_network.shp")
  testthat::expect_equal(all(files_needed %in% files), TRUE)
  unlink(save_folder)
})

test_that("Flow Accumulation ", {

})
