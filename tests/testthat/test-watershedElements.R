test_that("Watershed-Creation works", {
  test_model <- model() # generic model
  watershedPath <- a@watershedPath
  WatershedShape <- desertHydro::polygonize( "dem-test.tif", a@watershedPath)
  test_model@LandCoverCharacteristics <- "nlcd_characteristics.xlsx"
  test_model@landCoverFile <- "waterholes_LC.tif"
  test_model@key <- "ID"

  WatershedStack <- watershedElementsCreate(ModelFolder = "watershed2", WatershedElements = watershedPath,
                          WatershedShape = file.path(watershedPath, WatershedShape),
                          DEM = file.path(watershedPath, "dem-test.tif"),
                          landCoverFile = file.path(watershedPath, test_model@landCoverFile),
                          LandCoverCharacteristics = file.path(watershedPath, test_model@LandCoverCharacteristics),
                          key = test_model@key
                          )

  save_folder <- file.path(getwd(), "watershed2")
  files <- list.files(save_folder)
  files_needed <- c("model_dem.tif", "model_slope.tif",
                    "stack_flow.tif", "model_soil_stack.tif",
                    "stream_network.shp")
  testthat::expect_equal(all(files_needed %in% files), TRUE)

  unlink(paste0(save_folder,"/*"))
  unlink(save_folder, recursive = T)
})

test_that("Initial Soil Conditions", {
  test_model <- model()

})

test_that("Land Cover Process", {
  # Creat initial necessary variables
  model1 <- model()
  ModelFolder <- tempdir()
  landCoverFile <- "waterholes_LC.tif"
  landCoverPath <- file.path(model1@watershedPath, landCoverFile)
  model_dem <- "dem-test.tif" # not altered
  model_dem_path <- file.path(model1@watershedPath, model_dem)
  watershed_elements_path <- model1@watershedPath
  # Creates shapefile of watershed outline in watershed_elements folder
  watershed_shape <- desertHydro::polygonize(model_dem, watershed_elements_path)
  watershed_shape_path <- file.path(watershed_elements_path, watershed_shape)
  key <- "ID"

  # Test vector land cover
  lcp <- land_cover_process(landCoverPath,
                            model_dem_path,
                            watershed_shape_path,
                            key)
})
