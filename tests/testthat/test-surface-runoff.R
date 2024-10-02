test_that("surfaceRunoff function", {
  # Create necessary files
  test_model <- model() # generic model
  ModelFolder <- "test-ws"
  watershedPath <- test_model@watershedPath
  WatershedShape <- desertHydro::polygonize( "dem-test.tif", test_model@watershedPath)
  test_model@LandCoverCharacteristics <- "nlcd_characteristics.xlsx"
  test_model@landCoverFile <- "waterholes_LC.tif"
  test_model@key <- "ID"

  SoilStack <- watershedElementsCreate(
    ModelFolder = ModelFolder,
    WatershedElements = watershedPath,
    watershed_shape_path = file.path(watershedPath, WatershedShape),
    DEM = file.path(watershedPath, "dem-test.tif"),
    landCoverFile = file.path(watershedPath, test_model@landCoverFile),
    LandCoverCharacteristics = file.path(watershedPath, test_model@LandCoverCharacteristics),
    key = test_model@key
  )
  # Create elements necessary for surface water stack
  surfaceStack <- c(
    SoilStack$model_dem,
    SoilStack$flow_direction,
    SoilStack$slope,
    SoilStack$mannings_n,
    SoilStack$throughfall,
    SoilStack$surfaceWater
                    )

  # Create .01 inches of rainfall
  surfaceStack$throughfall <- SoilStack$model_dem/SoilStack$model_dem * .0254
  # Pass elements to surface routing
  surfaceRouting(surfaceStack, ModelFolder = ModelFolder)

})
