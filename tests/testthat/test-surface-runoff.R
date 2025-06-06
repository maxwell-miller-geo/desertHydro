test_that("surfaceRunoff function", {
  # Create necessary files
  test_model <- model() # generic model
  ModelFolder <- "test-ws"
  if(file.exists(ModelFolder)){
    all_files <- list.files(ModelFolder)
    file.remove(file.path(ModelFolder, all_files))
    unlink(ModelFolder)
  }
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
  surfaceStack <- c(SoilStack$model_dem,
    SoilStack$flow_direction,
    SoilStack$slope,
    SoilStack$mannings_n,
    SoilStack$throughfall,
    SoilStack$surfaceWater)

  # Make a smaller version of the surface stack
  extent <- terra::ext(-111.539166666852, -111.538, 36.8495, 36.850)
  surfaceStack <- terra::crop(surfaceStack, extent)
  # Create .01 inches of rainfall
  surfaceStack$throughfall <- surfaceStack$model_dem/surfaceStack$model_dem * .0254
  # Pass elements to surface routing
  surface <- surfaceRouting(surfaceStack, time_delta_s = 60, gridSize = 10)
  # SoilStack$surfaceWater <- surface[[1]]

  expect_equal(sumCells(surface), sumCells(surfaceStack$throughfall))

  ModelFolder <- "test-ws"
  if(file.exists(ModelFolder)){
    all_files <- list.files(ModelFolder)
    file.remove(file.path(ModelFolder, all_files))
    unlink(ModelFolder, recursive = T)
  }
})
test_that("Infiltration tests", {
  ModelFolder <- tempdir()
  flow_inputs <- template_watershed(ModelFolder)

  # Create elements necessary for surface water stack
  surfaceStack <- c(SoilStack$model_dem,
                    SoilStack$flow_direction,
                    SoilStack$slope,
                    SoilStack$mannings_n,
                    SoilStack$throughfall,
                    SoilStack$surfaceWater,
                    SoilStack$infiltration_cmhr)

  s_list <- surfaceRouting(surfaceStack, time_delta_s = 20, rain_step_min = 1)

  # Should return 0 infiltration!!
  # Remove the outputs from memory
  if(file.exists(ModelFolder)){
    all_files <- list.files(ModelFolder)
    file.remove(file.path(ModelFolder, all_files))
    unlink(ModelFolder)
  }
})
