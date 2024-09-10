test_that("Watershed-Creation works", {
  test_model <- model() # generic model
  watershedPath <- test_model@watershedPath
  WatershedShape <- desertHydro::polygonize( "dem-test.tif", test_model@watershedPath)
  test_model@LandCoverCharacteristics <- "nlcd_characteristics.xlsx"
  test_model@landCoverFile <- "waterholes_LC.tif"
  test_model@key <- "ID"

  WatershedStack <- watershedElementsCreate(ModelFolder = "watershed2", WatershedElements = watershedPath,
                          watershed_shape_path = file.path(watershedPath, WatershedShape),
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
  # Output
  model_landcover <- file.path(ModelFolder, "model_landcover.tif")
  # Test vector land cover
  land_cover_raster <- land_cover_process(landCoverPath,
                            model_dem_path,
                            watershed_shape_path,
                            key,
                            model_landcover = model_landcover,
                            save = F)
  # Compare the number of values in land cover with values in dem
  dem_count <- nrow(values(terra::rast(model_dem_path), na.rm = T))
  land_count <- nrow(values(land_cover_raster, na.rm = T ))
  expect_equal(dem_count, land_count)
})

test_that("Watershed set up- Full", {
  # Need 3 items
  model1 <- model()
  ModelFolder <- tempdir()
  WatershedElements <- model1@watershedPath
  dem <- "dem-test.tif" # not altered
  dem_path <- file.path(model1@watershedPath, dem)
  watershed_shape <- desertHydro::polygonize(dem, WatershedElements)
  watershed_shape_path <- file.path(WatershedElements, watershed_shape)

  flow_accumlation_wb(dem_path, ModelFolder = ModelFolder,
                      watershed_shape_path = watershed_shape_path,
                      overwrite = T)

  files <- list.files(ModelFolder)
  # Files needed for model simulation
  files_needed <- c("model_dem.tif",
                    "drainPoints.shp",
                    "flow_accumulation.tif",
                    "vect_stream.shp")
  files_present <- files_needed %in% files
  print(files_present)
  testthat::expect_equal(all(files_present), TRUE)

  # Model dem
  model_dem <- file.path(ModelFolder, "model_dem.tif")
  # Test slope is present
  slope <- file.path(ModelFolder, "model_slope.tif")
  slope_temp <- terra::terrain(terra::rast(model_dem), v = "slope", neighbors = 8, unit = "degrees")
  edges <- terra::focal(slope_temp, w = 3, "modal", na.policy = "only", na.rm = F)
  names(edges) <- "slope"
  # NOTE - slope does not compute for boundary cells
  terra::writeRaster(edges, filename = slope, overwrite = T)
  # Test slope file exists
  testthat::expect_equal(file.exists(slope), TRUE)

  # Model Land Cover
  model_landcover <- file.path(ModelFolder, "model_landcover.tif")
  ModelFolder <- tempdir()
  landCoverFile <- "waterholes_LC.tif"
  landCoverPath <- file.path(model1@watershedPath, landCoverFile)
  key = "ID"
  land_cover_raster <- land_cover_process(landCoverPath = landCoverPath,
                                          model_dem =model_dem,
                                          watershed_shape_path = watershed_shape_path,
                                          model_landcover = model_landcover,
                                          key = key,
                                          save = T)
  # Model land cover created test
  testthat::expect_equal(file.exists(model_landcover), TRUE)

  # Stack flow test
  # Flow Calculations
  flow_filename <- "stack_flow.tif" # must be created with names of layers
  flow_file <- file.path(ModelFolder, flow_filename)
  flowStack <- flowMap(dem = model_dem, outFolder = ModelFolder)
  testthat::expect_equal(file.exists(flow_file), TRUE)

  # Stack them all up
  WatershedStack <- c(terra::rast(model_dem),
                      terra::rast(slope),
                      terra::rast(flow_file),
                      terra::rast(model_landcover))
  # Pass Watershed Stack to Initial Soil Conditions
  ClassificationMap <- model_landcover
  LandCoverCharacteristics <- file.path(WatershedElements, "nlcd_characteristics.xlsx")

  SoilStack <- initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
                          ClassificationMap = ClassificationMap,
                          WatershedStack = WatershedStack,
                          outline = watershed_shape_path,
                          ModelFolder = ModelFolder,
                          WatershedElements = WatershedElements,
                          key = key,
                          overwrite = T
                          )

  testthat::expect_equal("mannings_n" %in% names(SoilStack), TRUE)

  unlink(paste0(ModelFolder,"/*"))
  #unlink(save_folder, recursive = T)
})

test_that("Initial Soil Conditions set up", {
  # LandCoverCharacteristics <- file.path(WatershedElements, "nlcd_characteristics.xlsx")
  #
  # initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
  #                         ClassificationMap = ClassificationMap,
  #                         WatershedStack = WatershedStack,
  #                         outline = watershed_shape_path,
  #                         ModelFolder = ModelFolder,
  #                         WatershedElements = WatershedElements,
  #                         key = key,
  #                         overwrite = T
  # )
})
