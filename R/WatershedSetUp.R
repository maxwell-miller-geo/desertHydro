# Watershed Characteristics
# The purpose of this script is to take the following inputs and adjust the input data for a given watershed
# 1 - Digital Elevation Model (DEM)
# 2 - Land Cover Characteristics
# 3 - Watershed shapefile
# 4 - Voronoi polygons
# Some of the scripts are created in external functions

watershedElementsCreate <- function(ModelFolder, WatershedElements, DEM, watershed_shape_path, LandCoverCharacteristics, landCoverFile, landcovername = "landcover_soil.tif", key = "MUSYM", overwrite = T, cellsize = 10){ # DEM should be unaltered
  #requireNamespace("terra")
  # DEM adjustments
  # Adjust the input DEM with the watershed shapefile.
  # Creates the following maps using the Whitebox package <- https://github.com/cran/whitebox
  if(!file.exists(ModelFolder)){
    dir.create(ModelFolder)
  }
  if(file.exists(file.path(ModelFolder, "model_soil_stack.tif"))){
    print("Found model soil stack in model folder, using that file!")
    return(file.path(ModelFolder, "model_soil_stack.tif"))
  }
  model_dem <- file.path(ModelFolder, "model_dem.tif") # hard coded - not great Watershed ELements previously
  print('Locating adjusted digital elevation model.')

  if(!file.exists(model_dem) | overwrite){
    print('Creating adjusted digital elevation model.')
    flow_accumlation_wb(dem_file_path = DEM,
                        ModelFolder = ModelFolder,
                        watershed_shape_path = watershed_shape_path,
                        overwrite = overwrite) # Does not overwrite - creates many rasters
    print('Finished creating adjusted DEM.')
  } else{
    print("Located DEM")
    #print("Copying digital elevation model over to model folder.")
    #file.copy(model_dem, ModelFolder, recursive= T)
  }
  # Slope creation
  slope <- file.path(ModelFolder, "model_slope.tif") # default name of slope file
  print('Locating slope file')
  if(!file.exists(slope) | overwrite){
    # Expects one of the outputs from previous function to produce "cropped_dem.tif"
    #cropped_dem <- file.path(WatershedElements, "cropped_dem.tif")
    slope_temp <- terra::terrain(terra::rast(model_dem), v = "slope", neighbors = 8, unit = "degrees")
    slope_total <- slope_edge(model_dem, slope_temp, cellsize = cellsize)
    names(slope_total) <- "slope"
    # NOTE - slope does compute for boundary cells - see slope_edge()
    terra::writeRaster(slope_total, filename = slope, overwrite = T)
  }
  # Determine potential troublesome cells # assumes path written in
  trouble <- terra::rast(file.path(ModelFolder, "stream_extracted.tif")) * slope_total
  terra::writeRaster(trouble, file.path(ModelFolder, "trouble-cells.tif"), overwrite = T)

  # Land Cover
  # Project the land cover data into same coordinate system of the DEM
  # Check if the landcover raster is already there
  model_landcover <- file.path(ModelFolder, "model_landcover.tif")
  fileExists <- file.exists(model_landcover)
  print('Locating adjusted land cover map.')
  if(!fileExists | overwrite){
      #source("createWatershedModel.R", local = TRUE) # Land Cover script
      print("Processing land cover...")
      #landCoverFile <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # school desktop
      land_cover_raster <- land_cover_process(landCoverFile,
                                              model_dem,
                                              watershed_shape_path,
                                              model_landcover = model_landcover,
                                              key = key,
                                              save = T,
                                              ModelFolder = ModelFolder)

      print("Land Cover file clipped and resized.")
  } else{
    # Local land cover raster
    print("Found Land Cover raster.")
    land_cover_raster <- terra::rast(model_landcover)
  }
  # Flow Calculations
  flow_filename <- "stack_flow.tif" # must be created with names of layers
  flow_file <- file.path(ModelFolder, flow_filename)

  print('Locating flow partition map.')
  if(!file.exists(flow_file) | overwrite){ # Can take a few minutes if not already created
    print("No flow partition map found.")
    print("Creating flow partition map.")
    method <- "flowMap1D"
    if(method == "flowMap1D"){
      discharge <- terra::rast(model_dem)/ terra::rast(model_dem)
      flowStack <- flowMap1D(discharge, dem_path = model_dem)
      terra::writeRaster(flowStack, flow_file)
    }else{
      flowStack <- flowMap(dem = model_dem, outFolder = ModelFolder)
    }
    print("Flow partition map created.")
  }else{
    print('Found flow partition map.')
    flowStack <- terra::rast(flow_file) + 0
  }
  print("Checking created flow calculations extent.")
  # Crop the flow calculations raster to the extent of the land cover. If needed, overwrite previous data
  if(!(dim(flowStack)[1] == dim(land_cover_raster)[1] & dim(flowStack)[2] == dim(land_cover_raster)[2])){
    copyflowStack <- terra::crop(flowStack, land_cover_raster, snap = "near")
    terra::writeRaster(copyflowStack, flow_file, overwrite = T)
  }

  # Create flow direction map to add to watershed stack
  flow_direction <- file.path(ModelFolder, "flow_direction.tif")
  whitebox::wbt_d8_pointer(model_dem, flow_direction)
  crsAssign(flow_direction, get_crs(model_dem))
  # Stack them all up
  WatershedStack <- c(terra::rast(model_dem),
                      terra::rast(slope),
                      terra::rast(flow_file),
                      terra::rast(model_landcover),
                      terra::rast(flow_direction))

  terra::writeRaster(WatershedStack, file.path(ModelFolder, "watershed_stack.tif"), overwrite = T)
  print(paste0("Finished creating/checking files. Watershed elements files located in: ", ModelFolder))
  # Check files were written into folder
  #return(WatershedStack)

  ClassificationMap <- model_landcover # adjusted/cropped classification map - must be named correctly
  # Overwrites Soil Stack
  initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
                          ClassificationMap = ClassificationMap,
                          WatershedStack = WatershedStack,
                          outline = watershed_shape_path,
                          ModelFolder = ModelFolder,
                          WatershedElements = WatershedElements,
                          key = key,
                          overwrite = overwrite
                          )
  # Doesn't return anything...
  SoilStack <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
  return(SoilStack)
}

# Test
# fileWatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)"
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)"
# land_cover_path <-  r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # school desktop
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)"
#
# watershedElements(WatershedElements = fileWatershedElements, DEM = dem_path, LandCover = land_cover_path, watershed_shape_path = watershed_shape_path)

## Create voronoi polygons - not run
createVoronoi <- function(coords, combined, shapefile, write = F){ # Not run
  # Function is hard coded for example (waterholes)
  points <- terra::vect(matrix(combined, nrow = length(coords)/2, byrow = T)) # matrix with points
  shapefile <- terra::vect(shapefile)
  lines <- terra::voronoi(points, bnd = shapefile, as.lines = T)
  df <- terra::geom(lines)[c(1:2,5:6),] # convert points to dataframe
  line_1 <- terra::as.lines(df)

  matrix_points <- as.matrix(data.frame(df$x, df$y))
  line_split <- terra::vect(df, type = "lines", crs = terra::crs(shapefile)) # creates points

  watershed_split <- terra::split(shapefile, line_split)
  terra::writeVector(watershed_split, "voronoi-test.shp", filetype = "ESRI Shapefile", overwrite = T)
}

## Set up in mini model -------------------------------------
# Clip the input shape file of the watershed by an extent
# shape <- terra::vect(r"(C:\\Thesis\\Arid-Land-Hydrology\\R\\Example\\WatershedElements/waterholes_shape.shp)")
# extent <- terra::ext(-111.539814814976, -111.49617, 36.8323, 36.8616666671242) # extent of mini raster
# smallerExt <- terra::ext(-111.539814814976, -111.51057, 36.84238, 36.8616666671242)
#
# mini <- terra::crop(shape, smallerExt)
# WatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\MiniWatershedElements)"
# writeVector(mini, filename = file.path(WatershedElements, "mini_ws.shp"))

## After mini vector has been selected, it should work with the overall model now..

# # Test - hardcoded right now
# # Hard-coded points for Waterholes gauges
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)"
# shapefile <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)"
# water1 <- c(-111.491997, 36.770302)
# water2 <- c(-111.491898, 36.824200)
# waterg <- c(-111.539467, 36.849217)
# coords <- c(water1, water2, waterg) # coordinates for 3 rainfall gauges
# points <- terra::vect(matrix(combined, nrow=3, byrow = T)) # matrix with points
# extent <- terra::ext(rast(dem_path))
# z <- terra::split(shape, q)
# plot(z)
# crs(z) <- crs(shape)
# p <- as.data.frame(geom(q))
# writeVector(z, "voronoi-test.shp", filetype = "ESRI Shapefile", overwrite = T)
# plot(vect("voronoi-test.shp"))
## ---------------------------- Function to process landcover
geologyProcess <- function(landCoverShape, SoilStack, WatershedElements, ModelFolder, key = "GEOFNT24K"){

  if(is.character(landCoverShape)){
    landCoverShape <- terra::vect(landCoverShape)
  }
  if(is.character(SoilStack)){
    SoilStack <- terra::rast(SoilStack)
  }
  # project land cover into soils layer
  reproject <- terra::project(landCoverShape, SoilStack)
  if(key %in% names(reproject)){
    geokey <- key
  } else{
    stop("Could not find key to match the geologic map. Check input key.")
  }
  # Rasterize land cover map
  landCoverRast <- terra::rasterize(reproject, SoilStack, geokey)
  # Geology excel adjustments
  geo_excel <- readxl::read_xlsx(file.path(WatershedElements, "geo_adjustments.xlsx"))
  adjustmentMaps <- createSoilRasters(landCoverRast, geo_excel, geokey)
  terra::writeRaster(adjustmentMaps, file.path(ModelFolder, "geomaps.tif"), overwrite = T)
  return(adjustmentMaps)
  #uni <- terra::intersect(soils, reproject)
  #terra::writeVector(uni, file.path(WatershedElements, "geo_soils.shp"))
  # Load in land cover excel sheet - hard coded
  #LandCoverCharacteristics <- readxl::read_xlsx(r"(C:\Thesis\Arid-Land-Hydrology\R\WatershedElements\LandCoverCharacteristics_Soils.xlsx)")
  # hard coded flow
}

land_cover_process <- function(landCoverPath, model_dem, watershed_shape_path, key, ModelFolder, model_landcover = NULL, save = T, overwrite = T, ...){
  # Output model land-cover
  if(is.null(model_landcover)){
    model_landcover <- file.path(ModelFolder, "model_landcover.tif") # must pass ModelFolder
  }
  extension <- sub(".*\\.", "", landCoverPath) # use regular expression to get file extension
  if(extension == "shp"){
    land_cover <- terra::vect(landCoverPath)
  }else{
    land_cover <- terra::rast(landCoverPath)
  }
  dem_local <- terra::rast(model_dem)
  land_cover_raster <- resizeShape(spatialObject = land_cover,
                                   extent_raster = dem_local,
                                   watershedboundary = watershed_shape_path,
                                   key = key,
                                   save = F)
  # Create model land cover raster
  if(save){
    terra::writeRaster(land_cover_raster, model_landcover, overwrite = overwrite)
  }
  return(land_cover_raster)
}


# Create a "template" version of model parameters without having to run through everything everytime
template_watershed <- function(ModelFolder = "test-watershed", model_object = NULL, WatershedElements = NULL, date = "2021-07-21", rainfall_method = "gauges", dem = "dem-test.tif"){

  # Use default model object
  if(is.null(model_object)){
    model_object <- model()
    WatershedElements <- model_object@watershedPath
    # Append path to lists of objects
    model_object@demFile <- file.path(model_object@watershedPath, dem)
    model_object@boundary <- file.path(model_object@watershedPath, model_object@boundary)
    model_object@LandCoverCharacteristics <- file.path(model_object@watershedPath, model_object@LandCoverCharacteristics)
    model_object@landCoverFile <- file.path(model_object@watershedPath, model_object@landCoverFile)
  }

  # Files needed
  SoilStack <- file.path(ModelFolder, "model_soil_stack.tif")
  drainCells <- file.path(ModelFolder, "drainCells.csv")
  if(!file.exists(SoilStack) | !file.exists(drainCells)){
    # Create model soil stack
    watershedElementsCreate(ModelFolder, WatershedElements,
                            DEM = model_object@demFile,
                            watershed_shape_path = model_object@boundary,
                            LandCoverCharacteristics = model_object@LandCoverCharacteristics,
                            landCoverFile = model_object@landCoverFile,
                            key = model_object@key,
                            overwrite = F,
                            cellsize = 10
                            )
  }
  if(!file.exists(SoilStack) | !file.exists(drainCells)){
    stop("The model soil stack or the drainage cells were not found.")
  }

  rain_file <- suppressWarnings(rainfallCreation(ModelFolder, WatershedElements,
                                                 date = date, method = rainfall_method,
                                                 overwrite = T))
  return(list(ModelFolder, rain_file))
}
