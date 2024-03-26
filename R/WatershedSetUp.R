# Watershed Characteristics
# The purpose of this script is to take the following inputs and adjust the input data for a given watershed
# 1 - Digital Elevation Model (DEM)
# 2 - Land Cover Characteristics
# 3 - Watershed shapefile
# 4 - Voronoi polygons
# Some of the scripts are created in external functions

watershedElementsCreate <- function(ModelFolder, WatershedElements, DEM, WatershedShape, LandCoverCharacteristics, landCoverFile, landcovername = "landcover_soil.tif", key = "MUSYM", overwrite = T){ # DEM should be unaltered
  requireNamespace("terra")
  # DEM adjustments
  # Adjust the input DEM with the watershed shapefile.
  # Creates the following maps using the Whitebox package <- https://github.com/cran/whitebox
  model_dem <- file.path(WatershedElements, "model_dem.tif")
  print('Locating adjusted digital elevation model.')
  if(!file.exists(model_dem) | overwrite){
    print('Creating adjusted digital elevation model.')
    #library(whitebox)
    #source("demProcessing.R", local = TRUE) # Custom function with whitebox scripts
    flow_accumlation_wb(dem_file_path = DEM,
                        Outpath = WatershedElements,
                        watershed_shape_path = WatershedShape,
                        ModelFolder = ModelFolder) # Does not overwrite - creates many rasters
    print('Finished creating adjusted DEM.')
  } else{
    print("Located DEM")
  }
  # Slope creation
  slope <- file.path(WatershedElements, "model_slope.tif") # default name of slope file
  print('Locating slope file')
  if(!file.exists(slope) | overwrite){
    # Expects one of the outputs from previous function to produce "cropped_dem.tif"
    #cropped_dem <- file.path(WatershedElements, "cropped_dem.tif")
    slope_temp <- terra::terrain(terra::rast(model_dem), v = "slope", neighbors = 8, unit = "degrees")
    edges <- terra::focal(slope_temp, w = 3, "modal", na.policy = "only", na.rm = F)
    names(edges) <- "slope"
    # NOTE - slope does not compute for boundary cells
    terra::writeRaster(edges, filename = file.path(WatershedElements, "model_slope.tif"), overwrite = T)
  }
  # Land Cover
  # Project the land cover data into same coordinate system of the DEM
  # Check if the landcover raster is already there
  model_landcover <- file.path(WatershedElements, "model_landcover.tif")
  fileExists <- file.exists(model_landcover)
  print('Locating adjusted land cover map.')
  if(!fileExists | overwrite){
      #source("createWatershedModel.R", local = TRUE) # Land Cover script
      print("Processing land cover...")
      #landCoverFile <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # school desktop
      extension <- sub(".*\\.", "", landCoverFile) # use regular expression to get file extension
      if(extension == "shp"){
        land_cover <- terra::vect(landCoverFile)
      }else{
        land_cover <- terra::rast(landCoverFile)
      }
      dem_local <- terra::rast(model_dem)
      land_cover_raster <- resizeShape(spatialObject = land_cover,
                                       extent_raster = dem_local,
                                       watershedboundary = WatershedShape,
                                       key = key,
                                       save = F)
      terra::writeRaster(land_cover_raster, model_landcover, overwrite = TRUE)
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
    #source("setup_FlowPartition.R", local = TRUE) # functions necessary
    #flowStack <- flow_Partition(clipped_adj_dem = model_dem, file_name_and_path = flow_file)
    flowStack <- flowMap(dem = model_dem, outFolder = ModelFolder)
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

  # Stack them all up
  WatershedStack <- c(terra::rast(model_dem),
                      terra::rast(slope),
                      terra::rast(flow_file),
                      terra::rast(model_landcover))

  terra::writeRaster(WatershedStack, file.path(WatershedElements, "watershed_stack.tif"), overwrite = overwrite)
  print(paste0("Finished creating/checking files. Watershed elements files located in: ", WatershedElements))
  # Check files were written into folder
  #return(WatershedStack)

  ClassificationMap <- model_landcover # adjusted/cropped classification map - must be named correctly

  initial_soil_conditions(LandCoverCharacteristics = LandCoverCharacteristics,
                          ClassificationMap = ClassificationMap,
                          WatershedStack = WatershedStack,
                          outline = watershed_shape_path,
                          ModelOutputs = ModelFolder,
                          key = key,
                          overwrite = overwrite
                          )
  SoilStack <- terra::rast(file.path(ModelFolder, "model_soil_stack.tif"))
}

# Test
# fileWatershedElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)"
# dem_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)"
# land_cover_path <-  r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\LandCoverData\nlcd_2021_land_cover_l48_20230630.img)" # school desktop
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)"
#
# watershedElements(WatershedElements = fileWatershedElements, DEM = dem_path, LandCover = land_cover_path, WatershedShape = watershed_shape_path)

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
