## Whitebox - Flow accumulation piece from the DEM
### Install whitebox tools
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
#whitebox::install_whitebox()
# load in whitebox
# library(whitebox)
# library(terra)

full_workflow_wb <- function(dem_file_path, outputLocation, out_dem = "./adjusted_dem.tif", out_pntr = "./flow_direction.tif", out_accum = "./flow_accum.tif"){
  setwd(outputLocation)
  #clipped_dem <- "./clipped_dem.tif" # name to save clip

  # create the flow accumulation
  whitebox::wbt_flow_accumulation_full_workflow(dem = dem_file_path,
                                                out_dem = out_dem,
                                                out_pntr = out_pntr,
                                                out_accum = out_accum,
                                                out_type = "cells",
                                                esri_pntr = TRUE)
}

## Test for full workflow
# full_workflow_wb(dem_file_path = dem, outputLocation = DataStorage, out_dem = "./adjusted_dem.tif", out_pntr = "./flow_direction.tif", out_accum = "./flow_accum.tif")
crsAssign <- function(raster_path, coordinateSystem = "epsg:4269"){
  #library(terra)
  if(grepl(".shp", raster_path)){
    temp_vect <- terra::vect(raster_path)
    terra::set.crs(temp_vect, coordinateSystem)
    terra::writeVector(temp_vect, raster_path, overwrite = T)
  }else{
  # load in original raster
  raster <- terra::rast(raster_path)
  # set coordinate system
  terra::set.crs(raster, coordinateSystem)
  # Temporary raster name/filepath
  temp_rast <- "temp_rast.tif"
  # write temporary raster
  terra::writeRaster(raster, temp_rast, overwrite = T)
  # Load in temporary raster
  temp_rast_loaded <- terra::rast(temp_rast)
  # Remove/delete old raster
  file.remove(raster_path)
  # Rewrite raster with new coordinate system
  terra::writeRaster(temp_rast_loaded, raster_path, overwrite = T)
  # Remove temporary raster
  file.remove(temp_rast)
  }
}
# Test - assign coordinate system
# raster_path <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\breached.tif)"
# crsAssign(raster_path = raster_path)
## Flow accumulation function - creates smoothed dem, breached map, and flow accumulation map
# Note the projections must be the same! - Whitebox will forget the coordinate systems when it creates rasters - makes this less pretty

flow_accumlation_wb <- function(dem_file_path, Outpath, watershed_shape_path = NA, ModelFolder = NA, smooth_tif = "smoothed_dem.tif", filled_dem = "filled_dem.tif", breached_tif = "breached.tif", out_accum = "flow_accumulation.tif", max_change = 25, carve = T, overwrite = T){
  gc()
  # List of created rasters
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(file.path(WatershedElements, "dem.tif")), describe = T)[[3]])
  model_dem <- file.path(ModelFolder, "model_dem.tif")
  flow_accum <- file.path(ModelFolder, "flow_accumulation.tif")
  extracted_streams <- file.path(ModelFolder, "stream_extracted.tif")
  # Remove model dem if present
  if(file.exists(model_dem) & overwrite){
    print("Overwriting model dem")
    file.remove(model_dem)
  }
  if(!file.exists(model_dem)){
    whitebox::wbt_breach_depressions_least_cost(dem = dem_file_path, output = model_dem, dist = max_change)
    whitebox::wbt_breach_depressions_least_cost(dem = file.path(WatershedElements, "dem.tif"), output = model_dem, dist = 1000, fill = TRUE)
    crsAssign(model_dem, coordinateSystem = crs_dem)
  }

  # Remove flow accumulation if exists
  if(file.exists(flow_accum) & overwrite){
    print("Overwriting flow accumulation")
    file.remove(flow_accum)
  }
  if(!file.exists(flow_accum)){
    whitebox::wbt_d8_flow_accumulation(input = model_dem, output = flow_accum)
    crsAssign(flow_accum, coordinateSystem = crs_dem)
  }


  # Extract streams
  if(file.exists(extracted_streams) & overwrite){
    print("Overwriting extracted streams")
    file.remove(extracted_streams)
  }
  if(!file.exists(extracted_streams)){
    whitebox::wbt_extract_streams(flow_accum, extracted_streams, threshold = 1000)
    crsAssign(extracted_streams, coordinateSystem = crs_dem)
  }

  # Carve dem
  if(carve){
    carve_dem <- carveDem(model_dem, flow_accum, outline = watershed_shape_path, depth = 1)
    #carve_dem <- carveDem(dem, flow_accum, depth = 1)
    terra::writeRaster(carve_dem[[1]], model_dem, overwrite = T)
    flow_accum <- file.path(ModelFolder, "model_flow_accumlation.tif")
    terra::writeRaster(carve_dem[[2]], flow_accum, overwrite = T)
  }
  #carve_dem <- terra::rast(model_dem) + 0
  # Determine outflow points of model to prevent back filling
  drain <- firstSecond(carve_dem[[2]], carve_dem[[1]], Outfolder = ModelFolder, name = "drainCells")

  xy <- data.frame(x = drain$x, y = drain$y)
  v <- terra::vect(xy, geom = c("x","y"), crs = crs_dem)
  terra::writeVector(v, file.path(ModelFolder, "drainPoints.shp"), overwrite = T)
  # Clip files if necessary
  if(!is.na(watershed_shape_path)){
    # Clip the dem, if a shapefile is present
    # Projection Check
    dem_code <- terra::crs(terra::rast(model_dem), describe = T)[[3]] # crs of dem
    shape_crs_code <-  terra::crs(terra::vect(watershed_shape_path), describe = T)[[3]] # crs of shapefile
    if(dem_code == shape_crs_code){
      # if coordinate systems match
      temp_dem <- terra::rast(model_dem) + 0 # load in dem
      temp_streams <- terra::rast(extracted_streams) + 0
      temp_shape <- terra::vect(watershed_shape_path) # load in boundary vector

      clipped_model_dem <- terra::crop(temp_dem, temp_shape, ext = F, mask = T)
      clipped_streams <- terra::crop(temp_streams, temp_shape, ext = F, mask = T)

      terra::writeRaster(clipped_model_dem, filename = model_dem, overwrite = T) # creates cropped
      terra::writeRaster(clipped_streams, filename = extracted_streams, overwrite = T) # creates cropped
      # should have a slightly different name to keep path stable
      # might need to add another line to make sure the clipped dem shows up right.
    }else{ # Breaks if coordinates do not match
      stop("The coordinates for the input files do not match.")
    }
  }
}

d8_flow_accumulation <- function(raster_path, coordinateSys, creation_method = "fill"){
  out_name <- file.path(paste0(tools::file_path_sans_ext(raster_path), "_", creation_method, "_flow.tif"))
  whitebox::wbt_d8_flow_accumulation(raster_path, out_name)
  crsAssign(raster_path = out_name, coordinateSystem = coordinateSys)
}

# # Test full flow accumulation function
# dem <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_extent.tif)"
#ModelElements <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements)"
# watershed_shape_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Spatial_Data\QGIS\waterholes_shape.shp)"
# flow_accumlation_wb(dem_file_path = dem, Outpath = ModelElements, watershed_shape_path = watershed_shape_path)

## --------------------- Stream network creation
# Create a stream network from flow accumulation raster
streamCreate <- function(flowRasterPath, model_dem, Outpath, threshold = 100){
  # Whitebox tool to extract stream network
  extracted_streams <- file.path(Outpath, "stream_extracted.tif")
  whitebox::wbt_extract_streams(flowRasterPath, extracted_streams, threshold = threshold)
  crsAssign(extracted_streams, coordinateSystem = terra::crs(terra::rast(flowRasterPath)))
  # Flow accumulation raster
  #flowRaster <- terra::rast(file.path(WatershedElements, "flow_accumulation.tif"))
  demFile <- dem
  dem_alternate <- file.path(Outpath, dem)
  streams <- file.path(Outpath, "streams.shp")
  flow_direction <- file.path(Outpath, "flow_d8.tif")

  # if(!file.exists(flow_direction)){
  #   if(file.exists(demFile)){
  #     whitebox::wbt_d8_pointer(demFile, flow_direction)
  #     crsAssign(flow_direction, terra::crs(terra::rast(demFile))) # flow direction
  #
  #   }else if(file.exists(dem_alternate)){
  #     whitebox::wbt_d8_pointer(dem_alternate, flow_direction) # flow direction
  #     crsAssign(flow_direction, terra::crs(terra::rast(dem_alternate))) # coords
  #   }else{
  #     errorCondition("Could not find digital elevation model in input folders.")
  #   }
  # }
  coords <- terra::crs(terra::rast(flow_direction)) # coordinates of flow direction
  whitebox::wbt_raster_streams_to_vector(extracted_streams, flow_direction, streams)
  crsAssign(streams, coordinateSystem = coords)

  print(paste("Saved stream network to: ", streams))
  #file.remove(extracted_streams)
  print("Streams have been created")
  return(terra::vect(streams))
}
# Test function
# streams_test <- streamCreate(
#   flowRasterPath = file.path(WatershedElements, "flow_accumulation.tif"),
#   Outpath = WatershedElements,
#   DemFolder = ModelFolder
# )
## ---------------------------- Stream network analysis
# wbt_vector_stream_network_analysis(
#   streams = streams,
#   dem = file.path(DemFolder, "model_dem.tif"),
#   output = file.path(WatershedElements, "stream_analysis.shp")
# )
# # View Changes
# Outpath = ModelElements
# clipped_dem <- file.path(Outpath, "clipped_dem.tif")
# smooth_tif <- file.path(Outpath, "smoothed_dem.tif")
# breached_tif <- file.path(Outpath, "breached.tif")
# flow_accum <- file.path(Outpath, "flow_accumulation.tif")
#
# flow_accum <- rast(flow_accum)
# difference_breach <- rast(clipped_dem) - rast(breached_tif)
# difference_smooth <- rast(clipped_dem) - rast(smooth_tif)
# plot(rast(flow_accum))
# flow_accumlation_wb(dem, ModelElements, watershed_shape_path = watershed_shapefile)

# Function to turn raster data into a linked method of transporting water

# # First load in the raster from examples
# dem <- terra::rast(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\breached_clipped_dem.tif)")
#
# plot(dem)
#
# # Get the values of the nodes
# nodes_dem  <- tibble(value = terra::values(dem)) |>
#   bind_cols(as_tibble(xyFromCell(dem)))


