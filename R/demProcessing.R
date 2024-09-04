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

#' Hydrological pre-processing workflow for digital elevation models
#'
#' @param dem_file_path path to digital elevation file
#' @param ModelFolder Output path to save model outputs
#' @param smooth T/F: If TRUE, will smooth stream segements to remove flat areas
#' @param watershed_shape_path Optional shapefile to clip the resulting results
#' @param max_dist numeric value in meters to breach the digital elevation model
#' @param stream_threshold Flow accumulation number (cells) to create channels
#' @param carve numeric. Number of meters to carve the channel network down by.
#' @param overwrite T/F. If TRUE, the model will overwrite previous files in
#' Model Folder
#'
#' @return SpatRaster of the modified digital elevation model
#' @export
#'
#' @examples
#' ModelFolder <- tempdir() # Create a temporary directory to save files into
# dem_path <- system.file("extdata", "dem.tif", package = "desertHydro") # pass dem as file path
# hydro_workflow <- flow_accumlation_wb(dem_path, ModelFolder, )

flow_accumlation_wb <- function(dem_file_path, ModelFolder, watershed_shape_path = NA_character_, smooth = T, max_dist = 1000, stream_threshold = NULL, carve = 1, overwrite = T){
  # Need this level of precision when filling - to work with whitebox tools
  terra::terraOptions(datatype="FLT8S")
  # List of created rasters
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  print(paste0("DEM Projection: ", crs_dem))
  units <- terra::res(terra::rast(dem_file_path))[1] # units of dem
  # crs_dem <- paste0("epsg:",terra::crs(terra::rast(file.path(WatershedElements, "dem.tif")), describe = T)[[3]])
  model_dem <- file.path(ModelFolder, "model_dem.tif")
  fill_dem <- file.path(ModelFolder, "dem_fill.tif")
  flow_accum <- file.path(ModelFolder, "flow_accumulation.tif")
  extracted_streams <- file.path(ModelFolder, "stream_extracted.tif")
  vect_stream <- file.path(ModelFolder, "vect_stream.shp")
  no_flow <- file.path(ModelFolder, "no-flow.tif")

  # Remove model dem if present
  file_removal(model_dem, overwrite)
  file_removal(fill_dem, overwrite)

  if(!file.exists(model_dem)){
    #whitebox::wbt_breach_depressions_least_cost(dem = dem_file_path, output = model_dem, dist = max_change)

    # Fill depressions
    whitebox::wbt_fill_depressions(dem = dem_file_path, output = fill_dem)
    crsAssign(fill_dem, coordinateSystem = crs_dem)

    # Breach depressions
    # whitebox::wbt_breach_depressions_least_cost(dem = fill_dem, output = model_dem, dist = max_dist, flat_increment = .01)
    # crsAssign(model_dem, coordinateSystem = crs_dem)

    # Find No Flow Cells
    whitebox::wbt_find_no_flow_cells(dem = fill_dem, output = no_flow)
    crsAssign(no_flow, coordinateSystem = crs_dem)
    #plot(terra::rast(no_flow))

    # temporary flow accumulation
    whitebox::wbt_d8_flow_accumulation(input = fill_dem, output = flow_accum)
    crsAssign(flow_accum, crs_dem)

    # Determine maximum flow accum cell
    maxCell <- dfMax(terra::rast(flow_accum), "max") # the one not to fill
    # Get cell numbers
    no_flow_rast <- terra::rast(no_flow)
    # read in raster
    dem <- terra::rast(fill_dem) + 0.00000000
    names(dem) <- "model_dem" # change raster name
    new_dem <- fill_edge(dem, cellsToFill = terra::cells(no_flow_rast), dontFillCells = maxCell$cell)
    # Compare two dems
    #plot(dem-new_dem)
    # Overwrite the current dem raster
    terra::writeRaster(new_dem, model_dem, overwrite = T)
    #rm(new_dem)
    # no_flow_new <- file.path(ModelFolder, "no-flow-2.tif")
    # whitebox::wbt_find_no_flow_cells(dem = model_dem, output = no_flow_new)
    # crsAssign(no_flow_new, crs_dem)
    # plot(rast(no_flow_new))
    # temp_rast <- "temp_rast.tif"
    # # write temporary raster
    # terra::writeRaster(new_dem, temp_rast, overwrite = T)
    # # Load in temporary raster
    # temp_rast_loaded <- terra::rast(temp_rast)
    # # Remove/delete old raster
    # file.remove(model_dem)
    # # Rewrite raster with new coordinate system
    # terra::writeRaster(temp_rast_loaded, model_dem, overwrite = T)
    # # Remove temporary raster
    # file.remove(temp_rast)
    # rm(dem)
    # rm(new_dem)
  }

  file_removal(flow_accum, overwrite)

  if(!file.exists(flow_accum)){
    whitebox::wbt_d8_flow_accumulation(input = model_dem, output = flow_accum)
    crsAssign(flow_accum, coordinateSystem = crs_dem)
  }
  file_removal(extracted_streams, overwrite)

  if(!file.exists(extracted_streams)){
    if(is.null(stream_threshold)){
      dimensions <- dim(terra::rast(dem_file_path))
      cells_est <- dimensions[1] * dimensions[2]
      if(cells_est > 10000){
        stream_threshold <- 1200
      }else{
        #stream_threshold <- round(cells_est / 15)
        stream_threshold <- 25
      }
    }
    whitebox::wbt_extract_streams(flow_accum, extracted_streams, threshold = stream_threshold)
    crsAssign(extracted_streams, coordinateSystem = crs_dem)
  }

  # Calculate D8 points
  d8_pntr <- file.path(ModelFolder, "fd8_pntr.tif")
  file_removal(d8_pntr, overwrite)
  whitebox::wbt_d8_pointer(model_dem, d8_pntr)

  # RasterStreams to Vector
  file_removal(vect_stream, overwrite)
  whitebox::wbt_raster_streams_to_vector(extracted_streams, d8_pntr, vect_stream)
  crsAssign(vect_stream, coordinateSystem = crs_dem)

  # Create stream network analysis
  stream_network <- file.path(ModelFolder, "stream_network.shp")
  whitebox::wbt_vector_stream_network_analysis(vect_stream, stream_network)
  crsAssign(stream_network, coordinateSystem = crs_dem)

  # Create Long Profile hmtl files for visualization
  if(FALSE){
    profile <- file.path(ModelFolder, "profile.html")
    whitebox::wbt_long_profile(d8_pntr, extracted_streams, model_dem, profile, verbose_mode = F)
  }
  # Create vector stream network
  # whitebox::wbt_raster_to_vector_lines(extracted_streams, vect_stream)
  # crsAssign(vect_stream, coordinateSystem = crs_dem)

  # Repair stream vectors - should not run on digital created networks
  # repair_streams <- file.path(ModelFolder, "stream_repair.shp")
  # if(file.exists(repair_streams) & overwrite){
  #   print("Overwriting repaired streams")
  #   file.remove(repair_streams)
  # }
  #
  # if(!file.exists(repair_streams)){
  #   whitebox::wbt_repair_stream_vector_topology(vect_stream, repair_streams, dist = 1*units) # sqrt(2) length away
  #   crsAssign(repair_streams, coordinateSystem = crs_dem)
  # }

  # Carve dem
  if(carve > 0){
    carve_dem <- carveDem(model_dem, flow_accum, outline = watershed_shape_path, depth = carve)
    #carve_dem <- carveDem(dem, flow_accum, depth = 1)
    terra::writeRaster(carve_dem[[1]], model_dem, overwrite = T)
    flow_accum <- file.path(ModelFolder, "model_flow_accumlation.tif")
    terra::writeRaster(carve_dem[[2]], flow_accum, overwrite = T)
    # Smooth the stream
    print("Smoothing stream network...")
    smoothStream(stream_network, model_dem, ModelFolder)
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
  return(terra::rast(model_dem))
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
  demFile <- model_dem
  dem_alternate <- file.path(Outpath, model_dem) # does not work for backups currently
  streams <- file.path(Outpath, "streams.shp")
  flow_direction <- file.path(Outpath, "flow_d8.tif")

  if(!file.exists(flow_direction)){
    if(file.exists(demFile)){
      whitebox::wbt_d8_pointer(demFile, flow_direction)
      crsAssign(flow_direction, terra::crs(terra::rast(demFile))) # flow direction

    }else if(file.exists(dem_alternate)){
      print("Using DEM alternate...")
      whitebox::wbt_d8_pointer(dem_alternate, flow_direction) # flow direction
      crsAssign(flow_direction, terra::crs(terra::rast(dem_alternate))) # coords

    }else{
      errorCondition("Could not find digital elevation model in input folders.")
    }
  }
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


