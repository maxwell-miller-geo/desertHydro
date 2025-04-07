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
crsAssign <- function(raster_path, coordinateSystem = "epsg:26912"){
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
  temp_rast <- tempfile("raster",fileext = ".tif")
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

flow_accumlation_wb <- function(dem_file_path, ModelFolder, watershed_shape_path = NA_character_, smooth = T, max_dist = 11, stream_threshold = NULL, carve = 0, overwrite = T){
  # Need this level of precision when filling - to work with whitebox tools
  terra::terraOptions(datatype="FLT8S")
  # List of created rasters
  crs_dem <- get_utm_epsg(dem_file_path)
  crs_dem_og <- paste0("EPSG:",terra::crs(terra::rast(dem_file_path), describe=T)[[3]])
  if(crs_dem != crs_dem_og){
    dem_proj <- terra::project(terra::rast(dem_file_path), crs_dem)
    dem_file_path <- file.path(ModelFolder, "dem.tif")
    terra::writeRaster(dem_proj, filename = dem_file_path, overwrite = T)
    #units <- terra::res(dem_proj)[1] # units of dem
  }

  get_crs(dem_file_path)
  #crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  print(paste0("DEM Projection: ", crs_dem))
  #units <- terra::res(terra::rast(dem_file_path))[1] # units of dem
  # crs_dem <- paste0("epsg:",terra::crs(terra::rast(file.path(WatershedElements, "dem.tif")), describe = T)[[3]])
  model_dem <- file.path(ModelFolder, "model_dem.tif")
  flow_accum <- file.path(ModelFolder, "flow_accumulation.tif")
  extracted_streams <- file.path(ModelFolder, "stream_extracted.tif")
  vect_stream <- file.path(ModelFolder, "vect_stream.shp")

  # Remove model dem if present
  file_removal(model_dem, overwrite)
  # Breach and fill necessary holes
  if(!file.exists(model_dem)){
    dem_adjustment(dem_file_path, model_dem, max_dist = max_dist)
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
  whitebox::wbt_d8_pointer(model_dem, d8_pntr, esri_pntr = TRUE)

  # RasterStreams to Vector
  file_removal(vect_stream, overwrite)


  # Create stream network analysis - Optional
  # stream_network <- file.path(ModelFolder, "stream_network.shp")
  # if(carve > 0){
  #   tryCatch({
  #     whitebox::wbt_raster_streams_to_vector(extracted_streams, d8_pntr, vect_stream)
  #     crsAssign(vect_stream, coordinateSystem = crs_dem)
  #
  #     whitebox::wbt_vector_stream_network_analysis(vect_stream, stream_network)
  #     crsAssign(stream_network, coordinateSystem = crs_dem)
  #   }, error = function(e) {
  #     print("An error occurred in whitebox tools:", e$message)
  #   })
  # }


  # Create Long Profile hmtl files for visualization
  if(FALSE){
    profile <- file.path(ModelFolder, "profile.html")
    whitebox::wbt_long_profile(d8_pntr, extracted_streams, model_dem, profile, verbose_mode = F)
  }
  # Carve dem
  # if(carve > 0 & file.exists(stream_network)){
  #   carve_dem <- carveDem(model_dem, flow_accum, outline = watershed_shape_path, depth = carve)
  #   #carve_dem <- carveDem(dem, flow_accum, depth = 1)
  #   terra::writeRaster(carve_dem[[1]], model_dem, overwrite = T)
  #   flow_accum <- file.path(ModelFolder, "model_flow_accumlation.tif")
  #   terra::writeRaster(carve_dem[[2]], flow_accum, overwrite = T)
  #   # Smooth the stream
  #   print("Smoothing stream network...")
  #   smoothStream(stream_network, model_dem, ModelFolder)
  # }
  #carve_dem <- terra::rast(model_dem) + 0
  # Determine outflow points of model to prevent back filling

  print("Finding outflow cell")
  drain <- outFlowCells(model_dem, flow_accum, Outfolder = ModelFolder, name = "drainCells")
  xy <- data.frame(x = drain$x, y = drain$y)
  v <- terra::vect(xy, geom = c("x","y"), crs = crs_dem)
  terra::writeVector(v, file.path(ModelFolder, "drainPoints.shp"), overwrite = T)

  # Clip files if necessary
  if(!is.na(watershed_shape_path)){
    # Clip the dem, if a shapefile is present
    # Projection Check
    #dem_code <- terra::crs(terra::rast(model_dem), describe = T)[[3]] # crs of dem
    dem_code <- get_crs(model_dem)
    #shape_crs_code <-  terra::crs(terra::vect(watershed_shape_path), describe = T)[[3]] # crs of shapefile
    shape_crs_code <- get_crs(watershed_shape_path, raster = F)
    # Adjust coordinates
    if(dem_code != shape_crs_code){
      # Project the shapefile containing the watershed boundary
      watershed_proj <- terra::project(terra::vect(watershed_shape_path), dem_code)
      #watershed_shape_path <- file.path(ModelFolder, "watershed.shp")
      terra::writeVector(watershed_proj, filename = watershed_shape_path, overwrite = T)
      shape_crs_code <- get_crs(watershed_shape_path, raster = F)
    }
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
      whitebox::wbt_d8_pointer(demFile, flow_direction, esri_pntr = T)
      crsAssign(flow_direction, terra::crs(terra::rast(demFile))) # flow direction

    }else if(file.exists(dem_alternate)){
      print("Using DEM alternate...")
      whitebox::wbt_d8_pointer(dem_alternate, flow_direction, esri_pntr = T) # flow direction
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
# Check for holes
holes_check <- function(dem, temp_no_flow = file.path(tempdir(), "temp-file.tif")){
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem), describe = T)[[3]])
  # Find No Flow Cells
  whitebox::wbt_find_no_flow_cells(dem = dem, output = temp_no_flow)
  crsAssign(temp_no_flow, crs_dem)
  terra::plot(terra::rast(temp_no_flow))
  holes <- terra::cells(terra::rast(temp_no_flow))
  # Remove temp file
  rm(temp_no_flow)
  if(length(holes) > 1){
    return(T)
  }
  return(F)
}
# plot_holes <- function(no_flow_rast){
#   plot(terra::rast(no_flow_rast))
# }
# Water with one outlet
dem_adjustment <- function(dem_file_path, model_dem, max_dist = 11){
  fill_dem <- file.path(tempdir(), "temp-fill.tif")
  no_flow <- file.path(tempdir(), "no-fill.tif")
  flow_accum <- file.path(tempdir(), "flow-accum.tif")
  # Need this level of precision when filling - to work with whitebox tools
  terra::terraOptions(datatype="FLT8S")
  # List of created rasters
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  print(paste0("DEM Projection: ", crs_dem))
  ## First check original DEM
  holes_present <- holes_check(dem_file_path)
  # Breach depressions
  whitebox::wbt_breach_depressions_least_cost(dem = dem_file_path, output = model_dem, dist = max_dist)
  crsAssign(model_dem, coordinateSystem = crs_dem)
  holes_present <- holes_check(model_dem)
  counter <- 1
  while(holes_present){
    # Fill depressions
    whitebox::wbt_fill_depressions(dem = model_dem, output = fill_dem)
    crsAssign(fill_dem, coordinateSystem = crs_dem)

    # temporary flow accumulation
    whitebox::wbt_d8_flow_accumulation(input = fill_dem, output = flow_accum)
    crsAssign(flow_accum, crs_dem)

    # Find No Flow Cells
    whitebox::wbt_find_no_flow_cells(dem = fill_dem, output = no_flow)
    crsAssign(no_flow, coordinateSystem = crs_dem)
    # Get cell numbers
    no_flow_rast <- terra::rast(no_flow)

    # Determine maximum flow accum cell
    maxCell <- dfMax(terra::rast(flow_accum), "max") # the one not to fill

    # read in raster
    dem <- terra::rast(fill_dem) + 0.00000000
    names(dem) <- "model_dem" # change raster name
    new_dem <- fill_edge(dem, cellsToFill = terra::cells(no_flow_rast), dontFillCells = maxCell$cell)
    # Overwrite the current dem raster
    terra::writeRaster(new_dem, model_dem, overwrite = T)
    holes_present <- holes_check(model_dem)
    counter <- counter + 1
    if(counter == 5){
      break
    }
  }
  rm(fill_dem)
  rm(no_flow)
  rm(flow_accum)
  return(model_dem)
}
