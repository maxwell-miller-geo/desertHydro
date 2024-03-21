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
  terra::writeRaster(temp_rast_loaded, raster_path)
  # Remove temporary raster
  file.remove(temp_rast)
  }
}
# Test - assign coordinate system
# raster_path <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\breached.tif)"
# crsAssign(raster_path = raster_path)
## Flow accumulation function - creates smoothed dem, breached map, and flow accumulation map
# Note the projections must be the same! - Whitebox will forget the coordinate systems when it creates rasters - makes this less pretty

flow_accumlation_wb <- function(dem_file_path, Outpath, watershed_shape_path = NA, ModelFolder = NA, smooth_tif = "smoothed_dem.tif", filled_dem = "filled_dem.tif", breached_tif = "breached.tif", out_accum = "flow_accumulation.tif", max_change = 25){
  # List of created rasters
  model_dem <- file.path(Outpath, "model_dem.tif")
  cropped_dem <- file.path(Outpath, "cropped_dem.tif")
  smooth_tif <- file.path(Outpath, "smoothed_dem.tif")
  filled_tif <- file.path(Outpath, "filled_dem.tif")
  # filled_tif_planchon_and_darboux <- file.path(Outpath, "filled_dem_planchon_and_darboux.tif")
  # filled_tif_wang_and_liu <- file.path(Outpath, "filled_tif_wang_and_liu.tif")
  breached_tif <- file.path(Outpath, "breached.tif")
  flow_accum <- file.path(Outpath, "flow_accumulation.tif")
  flow_direct <- file.path(Outpath, "flow_d8.tif")

  # Store the coordinate system codes
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  # Requirements for function and optional clip step
    # Initialize whitebox
    #whitebox::wbt_init()
    if(!is.na(watershed_shape_path)){
      # Clip the dem, if a shapefile is present
      # Projection Check
      dem_code <- terra::crs(terra::rast(dem_file_path), describe = T)[[3]] # crs of dem
      shape_crs_code <-  terra::crs(terra::vect(watershed_shape_path), describe = T)[[3]] # crs of shapefile
      if(dem_code == shape_crs_code){
        # if coordinate systems match
        temp_dem <- terra::rast(dem_file_path) # load in dem
        temp_shape <- terra::vect(watershed_shape_path) # load in boundary vector
        clipped_dem <- terra::crop(temp_dem, temp_shape, ext = F, mask = T)
        terra::writeRaster(clipped_dem, filename = cropped_dem, overwrite = T) # creates cropped
        # should have a slightly different name to keep path stable
        # might need to add another line to make sure the clipped dem shows up right.
      }else{ # Breaks if coordinates do not match
        stop("The coordinates for the input files do not match.")
      }

    }else{
      cropped_dem <- dem_file_path # sets input dem to clipped version
    }


  #clipped_dem_rast <- terra::rast(clipped_dem)
  # 1. Feature Presvering Smoothings
  # Now, we want to smooth the DEM, preserving the features, leaving the maximum changes default = 0.5
  whitebox::wbt_feature_preserving_smoothing(dem = cropped_dem,
                                             output = smooth_tif,
                                              max_diff = max_change)
  # Save the coordinate system to new file
  crsAssign(smooth_tif, coordinateSystem = crs_dem)

  # 2a. Fill Depressions
  whitebox::wbt_fill_depressions(dem = cropped_dem, output = filled_tif)
  crsAssign(filled_tif, coordinateSystem = crs_dem)

  # whitebox::wbt_fill_depressions_planchon_and_darboux(dem = clipped_dem, output = filled_tif_planchon_and_darboux)
  # crsAssign(filled_tif_planchon_and_darboux, coordinateSystem = crs_dem)
  #
  # whitebox::wbt_fill_depressions_wang_and_liu(dem = clipped_dem, output = filled_tif_wang_and_liu)
  # crsAssign(filled_tif_wang_and_liu, coordinateSystem = crs_dem)

  # 2b. BreachDepressions
  whitebox::wbt_breach_depressions_least_cost(dem = smooth_tif, output = breached_tif, dist = 1, max_cost = max_change)
  ##### The curent model dem ####### KEY!
  # model_dem <- file.path(WatershedElements, "model_dem_5.tif")
  # max_change <- 5
  whitebox::wbt_breach_depressions_least_cost(dem = cropped_dem, output = model_dem, dist = max_change, max_cost = max_change)
  whitebox::wbt_d8_pointer(model_dem, flow_direct)
  crsAssign(flow_direct, coordinateSystem = crs_dem)

  # Save the coordinate system to new file
  crsAssign(breached_tif, coordinateSystem = crs_dem)
  # Save the coordinate system to new file
  crsAssign(model_dem, coordinateSystem = crs_dem)
  #plot(rast(breached_tif))# Display output

  # 3. D8 Flow accumulation
  #whitebox::wbt_d_inf_flow_accumulation(input = breached_tif, output = flow_accum)
  whitebox::wbt_d8_flow_accumulation(input = model_dem, output = flow_accum)
  # Save the coordinate system to new file
  crsAssign(flow_accum, coordinateSystem = crs_dem)

  # Additional D8 Flow accumulation calculations
  #d8_flow_accumulation(raster_path = filled_tif, coordinateSys = crs_dem, creation_method = "fill")
  # d8_flow_accumulation(raster_path = filled_tif_planchon_and_darboux, coordinateSys = crs_dem, creation_method = "fill_planch_and_darboux")
  # d8_flow_accumulation(raster_path = filled_tif_wang_and_liu, coordinateSys = crs_dem, creation_method = "fill_wang_and_liu")
  #d8_flow_accumulation(raster_path = smooth_tif, coordinateSys = crs_dem, creation_method = "smooth")

  #plot(rast(flow_accum))
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
streamCreate <- function(flowRasterPath, Outpath, threshold = 1000, DemFolder = NA, dem = "model_dem.tif"){
  # Whitebox tool to extract stream network
  extracted_streams <- file.path(Outpath, "stream_extracted.tif")
  whitebox::wbt_extract_streams(flowRasterPath, extracted_streams, threshold = threshold)
  # Flow accumulation raster
  #flowRaster <- terra::rast(file.path(WatershedElements, "flow_accumulation.tif"))
  demFile <- file.path(DemFolder, dem)
  dem_alternate <- file.path(Outpath, dem)
  streams <- file.path(Outpath, "streams.shp")
  flow_direction <- file.path(Outpath, "flow_d8.tif")
  if(!file.exists(flow_direction)){
    if(file.exists(demFile)){
      whitebox::wbt_d8_pointer(demFile, flow_direction)
      crsAssign(flow_direction, crs(terra::rast(demFile))) # flow direction

    }else if(file.exists(dem_alternate)){
      whitebox::wbt_d8_pointer(dem_alternate, flow_direction) # flow direction
      crsAssign(flow_direction, crs(terra::rast(dem_alternate))) # coords
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


