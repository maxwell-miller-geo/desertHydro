test_that("Fill edge test", {
  # Find holes
  a <- model()
  require(terra)
  dem_file_path <- file.path(a@watershedPath, "dem-test.tif")
  crs_dem <- paste0("epsg:",terra::crs(terra::rast(dem_file_path), describe = T)[[3]])
  # find holes
  no_flow <- file.path(tempdir(), "no-flow.tif")
  fill_dem <- file.path(tempdir(), "dem_fill.tif")
  flow_accum <- file.path(tempdir(), "flow_accumulation.tif")
  # whitebox::wbt_find_no_flow_cells(dem = dem_file_path, output = no_flow)
  # no_flow <- terra::rast(no_flow)
  # plot(no_flow)
  # Fill holes
  whitebox::wbt_fill_depressions(dem = dem_file_path, output = fill_dem)
  crsAssign(fill_dem, crs_dem)
  #plot(terra::rast(fill_dem))
  whitebox::wbt_find_no_flow_cells(dem = fill_dem, output = no_flow)
  crsAssign(no_flow, crs_dem)
  #plot(rast(no_flow))
  # Find the no flow cells
  whitebox::wbt_d8_flow_accumulation(input = fill_dem, output = flow_accum)
  crsAssign(flow_accum, crs_dem)
  #plot(rast(flow_accum))
  # Determine maximum flow accum cell
  maxCell <- dfMax(rast(flow_accum), "max") # the one not to fill
  # Get cell numbers
  nf <- rast(no_flow)

  # read in raster
  dem <- rast(fill_dem) + 0.000000
  # New dem path
  dem_path_new <- file.path(tempdir(), "new_dem.tif")
  new_dem <- fill_edge(dem, cellsToFill = cells(nf), dontFillCells = maxCell$cell)
  terra::writeRaster(new_dem, dem_path_new, overwrite = T)

  # Check no fill cells again
  # whitebox::wbt_find_no_flow_cells(dem = dem_path_new, output = no_flow)
  # crsAssign(no_flow, crs_dem)
  #plot(rast(no_flow))
  # Check that it matches up
  expect_equal(dem[maxCell$cell], new_dem[maxCell$cell])

  # Check that other cells are not equal
  expect_gt(new_dem[cells(nf)[2]][[1]], dem[cells(nf)[2]][[1]])
}
)
