test_that("Check flow roughness adjustment", {
  l <- c(1:25)
  m <- matrix(l, 5,5)
  r <- terra::rast(m)
  n <- r/100
  d <- c(1:10, 100:110, 1:4) / 100
  d_m <- matrix(d, 5, 5)
  depth <- terra::rast(d_m)
  n_new <- roughnessAdjust(depth = depth, roughness = n)
  combine <- c(n, n_new, depth)
  terra::values(combine)

})
test_that("Land Cover reads in: ", {
  a <- model() # generic model
  watershedPath <- a@watershedPath
  ClassMap <- terra::rast(file.path(watershedPath, "waterholes_LC.tif"))
  soilTable <- readxl::read_xlsx(file.path(watershedPath, "nlcd_characteristics.xlsx"))
  key <- "ID"
  lc_adjusted <- createSoilRasters(ClassMap, soilTable, key = key)
  expect_gt(terra::nlyr(lc_adjusted), 1)
})

test_that("Create node network", {
  require(terra)
  require(whitebox)


  mat <- matrix(c(1,1,1,1,0,1,1,1,1), nrow = 3)
  r <- terra::rast(mat, crs = "EPSG:4269")
  temp_file <- file.path(tempdir(), "temp-rast.tif")
  terra::writeRaster(r, temp_file, overwrite = T)
  small_test <- node_raster(temp_file)
  # Synthetic data
  expect_equal(all(values(small_test$lyr.1) == 5), TRUE)
  expect_equal(sum(values(small_test$lyr.2)), 9.6568542)

  # Another small test
  mat <- matrix(1:25, nrow = 5)
  mat[1,2] <- NaN
  mat[5,1] <- NaN
  r <- terra::rast(mat, crs = "EPSG:4269")
  temp_file <- file.path(tempdir(), "temp-rast.tif")
  terra::writeRaster(r, temp_file, overwrite = T)
  small_test2 <- node_raster(temp_file)

  a <- model()
  dem_path <- file.path(a@watershedPath, "dem-test.tif")
  dem <- terra::rast(dem_path)
  file.exists(dem_path)
  med_test <- node_raster(dem_path)

  dem_values <- length(values(dem, na.rm = T))
  node_values <- length(values(med_test$lyr.1, na.rm = T))
  distance_values <- length(values(med_test$lyr.2, na.rm = T))
  expect_equal(dem_values, node_values)
  expect_equal(dem_values, distance_values)

})

