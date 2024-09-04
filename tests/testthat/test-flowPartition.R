test_that("Flow routing works", {
  # model_path <- dirname(system.file("extdata", "dem.tif", package = "desertHydro"))
  # dem <- terra::rast(file.path(model_path, "dem.tif"))
  # flow <- flowMap(dem)
  # water <- flow[[1]]
  # flowRouting(water,flow)
})

test_that("Flow partition", {
  # model_path <- dirname(system.file("extdata", "dem.tif", package = "desertHydro"))
  # dem <- terra::rast(file.path(model_path, "dem.tif"))
  # profvis(flowMap(dem))
})

test_that("Calculation", {
  # x <- c(1,2,3,4,5,6,7,8,9)
  # y <- c(1,1,1,1,2,1,1,1,1)
  # z <- c(5,6, NaN, 8, 9, NaN, NaN, NaN, NaN)
  # flowOutSum(x)
  # flow_window(x)
  # flowOutSum(y)
  # flow_window(y)
  # flowOutSum(z)
  # flow_window(z)
})

test_that("C++ function works", {
  # nc <- nr <- 10
  # r <- terra::rast(ncols=nc, nrows=nr, ext= c(0, nc, 0, nr))
  # terra::values(r) <- 1:terra::ncell(r)
  # t <- terra::focalCpp(r, w = 3, fun = flow_sum, fillvalue = NaN, silent = F)
  # kernel <- array(1, dim = c(3,3,1)) # Create '3D' matrix 3x3x1 with only 1's
  # #kernel <-
  # dem_flow <- terra::focal3D(r, w = kernel, fun = flowOutSum, pad = TRUE)
  # expect_equal(sum(terra::values(dem_flow) - terra::values(t)), 0)

})
