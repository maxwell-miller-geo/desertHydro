test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Simple flow maps are equivalent", {
  t1 <- c(1,1,1,1,2,1,1,1,1)
  flow <- terra::rast(matrix(rep(1,9), nrow = 3, ncol = 3))
  t1rast <- terra::rast(matrix(t1, nrow = 3, ncol = 3))
  flowMap1 <- flowMap(t1rast)
  flowNew <- flowRouting(flow, flowMap1)
  flowAdjusted <- flow + flowNew
  expect_equal(round(flowOutSum(t1),5), round(6.828427,5))
  expect_equal(sum(terra::values(flowMap(t1rast)), na.rm = T), 1)
  expect_equal(sum(terra::values(flowRouting(flow, flowMap1))), 9)
  expect_equal(sum(terra::values(flowAdjusted)), 18)
  expect_equal(sum(terra::values(flowNew)), sum(terra::values(flow)))

})

test_that("Flow routing more complex", {
  t2 <- c(1,1,1,2,3,2,1,1,1)
  flow <- terra::rast(matrix(rep(1,9), nrow = 3, ncol = 3))
  trast <- terra::rast(matrix(t2, nrow = 3, ncol = 3))
  flowMap <- flowMap(trast)
  flowNew <- flowRouting(flow, flowMap)
  expect_equal(sum(terra::values(flowMap), na.rm = T), 3)
  expect_equal(sum(terra::values(flowRouting(flow, flowMap))), 9)
  expect_equal(sum(terra::values(flowNew)), sum(terra::values(flow)))

  # Half flow routing
  flowHalf <- flow * .5
  flowRemaining <- flow - flowHalf
  flowNew <- flowRouting(flowHalf, flowMap)
  expect_equal(sum(terra::values(flowNew)), 4.5)
  flowFinal <- flowRemaining + flowNew
  expect_equal(sum(terra::values(flowFinal)),9)
  expect_equal(sum(terra::values(flowHalf - flowNew)), 0)
  #plot(flowFinal)
# <<<<<<< Updated upstream
#   # WS <- terra::rast("./inst/extdata/DemoElements/watershed_stack.tif")
#   # SS <- terra::rast("./inst/extdata/DemoElements/model_soil_stack.tif")
#   # FR <- flowMap(SS$model_dem)
# =======
#   WS <- terra::rast("./inst/extdata/DemoElements/watershed_stack.tif")
#   SS <- terra::rast("./inst/extdata/DemoElements/model_soil_stack.tif")
#   FR <- flowMap(SS$model_dem)
# >>>>>>> Stashed changes
#   # #FR <- terra::rast("./inst/extdata/DemoElements/stack_flow.tif")
  # time_step <- 15
  # length <- 10
  # cm_flow <- WS[[1]]/ WS[[1]]
  # velocity <- manningsVelocity(SS$mannings_n, cm_flow, SS$slope, length = length)
  # DC <- depthChange(velocity, cm_flow, time_step = time_step, flowDirectionMap = FR,length = length)
  # routeWater(SS, flowDirectionMap = FR, time_step = time_step, length = length)

})

test_that("Flow out sum", {
# <<<<<<< Updated upstream
#   # SS <- terra::rast("./inst/extdata/DemoElements/model_soil_stack.tif")
#   # dem <- SS$model_dem
#   #
#   # flowOutSum()
# =======
#   SS <- terra::rast("./inst/extdata/DemoElements/model_soil_stack.tif")
#   dem <- SS$model_dem
#
#   flowOutSum()
# >>>>>>> Stashed changes
})

