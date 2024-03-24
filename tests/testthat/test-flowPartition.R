test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Flow maps are equivalent", {
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

