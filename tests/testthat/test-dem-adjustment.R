test_that("adjust-dem", {
  model1 <- model()
  dem <- file.path(model1@watershedPath, "dem-test.tif")
  file.exists(dem)
  model_dem <- file.path(tempdir(), "temp-dem.tif")
  dem_adjustment(dem, model_dem) # adjusts model_dem
  expect_equal(holes_check(model_dem), F)
  rm(model_dem)
})
