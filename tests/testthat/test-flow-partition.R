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
