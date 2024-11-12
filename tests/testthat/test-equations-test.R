test_that("Manning's Velocity", {
  n <- .5
  initial_depth_cm <- 1
  slope <- 45 # degrees
  length <- 10 # grid length
  mv <- manningsVelocity(n, initial_depth_cm, slope, length = length)
  expect_lt(mv, 10)
})

