test_that("String matching", {
  z <- list("dev", "TOTAL Cumulative Rain (in)")
  string_match <- "TOTAL"
  expect_equal(sum(grepl(string_match, z)), 1)
})
