library(hansard)
context("bills")

test_that("bills return expected format", {
  skip_on_cran()

  xb <- bills(start_date="2017-01-01")
  expect_length(xb, 10)
  expect_is(xb, "data.frame")

  xba <- bills(amendments=TRUE, start_date = "2016-12-21")
  expect_length(xba, 14)
  expect_is(xba, "data.frame")

  xbid <- bills(1719)
  expect_length(xbid, 10)
  expect_is(xbid, "data.frame")

})
