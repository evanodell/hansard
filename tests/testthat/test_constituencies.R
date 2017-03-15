
context("constituencies")

test_that("constituencies return expected format", {
  skip_on_cran()

  ctx <- constituencies()

  expect_length(ctx, 9)
  expect_is(ctx, "data.frame")

})
