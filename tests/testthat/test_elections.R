library(hansard)
context("elections")


test_that("election functions return expected format", {
  skip_on_cran()

  elect <- elections()
  expect_length(elect, 5)
  expect_type(elect, "list")
  expect_is(elect, "data.frame")

  electid <- elections(517994)
  expect_length(electid, 6)
  expect_type(electid, "list")
  expect_is(electid, "data.frame")
  expect_equal(nrow(electid),1)

  elre <- election_results()
  expect_length(elre, 9)
  expect_type(elre, "list")
  expect_is(elre, "data.frame")


  elre2010 <- election_results(382037)
  expect_length(elre2010, 9)
  expect_type(elre2010, "list")
  expect_is(elre2010, "data.frame")
  expect_equal(nrow(elre2010), 650)

})

