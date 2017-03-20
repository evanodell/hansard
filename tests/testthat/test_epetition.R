library(hansard)
context("epetitions")


test_that("epetitions functions return expected format", {
  skip_on_cran()


  epetit<- epetition(ID = 706964, by_constituency=TRUE)
  expect_length(epetit, 7)
  expect_type(epetit, "list")
  expect_is(epetit, "data.frame")

})

