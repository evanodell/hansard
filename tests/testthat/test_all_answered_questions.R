library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {

    skip_on_cran()

  aaqx <- all_answered_questions(4019)

  expect_length(aaqx, 17)
  expect_type(aaqx, "list")
  expect_is(aaqx, "data.frame")

})
