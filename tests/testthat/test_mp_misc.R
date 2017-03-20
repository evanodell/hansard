library(hansard)
context("misc MP functions")

test_that("members vote record and questions functions return expected format", {
  skip_on_cran()

  mpqs <- mp_questions(172, 'all', start_date="2015-02-01")

  expect_length(mpqs, 11)
  expect_type(mpqs, "list")
  expect_is(mpqs, "data.frame")

  mpoqs <- mp_questions(172, 'oral')

  expect_length(mpoqs, 24)
  expect_type(mpoqs, "list")
  expect_is(mpoqs, "data.frame")

  mpvrall <- mp_vote_record(172, lobby='all', start_date="2017-01-01")

  expect_length(mpvrall, 6)
  expect_type(mpvrall, "list")
  expect_is(mpvrall, "data.frame")

  mpvraye <- mp_vote_record(172, lobby='aye', start_date="2017-01-01")

  expect_length(mpvraye, 5)
  expect_type(mpvraye, "list")
  expect_is(mpvraye, "data.frame")

})