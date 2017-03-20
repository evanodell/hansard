library(hansard)
context("lords functions")


test_that("lords functions return expected format", {
  skip_on_cran()

  # Voting Records

  lvotesall <- lord_vote_record(530, lobby='all')
  expect_length(lvotesall, 11)
  expect_type(lvotesall, "list")
  expect_is(lvotesall, "data.frame")

  lvotesnot <- lord_vote_record(530, lobby='notcontent')
  expect_length(lvotesnot, 10)
  expect_type(lvotesnot, "list")
  expect_is(lvotesnot, "data.frame")

  lvotescont <- lord_vote_record(530, lobby='content')
  expect_length(lvotescont, 10)
  expect_type(lvotescont, "list")
  expect_is(lvotescont, "data.frame")

})

