library(hansard)
context("lords functions")


test_that("lords functions return expected format", {
  skip_on_cran()

  # Voting Records

  lvotesall <- lord_vote_record(530, lobby='all', start_date="2017-01-01")
  expect_length(lvotesall, 11)
  expect_type(lvotesall, "list")
  expect_is(lvotesall, "data.frame")

  lvotesnot <- lord_vote_record(530, lobby='notcontent', start_date="2017-01-01")
  expect_length(lvotesnot, 10)
  expect_type(lvotesnot, "list")
  expect_is(lvotesnot, "data.frame")

  lvotescont <- lord_vote_record(530, lobby='content', start_date="2017-01-01")
  expect_length(lvotescont, 10)
  expect_type(lvotescont, "list")
  expect_is(lvotescont, "data.frame")

  # Amendments
  lamend <- lords_amendments(decision = "Agreed", start_date="2017-03-17")
  expect_length(lamend, 31)
  expect_type(lamend, "list")
  expect_is(lamend, "data.frame")

  # Divisions
  ldivsum <- lords_divisions(division_id = 705891, summary = TRUE)
  expect_length(ldivsum, 11)
  expect_type(ldivsum, "list")
  expect_is(ldivsum, "data.frame")

  ldiv <- lords_divisions(division_id = 705891, summary = FALSE)
  expect_length(ldiv, 6)
  expect_type(ldiv, "list")
  expect_is(ldiv, "data.frame")

  ldivdec <- lords_divisions(NULL, FALSE, start_date = '2016-12-01', end_date = '2016-12-31')
  expect_length(ldivdec, 5)
  expect_type(ldivdec, "list")
  expect_is(ldivdec, "data.frame")

  # Written Questions
  lwq <- lords_written_questions(peer_id = 3526, answering_department = 'cabinet')
  expect_length(lwq, 12)
  expect_type(lwq, "list")
  expect_is(lwq, "data.frame")

})

