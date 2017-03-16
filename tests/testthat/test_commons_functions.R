
library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {
  skip_on_cran()

  xcaq <- commons_answered_questions(answering_department = 'health', answered_by = '4019')
  expect_length(xcaq, 10)
  expect_type(xcaq, "list")
  expect_is(xcaq, "data.frame")

  xcd <- commons_divisions(division_id = 694163, summary = FALSE)
  expect_length(xcd, 5)
  expect_type(xcd, "list")
  expect_is(xcd, "data.frame")

  xcdall <- commons_divisions()
  expect_length(xcdall, 5)
  expect_type(xcdall, "list")
  expect_is(xcdall, "data.frame")

  xcoqt <- commons_oral_question_times(session='2016/17', question_id='685697')
  expect_length(xcoqt, 14)
  expect_type(xcoqt, "list")
  expect_is(xcoqt, "data.frame")

  xcoqs <- commons_oral_question_times(session='2015/16')
  expect_length(xcoqs, 14)
  expect_type(xcoqs, "list")
  expect_is(xcoqs, "data.frame")
  expect_equal(nrow(xcoqs), 313)

  xcoqe <- commons_oral_questions(mp_id=4019, answering_department = 'education')
  expect_length(xcoqe, 24)
  expect_type(xcoqe, "list")
  expect_is(xcoqe, "data.frame")

  xct <- commons_terms(search="estate")
  expect_length(xct, 20)
  expect_type(xct, "list")
  expect_is(xct, "data.frame")




})
