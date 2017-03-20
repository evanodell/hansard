
library(hansard)
context("commons functions")

test_that("commons fucntions return expected format", {
  skip_on_cran()

  xcaq <- commons_answered_questions(answering_department = 'health', answered_by = '4019', start_date='2017-02-01')
  expect_length(xcaq, 10)
  expect_type(xcaq, "list")
  expect_is(xcaq, "data.frame")

  xcd <- commons_divisions(division_id = 694163, summary = FALSE)
  expect_length(xcd, 5)
  expect_type(xcd, "list")
  expect_is(xcd, "data.frame")

  xcdall <- commons_divisions(start_date='2017-02-01', end_date='2017-03-01')
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

  xcwq <- commons_written_questions(mp_id=410, 'cabinet office')
  expect_length(xcwq, 12)
  expect_type(xcwq, "list")
  expect_is(xcwq, "data.frame")

  xcte <- commons_terms(search="estate")
  expect_length(xcte, 20)
  expect_type(xcte, "list")
  expect_is(xcte, "data.frame")

  xctec <- commons_terms(search="estate", class="ORG")
  expect_length(xctec, 19)
  expect_type(xctec, "list")
  expect_is(xctec, "data.frame")

})
