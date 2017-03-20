library(hansard)
context("research_briefings")


test_that("research_briefings return expected format", {
  skip_on_cran()

  rtl <- research_topics_list()

  expect_is(rtl, "list")
  expect_length(rtl, 20)

  rsl <- research_subtopics_list()

  expect_is(rsl, "list")
  expect_length(rsl, 20)

  rtyl <- research_types_list()

  expect_is(rtyl, "list")
  expect_length(rtyl, 6)

  rbdf <- research_briefings(subtopic = "Falkland Islands")
  expect_is(rbdf, "data.frame")
  expect_length(rbdf, 14)
  expect_type(rbdf, "list")

  rbtsb <- research_briefings(topic = "Defence", subtopic = "Falkland Islands")
  expect_is(rbtsb, "data.frame")
  expect_length(rbtsb, 14)
  expect_type(rbtsb, "list")

  rbnu <- research_briefings()
  expect_is(rbnu, "data.frame")
  expect_length(rbnu, 14)
  expect_type(rbnu, "list")


})

