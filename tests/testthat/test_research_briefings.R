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

  #rbdf <- research_briefings(subtopic = "Falkland Islands")

  #expect_is(rbdf, "data.frame")

})

