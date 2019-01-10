library(hansard)
context("research_briefings")


test_that("research_briefings return expected format", {
  skip_on_cran()

  rtl <- hansard_research_topics_list()
  expect_is(rtl, "list")
  # expect_length(rtl, 20)

  rsl <- hansard_research_subtopics_list()
  expect_is(rsl, "list")
  # expect_length(rsl, 20)

  rtyl <- hansard_research_types_list()
  expect_is(rtyl, "list")
  # expect_length(rtyl, 6)

  rbdf <- hansard_research_briefings(
    subtopic = "Falkland Islands",
    verbose = TRUE
  )
  expect_length(rbdf, 14)
  expect_true(tibble::is_tibble(rbdf))

  rbtsb <- hansard_research_briefings(
    topic = "Defence",
    subtopic = "Falkland Islands",
    verbose = TRUE
  )
  # expect_length(rbtsb, 14)
  expect_true(tibble::is_tibble(rbtsb))
  expect_true(rbdf[[1]][[1]] == rbtsb[[1]][[1]])
})
