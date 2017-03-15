
context("research_briefings tests")


test_that("research_briefings list calls work", {
skip_on_cran()

  rtl <- research_topics_list()

  expect_is(rtl, "list")
  expect_length(rtl, 10)

})
