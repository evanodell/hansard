
library(hansard)
context("all_answered_questions part2")

test_that("all_answered_questions return expected format", {
  skip_on_cran()
#skip_on_travis()

  anameid <- hansard_all_answered_questions(
    house = "lords", answering_body = 60,
    start_date = "2017-03-01",
    end_date = "2017-03-20",
    verbose = TRUE
  )
  expect_length(anameid, 31)
  expect_type(anameid, "list")
  expect_true(tibble::is_tibble(anameid))
  expect_equal(nrow(anameid), 38)

  bidname <- hansard_all_answered_questions(
    house = 2,
    answering_body = "Education",
    start_date = "2017-03-01",
    end_date = "2017-03-20",
    verbose = TRUE
  )
  expect_length(bidname, 31)
  expect_type(bidname, "list")
  expect_true(tibble::is_tibble(bidname))
  expect_equal(nrow(bidname), 38)
  expect_true(names(bidname[1]) == names(anameid[1]))
  expect_equivalent(names(bidname), names(anameid))
  expect_equal(nrow(bidname), nrow(anameid))
})
