library(hansard)
context("all_answered_questions part1")

test_that("all_answered_questions return expected format", {
  skip_on_cran()

  aaqx <- hansard_all_answered_questions(
    mp_id = c(4019, 3980),
    answering_body = c("health", "justice"), house = "commons",
    start_date = "2016-12-18", end_date = "2017-01-15", verbose = TRUE
  )
  #expect_length(aaqx, 39)
  expect_type(aaqx, "list")
  expect_true(tibble::is_tibble(aaqx))
  expect_equal(nrow(aaqx), 165)

  aaq <- hansard_all_answered_questions(
    start_date = "2017-03-01",
    end_date = "2017-03-01", verbose = TRUE
  )
  #expect_length(aaq, 37)
  expect_type(aaq, "list")
  expect_true(tibble::is_tibble(aaq))

  xaaqda <- hansard_all_answered_questions(
    tabling_mp_id = 172,
    start_date = "2016-12-18",
    end_date = "2017-03-02",
    verbose = TRUE
  )
  #expect_length(xaaqda, 29)
  expect_type(xaaqda, "list")
  expect_true(tibble::is_tibble(xaaqda))
  expect_equal(nrow(xaaqda), 6)
})
