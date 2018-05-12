library(hansard)
context("misc MP functions")

test_that("members vote record, questions functions return expected format", {
  skip_on_cran()

  mpqs <- hansard_mp_questions(c(172, 4019), "all", start_date = "2015-02-01",
                               end_date = "2015-07-01", verbose = TRUE)
  expect_length(mpqs, 11)
  expect_type(mpqs, "list")
  expect_true(tibble::is_tibble(mpqs))

  # mpoqs <- hansard_mp_questions(172, "oral", start_date = "2015-02-01",
  # end_date = "2015-07-01", verbose=TRUE)
  # expect_length(mpoqs, 24)
  # expect_type(mpoqs, "list")
  # expect_true(tibble::is_tibble(mpoqs))

  mpvrall <- hansard_mp_vote_record(172, lobby = "all",
                                    start_date = "2017-01-01",
                                    end_date = "2017-03-01", verbose = TRUE)
  expect_length(mpvrall, 6)
  expect_type(mpvrall, "list")
  expect_true(tibble::is_tibble(mpvrall))

  # mpvraye <- hansard_mp_vote_record(172, lobby = "aye",
  # start_date = "2017-01-01", end_date = "2017-02-01", tidy = FALSE)
  # expect_length(mpvraye, 5)
  # expect_type(mpvraye, "list")
  # expect_true(tibble::is_tibble(mpvraye))
  #
  # mpvno <- hansard_mp_vote_record(172, lobby = "no",
  # start_date = "2017-01-01", end_date = "2017-02-01")
  # expect_length(mpvno, 6)
  # expect_type(mpvno, "list")
  # expect_true(tibble::is_tibble(mpvno))
})
