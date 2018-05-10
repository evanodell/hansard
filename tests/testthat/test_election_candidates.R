library(hansard)
context("election_candidates")


test_that("election_candidates return expected format", {
  skip_on_cran()
  # skip_on_travis()

  xec <- hansard_election_candidates(ID = 650517, verbose = TRUE)
  expect_length(xec, 18)
  expect_type(xec, "list")
  expect_true(tibble::is_tibble(xec))
  expect_true(nrow(xec) == 1)
})
