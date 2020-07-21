library(hansard)
context("election_results")


test_that("election_results return expected format", {
  skip_on_cran()
  skip_if(Sys.getenv(x = "TRAVIS_R_VERSION_STRING") == "devel")

  elect_all_data <- hansard_election_results(
    ID = 382037,
    all_data = FALSE,
    verbose = TRUE
  )
  expect_length(elect_all_data, 9)
  expect_type(elect_all_data, "list")
  expect_true(tibble::is_tibble(elect_all_data))
  expect_true(nrow(elect_all_data) == 650)
})
