library(hansard)
context("election_candidates")


test_that("election_candidates return expected format", {

    skip_on_cran()
    skip_on_travis()

    xec <- hansard_election_candidates(ID=382037)
    expect_length(xec, 127)
    expect_type(xec, "list")
    expect_true(tibble::is_tibble(xec))
    expect_true(nrow(xec)==650)

})
