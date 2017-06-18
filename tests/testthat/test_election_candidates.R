library(hansard)
context("elections")


test_that("election functions return expected format", {

    skip_on_cran()

    xec <- election_candidates(ID=382037)
    expect_length(xec, 127)
    expect_type(xec, "list")
    expect_true(tibble::is_tibble(xec))
    expect_true(nrow(xec)==650)

})

