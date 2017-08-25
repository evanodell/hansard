library(hansard)
context("elections")


test_that("election functions return expected format", {

    skip_on_cran()

    elect <- hansard_elections(verbose=TRUE)
    expect_length(elect, 5)
    expect_type(elect, "list")
    expect_true(tibble::is_tibble(elect))

    electid <- hansard_elections(ID = 517994, verbose=TRUE)
    expect_length(electid, 6)
    expect_type(electid, "list")
    expect_true(tibble::is_tibble(electid))
    expect_equal(nrow(electid), 1)

    elreidperc <- hansard_election_results(ID = 517994, calculate_percent = TRUE, verbose=TRUE)
    expect_length(elreidperc, 11)
    expect_type(elreidperc, "list")
    expect_true(tibble::is_tibble(elreidperc))

    electall <- hansard_election_results(verbose=TRUE)
    expect_length(electall, 9)
    expect_type(electall, "list")
    expect_true(tibble::is_tibble(electall))

    electcons <- hansard_election_results(ID=730039, calculate_percent = TRUE, constit_details = TRUE, verbose=TRUE)
    expect_length(electcons, 19)
    expect_type(electcons, "list")
    expect_true(tibble::is_tibble(electcons))
    expect_true(nrow(electcons)==650)

})
