library(hansard)
context("elections")


test_that("election functions return expected format", {
    
    skip_on_cran()
    
    elect <- elections()
    expect_length(elect, 5)
    expect_type(elect, "list")
    expect_true(tibble::is_tibble(elect))
    
    electid <- elections(ID = 517994)
    expect_length(electid, 6)
    expect_type(electid, "list")
    expect_true(tibble::is_tibble(electid))
    expect_equal(nrow(electid), 1)
    
    elreidperc <- election_results(ID = 517994, calculate_percent = TRUE)
    expect_length(elreidperc, 11)
    expect_type(elreidperc, "list")
    expect_true(tibble::is_tibble(elreidperc))
    
})

