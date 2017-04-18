library(hansard)
context("elections")


test_that("election functions return expected format", {
    skip_on_cran()

    elect <- elections()
    expect_length(elect, 5)
    expect_type(elect, "list")
    expect_true(tibble::is_tibble(elect))

    electid <- elections(517994)
    expect_length(electid, 6)
    expect_type(electid, "list")
    expect_true(tibble::is_tibble(electid))
    expect_equal(nrow(electid), 1)

    elre <- election_results()
    expect_length(elre, 9)
    expect_type(elre, "list")
    expect_true(tibble::is_tibble(elre))

    elre2010 <- election_results(382037)
    expect_length(elre2010, 9)
    expect_type(elre2010, "list")
    expect_true(tibble::is_tibble(elre2010))
    expect_equal(nrow(elre2010), 650)

    elre2010perc <- election_results(ID=382037, calculate_percent = TRUE)
    expect_length(elre2010perc, 11)
    expect_type(elre2010perc, "list")
    expect_true(tibble::is_tibble(elre2010perc))
    expect_equal(nrow(elre2010perc), 650)

})

