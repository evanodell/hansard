library(hansard)
context("epetitions")

test_that("epetitions functions return expected format", {

    skip_on_cran()

    epetit <- epetition(ID = 706964, by_constituency = TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_true(tibble::is_tibble(epetit))

    epetitnocont <- epetition(ID = 706964, by_constituency = FALSE)
    expect_length(epetitnocont, 12)
    expect_type(epetitnocont, "list")
    expect_true(tibble::is_tibble(epetitnocont))

})
