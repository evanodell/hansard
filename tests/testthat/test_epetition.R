library(hansard)
context("epetitions")

test_that("epetitions functions return expected format", {

    skip_on_cran()

    epetit <- hansard_epetition(ID = 706964, by_constituency = TRUE, verbose=TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_true(tibble::is_tibble(epetit))
    expect_true(nrow(epetit)==650)

    epetitnocont <- hansard_epetition(ID = 706964, by_constituency = FALSE, verbose=TRUE)
    expect_length(epetitnocont, 13)
    expect_type(epetitnocont, "list")
    expect_true(tibble::is_tibble(epetitnocont))
    expect_true(nrow(epetitnocont)==1)

    xepl <- hansard_epetition_tibble(start_date="2016-02-01", end_date="2016-03-25", max_signatures = 9, verbose=TRUE)
    expect_length(xepl, 8)
    expect_type(xepl, "list")
    expect_true(tibble::is_tibble(xepl))
    expect_true(nrow(xepl)==73)

})
