library(hansard)
context("epetitions")

test_that("epetitions functions return expected format", {

    skip_on_cran()
    skip_on_travis()

    epetit <- hansard_epetition(ID = 706964, by_constituency = TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_true(tibble::is_tibble(epetit))

    epetitnocont <- hansard_epetition(ID = 706964, by_constituency = FALSE)
    expect_length(epetitnocont, 12)
    expect_type(epetitnocont, "list")
    expect_true(tibble::is_tibble(epetitnocont))

    xepl <- hansard_epetition_tibble(start_date="2016-02-01", end_date="2016-08-25", max_signatures = 9)
    expect_length(xepl, 8)
    expect_type(xepl, "list")
    expect_true(tibble::is_tibble(xepl))
    expect_true(nrow(xepl)==472)

})
