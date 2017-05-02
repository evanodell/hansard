library(hansard)
context("epetitions")


test_that("epetitions functions return expected format", {
    skip_on_cran()
 skip_on_travis()


    epetit <- epetition(ID = 706964, by_constituency = TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_true(tibble::is_tibble(epetit))

    epetitnocont <- epetition(ID = 706964, by_constituency = FALSE)
    expect_length(epetitnocont, 13)
    expect_type(epetitnocont, "list")
    expect_true(tibble::is_tibble(epetitnocont))

    epetitempty <- epetition()
    expect_length(epetitempty, 8)
    expect_type(epetitempty, "list")
    expect_true(tibble::is_tibble(epetitempty))

})

