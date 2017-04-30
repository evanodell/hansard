library(hansard)
context("epetitions")


test_that("epetitions functions return expected format", {
    skip_on_cran()
    
    
    epetit <- epetition(ID = 706964, by_constituency = TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_true(tibble::is_tibble(epetit))
    Sys.sleep(45)
    
    epetitnocont <- epetition(ID = 706964, by_constituency = FALSE)
    expect_length(epetitnocont, 13)
    expect_type(epetitnocont, "list")
    expect_true(tibble::is_tibble(epetitnocont))
    Sys.sleep(45)
    
    epetitempty <- epetition()
    expect_length(epetitempty, 8)
    expect_type(epetitempty, "list")
    expect_true(tibble::is_tibble(epetitempty))
    Sys.sleep(45)
    
})

