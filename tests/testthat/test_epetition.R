library(hansard)
context("epetitions")


test_that("epetitions functions return expected format", {
    skip_on_cran()
    
    
    epetit <- epetition(ID = 706964, by_constituency = TRUE)
    expect_length(epetit, 7)
    expect_type(epetit, "list")
    expect_is(epetit, "data.frame")
    
    epetitnocont <- epetition(ID = 706964, by_constituency = FALSE)
    expect_length(epetitnocont, 17)
    expect_type(epetitnocont, "list")
    expect_is(epetitnocont, "data.frame")
    
    epetitempty <- epetition()
    expect_length(epetitempty, 8)
    expect_type(epetitempty, "list")
    expect_is(epetitempty, "data.frame")
    
    epetitblank <- epetition(ID = 7e+05)
    
    
})

