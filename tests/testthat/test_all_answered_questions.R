library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {
    
    skip_on_cran()
    
    aaqx <- all_answered_questions(4019, start_date = "2017-01-01")
    
    expect_length(aaqx, 17)
    expect_type(aaqx, "list")
    expect_true(tibble::is_tibble(aaqx))
    
    aaq <- all_answered_questions(start_date = "2017-03-01", end_date = "2017-03-02")
    
    expect_length(aaq, 37)
    expect_type(aaq, "list")
    expect_true(tibble::is_tibble(aaq))
    
})
