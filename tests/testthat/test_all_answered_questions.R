library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {

    skip_on_cran()

    aaqx <- all_answered_questions(4019, start_date = "2017-01-01")
    expect_length(aaqx, 18)
    expect_type(aaqx, "list")
    expect_true(tibble::is_tibble(aaqx))

    aaq <- all_answered_questions(start_date = "2017-03-01", end_date = "2017-03-02")
    expect_length(aaq, 38)
    expect_type(aaq, "list")
    expect_true(tibble::is_tibble(aaq))


    zhaaqtbling <- hansard_all_answered_questions(tabling_mp_id=172, start_date ='2016-01-01', end_date="2017-03-02")
    expect_length(zhaaqtbling, 32)
    expect_type(zhaaqtbling, "list")
    expect_true(tibble::is_tibble(zhaaqtbling))
    expect_true(nrow(zhaaqtbling)==86)



})
