library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {

    skip_on_cran()

    aaqx <- hansard_all_answered_questions(4019, start_date = "2017-01-01")
    expect_length(aaqx, 18)
    expect_type(aaqx, "list")
    expect_true(tibble::is_tibble(aaqx))

    aaq <- hansard_all_answered_questions(start_date = "2017-03-01", end_date = "2017-03-02")
    expect_length(aaq, 38)
    expect_type(aaq, "list")
    expect_true(tibble::is_tibble(aaq))

    zhaaqtbling <- hansard_all_answered_questions(tabling_mp_id=172, start_date ='2016-01-01', end_date="2017-03-02")
    expect_length(zhaaqtbling, 32)
    expect_type(zhaaqtbling, "list")
    expect_true(tibble::is_tibble(zhaaqtbling))
    expect_true(nrow(zhaaqtbling)==86)

    anameid <- hansard_all_answered_questions(house="lords", answering_body=60, start_date ='2017-01-01')
    expect_length(anameid, 30)
    expect_type(anameid, "list")
    expect_true(tibble::is_tibble(anameid))

    bidname <- hansard_all_answered_questions(house=2, answering_body="Education", start_date ='2017-01-01')
    expect_length(bidname, 30)
    expect_type(bidname, "list")
    expect_true(tibble::is_tibble(bidname))
    expect_true(names(bidname[1])==names(anameid[1]))
    expect_true(nrow(bidname)==nrow(anameid))

})
