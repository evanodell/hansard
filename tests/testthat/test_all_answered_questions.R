library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {

    skip_on_cran()

    aaqx <- hansard_all_answered_questions(mp_id = c(4019, 3980), tabling_mp_id = c(338, 172), start_date = "2016-12-18", end_date = "2017-01-12")
    expect_length(aaqx, 18)
    expect_type(aaqx, "list")
    expect_true(tibble::is_tibble(aaqx))

    aaq <- hansard_all_answered_questions(start_date = "2017-03-01", end_date = "2017-03-01")
    expect_length(aaq, 38)
    expect_type(aaq, "list")
    expect_true(tibble::is_tibble(aaq))

    xaaqda <- hansard_all_answered_questions(tabling_mp_id=172, start_date ='2016-12-18', end_date="2017-03-02")
    expect_length(xaaqda, 30)
    expect_type(xaaqda, "list")
    expect_true(tibble::is_tibble(xaaqda))
    #expect_true(nrow(zhaaqtbling)==86)

    anameid <- hansard_all_answered_questions(house="lords", answering_body=60, start_date ='2017-03-01', end_date='2017-03-20')
    expect_length(anameid, 32)
    expect_type(anameid, "list")
    expect_true(tibble::is_tibble(anameid))

    bidname <- hansard_all_answered_questions(house=2, answering_body="Education", start_date ='2017-03-01', end_date='2017-03-20')
    expect_length(bidname, 32)
    expect_type(bidname, "list")
    expect_true(tibble::is_tibble(bidname))
    expect_true(names(bidname[1])==names(anameid[1]))
    expect_true(nrow(bidname)==nrow(anameid))

})
