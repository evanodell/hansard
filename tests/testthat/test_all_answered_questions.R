library(hansard)
context("all_answered_questions")

test_that("all_answered_questions return expected format", {

    skip_on_cran()

    aaqx <- hansard_all_answered_questions(mp_id = c(4019, 3980), tabling_mp_id = c(338, 172), answering_body = c("health", "justice"), house = "commons", start_date = "2016-12-18", end_date = "2017-03-12", verbose = TRUE)
    expect_length(aaqx, 32)
    expect_type(aaqx, "list")
    expect_true(tibble::is_tibble(aaqx))
    expect_equal(nrow(aaqx), 23)

    aaq <- hansard_all_answered_questions(start_date = "2017-03-01", end_date = "2017-03-01")
    expect_length(aaq, 38)
    expect_type(aaq, "list")
    expect_true(tibble::is_tibble(aaq))

    xaaqda <- hansard_all_answered_questions(tabling_mp_id=172, start_date ='2016-12-18', end_date="2017-03-02")
    expect_length(xaaqda, 30)
    expect_type(xaaqda, "list")
    expect_true(tibble::is_tibble(xaaqda))
    expect_equal(nrow(xaaqda),6)

    anameid <- hansard_all_answered_questions(house="lords", answering_body=60, start_date ='2017-03-01', end_date='2017-03-20')
    expect_length(anameid, 32)
    expect_type(anameid, "list")
    expect_true(tibble::is_tibble(anameid))
    expect_equal(nrow(anameid),38)

    bidname <- hansard_all_answered_questions(house=2, answering_body="Education", start_date ='2017-03-01', end_date='2017-03-20')
    expect_length(bidname, 32)
    expect_type(bidname, "list")
    expect_true(tibble::is_tibble(bidname))
    expect_equal(nrow(bidname),38)
    expect_true(names(bidname[1])==names(anameid[1]))
    expect_equivalent(names(bidname), names(anameid))
    expect_equal(nrow(bidname), nrow(anameid))

})
