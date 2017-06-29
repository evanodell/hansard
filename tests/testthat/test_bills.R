library(hansard)
context("bills")

test_that("bills return expected format", {

    skip_on_cran()
    

    xb <- hansard_bills(start_date = "2017-01-01")
    expect_length(xb, 10)
    expect_true(tibble::is_tibble(xb))

    xba <- hansard_bills(amendments = TRUE, start_date = "2016-12-21")
    expect_length(xba, 14)
    expect_true(tibble::is_tibble(xba))

    xbid <- hansard_bills(1719)
    expect_length(xbid, 10)
    expect_true(tibble::is_tibble(xbid))

    bst <- hansard_bill_stage_types()
    expect_length(bst, 7)
    expect_true(tibble::is_tibble(bst))


})
