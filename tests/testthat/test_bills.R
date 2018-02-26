library(hansard)
context("bills")

test_that("bills return expected format", {

    skip_on_cran()

    xb <- hansard_bills(start_date = "2017-07-01", verbose=TRUE)
    expect_length(xb, 10)
    expect_true(tibble::is_tibble(xb))

    xba <- hansard_bills(amendments = TRUE, start_date = "2016-12-21",
                         verbose=TRUE)
    expect_length(xba, 14)
    expect_true(tibble::is_tibble(xba))

    xbid <- hansard_bills(1719, verbose=TRUE)
    expect_length(xbid, 10)
    expect_true(tibble::is_tibble(xbid))

    bst <- hansard_bill_stage_types(verbose=TRUE)
    expect_length(bst, 7)
    expect_true(tibble::is_tibble(bst))

    bill_pubs <- bill_publications(ID=752025)
    expect_length(bill_pubs, 10)
    expect_true(tibble::is_tibble(bill_pubs))

    bill_pubs_date <- bill_publications(start_date = "2018-01-01",
                                        end_date = "2018-01-12")
    expect_length(bill_pubs_date, 11)
    expect_true(tibble::is_tibble(bill_pubs_date))
    expect_true(nrow(bill_pubs_date)==8)

})
