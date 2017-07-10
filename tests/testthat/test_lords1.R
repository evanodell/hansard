library(hansard)
context("lords functions")


test_that("lords functions return expected format", {

    skip_on_cran()

    # Voting Records
    lvotesall <- hansard_lord_vote_record(530, lobby = "all", start_date = "2017-03-01")
    expect_length(lvotesall, 11)
    expect_type(lvotesall, "list")
    expect_true(tibble::is_tibble(lvotesall))

    lvotesnot <- hansard_lord_vote_record(530, lobby = "notcontent", start_date = "2017-03-01")
    expect_length(lvotesnot, 10)
    expect_type(lvotesnot, "list")
    expect_true(tibble::is_tibble(lvotesnot))

    lvotescont <- hansard_lord_vote_record(530, lobby = "content", start_date = "2017-03-01")
    expect_length(lvotescont, 10)
    expect_type(lvotescont, "list")
    expect_true(tibble::is_tibble(lvotescont))

    # Amendments
    lamend <- hansard_lords_amendments(decision = "Agreed", start_date = "2017-03-17")
    expect_length(lamend, 31)
    expect_type(lamend, "list")
    expect_true(tibble::is_tibble(lamend))

    lamendnu <- hansard_lords_amendments(decision = NULL, start_date = "2017-03-17", end_date = "2017-03-20")
    expect_length(lamendnu, 29)
    expect_type(lamendnu, "list")
    expect_true(tibble::is_tibble(lamendnu))



})

