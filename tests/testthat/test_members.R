library(hansard)
context("members")

test_that("members functions return expected format", {
    skip_on_cran()
 skip_on_travis()

    # Members search

    mschris <- members_search("chris")

    expect_length(mschris, 12)
    expect_type(mschris, "list")
    expect_true(tibble::is_tibble(mschris))
    Sys.sleep(45)

    # Lookup by member id

    mabbot <- members(172)

    expect_length(mabbot, 12)
    expect_type(mabbot, "list")
    expect_true(tibble::is_tibble(mabbot))
    Sys.sleep(45)

    memempt <- members()
    expect_length(memempt, 12)
    expect_type(memempt, "list")
    expect_true(tibble::is_tibble(memempt))
    Sys.sleep(45)

    camem <- commons_members()

    expect_length(camem, 12)
    expect_type(camem, "list")
    expect_true(tibble::is_tibble(camem))
    Sys.sleep(45)

    lamem <- lords_members()

    expect_length(lamem, 9)
    expect_type(lamem, "list")
    expect_true(tibble::is_tibble(lamem))
    Sys.sleep(45)

    lint <- lords_interests(530)

    expect_length(lint, 8)
    expect_type(lint, "list")
    expect_true(tibble::is_tibble(lint))
    Sys.sleep(45)

})
