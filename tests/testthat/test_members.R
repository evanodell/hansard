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

    # Lookup by member id

    mabbot <- members(172)

    expect_length(mabbot, 12)
    expect_type(mabbot, "list")
    expect_true(tibble::is_tibble(mabbot))

    memempt <- members()
    expect_length(memempt, 12)
    expect_type(memempt, "list")
    expect_true(tibble::is_tibble(memempt))

    camem <- commons_members()

    expect_length(camem, 12)
    expect_type(camem, "list")
    expect_true(tibble::is_tibble(camem))

    lamem <- lords_members()

    expect_length(lamem, 9)
    expect_type(lamem, "list")
    expect_true(tibble::is_tibble(lamem))

    lint <- lords_interests(530)

    expect_length(lint, 8)
    expect_type(lint, "list")
    expect_true(tibble::is_tibble(lint))

})
