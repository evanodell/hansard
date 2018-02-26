library(hansard)
context("papers, publications, sessions, tv part 2")

test_that("papers laid, publications, sessions, tv part 2, expected format", {

    skip_on_cran()

    tvclipsmp <- hansard_tv_clips(4591, start_date = "2016-01-11",
                                  end_date = "2017-01-12", verbose = TRUE)
    expect_length(tvclipsmp, 7)
    expect_type(tvclipsmp, "list")
    expect_true(tibble::is_tibble(tvclipsmp))

    chan <- hansard_tv_channels(verbose=TRUE)
    expect_length(chan, 3)
    expect_type(chan, "list")
    expect_true(tibble::is_tibble(chan))

    gen <- hansard_generic("elections.json?")
    expect_length(gen, 5)
    expect_type(gen, "list")
    expect_true(tibble::is_tibble(gen))

    sess <- hansard_sessions_info(days = FALSE, verbose=TRUE)
    expect_length(sess, 9)
    expect_type(sess, "list")
    expect_true(tibble::is_tibble(sess))

})
