library(hansard)
context("papers, publications, sessions, tv")

test_that("papers laid, publications, sessions, tv  functions return expected format", {

    skip_on_cran()

    papl <- hansard_papers_laid(withdrawn = FALSE, house = "commons", start_date = "2017-01-30", end_date = "2017-02-02")
    expect_length(papl, 16)
    expect_type(papl, "list")
    expect_true(tibble::is_tibble(papl))

    pldec <- hansard_papers_laid(withdrawn = TRUE, house = "lords", start_date = "2015-12-01", end_date = "2015-12-31")
    expect_length(pldec, 16)
    expect_type(pldec, "list")
    expect_true(tibble::is_tibble(pldec))
    expect_equal(nrow(pldec), 11)

    publ <- hansard_publication_logs(683267)
    expect_length(publ, 11)
    expect_type(publ, "list")
    expect_true(tibble::is_tibble(publ))
    expect_equal(nrow(publ), 1)

    publjan <- hansard_publication_logs(start_date = "2016-01-01", end_date = "2016-01-10")
    expect_length(publjan, 5)
    expect_type(publjan, "list")
    expect_true(tibble::is_tibble(publjan))
    expect_equal(nrow(publjan), 2)

    # TV Programmes

    tvcoms <- hansard_tv_programmes("commons", start_date = "2016-12-10", end_date = "2017-02-14")
    expect_length(tvcoms, 9)
    expect_type(tvcoms, "list")
    expect_true(tibble::is_tibble(tvcoms))


})
