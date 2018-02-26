library(hansard)
context("papers, publications, sessions, tv part 1")

test_that("papers laid, publications, sessions, tv return expected format", {

    skip_on_cran()

    pldec <- hansard_papers_laid(withdrawn = TRUE, house = "lords",
                                 start_date = "2015-12-01",
                                 end_date = "2015-12-31", verbose=TRUE)
    expect_length(pldec, 16)
    expect_type(pldec, "list")
    expect_true(tibble::is_tibble(pldec))
    expect_equal(nrow(pldec), 11)

    publ <- hansard_publication_logs(683267, verbose=TRUE)
    expect_length(publ, 11)
    expect_type(publ, "list")
    expect_true(tibble::is_tibble(publ))
    expect_equal(nrow(publ), 1)

    publjan <- hansard_publication_logs(start_date = "2016-01-01",
                                        end_date = "2016-01-10", verbose=TRUE)
    expect_length(publjan, 5)
    expect_type(publjan, "list")
    expect_true(tibble::is_tibble(publjan))
    expect_equal(nrow(publjan), 2)

    publcommons <- hansard_publication_logs(house="commons",
                                            start_date = "2016-01-01",
                                            end_date = "2016-01-15",
                                            tidy=FALSE, verbose=TRUE)
    expect_length(publcommons, 5)
    expect_type(publcommons, "list")
    expect_true(tibble::is_tibble(publcommons))
    expect_equal(nrow(publcommons), 22)

    # TV Programmes
    tvcoms <- hansard_tv_programmes("commons", start_date = "2016-12-19",
                                    end_date = "2017-01-01", verbose=TRUE)
    expect_length(tvcoms, 9)
    expect_type(tvcoms, "list")
    expect_true(tibble::is_tibble(tvcoms))
    expect_equal(nrow(tvcoms), 16)


})
