library(hansard)
context("papers, publications, sessions, tv")

test_that("papers laid, publications, sessions, tv  functions return expected format", {

    skip_on_cran()
    skip_on_travis()

    papl <- papers_laid(withdrawn = FALSE, house = "commons", start_date = "2016-12-01", end_date = "2017-02-02")
    expect_length(papl, 16)
    expect_type(papl, "list")
    expect_true(tibble::is_tibble(papl))
    

    pldec <- papers_laid(withdrawn = TRUE, house = "lords", start_date = "2015-12-01", end_date = "2015-12-31")
    expect_length(pldec, 16)
    expect_type(pldec, "list")
    expect_true(tibble::is_tibble(pldec))
    
    expect_equal(nrow(pldec), 11)

    publ <- publication_logs(683267)
    expect_length(publ, 11)
    expect_type(publ, "list")
    expect_true(tibble::is_tibble(publ))
    
    expect_equal(nrow(publ), 1)

    publjan <- publication_logs(start_date = "2016-01-01", end_date = "2016-01-31")
    expect_length(publjan, 5)
    expect_type(publjan, "list")
    expect_true(tibble::is_tibble(publjan))
    
    expect_equal(nrow(publjan), 172)

    sessdays <- sessions_info(days = TRUE, start_date = "2017-01-01", end_date = "2017-02-28")
    expect_length(sessdays, 7)
    expect_type(sessdays, "list")
    expect_true(tibble::is_tibble(sessdays))
    

    sess <- sessions_info(days = FALSE)
    expect_length(sess, 9)
    expect_type(sess, "list")
    expect_true(tibble::is_tibble(sess))
    

    # TV Programmes

    tvcoms <- tv_programmes("commons", start_date = "2016-12-10", end_date = "2017-02-14")
    expect_length(tvcoms, 8)
    expect_type(tvcoms, "list")
    expect_true(tibble::is_tibble(tvcoms))
    

    tvlords <- tv_programmes("lords", start_date = "2016-12-10", end_date = "2017-02-14")
    expect_length(tvlords, 8)
    expect_type(tvlords, "list")
    expect_true(tibble::is_tibble(tvlords))
    

    tvclips <- tv_clips(4591)
    expect_length(tvclips, 6)
    expect_type(tvclips, "list")
    expect_true(tibble::is_tibble(tvclips))
    

    tvclipsempty <- tv_clips(start_date = "2016-01-11", end_date = "2016-01-12")
    expect_length(tvclipsempty, 6)
    expect_type(tvclipsempty, "list")
    expect_true(tibble::is_tibble(tvclipsempty))
    
    expect_equal(nrow(tvclipsempty), 378)

    chan <- tv_channels()
    expect_length(chan, 3)
    expect_type(chan, "list")
    expect_true(tibble::is_tibble(chan))
    

    chanmess <- tv_channels(tidy = FALSE)
    expect_length(chanmess, 3)
    expect_type(chanmess, "list")
    expect_true(tibble::is_tibble(chanmess))
    

    gen <- hansard_generic("elections.json")
    expect_length(gen, 5)
    expect_type(gen, "list")
    expect_true(tibble::is_tibble(gen))
    


})
