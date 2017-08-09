library(hansard)
context("early day motions")

test_that("edm functions return expected format", {

    skip_on_cran()


    xedmid <- hansard_early_day_motions(edm_id = 1073)
    expect_length(xedmid, 11)
    expect_type(xedmid, "list")
    expect_true(tibble::is_tibble(xedmid))

    xedmids <- hansard_early_day_motions(edm_id = 1073, session = "2015/16")
    expect_length(xedmids, 11)
    expect_type(xedmids, "list")
    expect_true(tibble::is_tibble(xedmids))
    expect_equal(nrow(xedmids), 1)

    xedmid_full <- hansard_mp_edms(mp_id = 3967, primary_sponsor = TRUE, sponsor = FALSE, signatory = TRUE, full_data = TRUE)
    expect_length(xedmid_full, 30)
    expect_type(xedmid_full, "list")
    expect_true(tibble::is_tibble(xedmid_full))



})
