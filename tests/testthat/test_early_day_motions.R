library(hansard)
context("early day motions")

test_that("edm functions return expected format", {
    
    
    xedmid <- early_day_motions(edm_id = 1073)
    expect_length(xedmid, 11)
    expect_type(xedmid, "list")
    expect_is(xedmid, "data.frame")
    
    xedmids <- early_day_motions(edm_id = 1073, session = "2015/16")
    expect_length(xedmids, 11)
    expect_type(xedmids, "list")
    expect_is(xedmids, "data.frame")
    expect_equal(nrow(xedmids), 1)
    
    xedmid_full <- mp_edms(mp_id = 3967, primary_sponsor = TRUE, sponsor = FALSE, signatory = TRUE, full_data = TRUE)
    
    expect_length(xedmid_full, 30)
    expect_type(xedmid_full, "list")
    expect_is(xedmid_full, "data.frame")
    
})





