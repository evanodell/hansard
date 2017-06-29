library(hansard)
context("constituencies")

test_that("constituencies functions return expected format", {

    skip_on_cran()
    

    ctx <- hansard_constituencies(current = TRUE)
    expect_length(ctx, 9)
    expect_true(tibble::is_tibble(ctx))
    expect_equal(nrow(ctx), 650)
    expect_true(names(ctx)[5]=="ended_date_value")

})
