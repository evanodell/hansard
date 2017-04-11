library(hansard)
context("constituencies")

test_that("constituencies return expected format", {
    skip_on_cran()
    
    ctx <- constituencies(current = TRUE)
    
    expect_length(ctx, 9)
    expect_true(tibble::is_tibble(ctx))
    expect_equal(nrow(ctx), 650)
    
})
