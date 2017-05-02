library(hansard)
context("constituencies")

test_that("constituencies functions return expected format", {
    skip_on_cran()
 skip_on_travis()

    ctx <- constituencies(current = TRUE)

    expect_length(ctx, 9)
    expect_true(tibble::is_tibble(ctx))
    expect_equal(nrow(ctx), 650)

})
