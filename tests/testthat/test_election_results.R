library(hansard)
context("election_results")


test_that("election_results return expected format", {

    skip_on_cran()

    elect_all_data <- hansard_election_results(ID=382037, all_data = FALSE)
    expect_length(elect_all_data, 127)
    expect_type(elect_all_data, "list")
    expect_true(tibble::is_tibble(elect_all_data))
    expect_true(nrow(elect_all_data)==650)
    expect_true(elect_all_data$Conservative[[3]]==3133)

})
