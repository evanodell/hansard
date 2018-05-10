library(hansard)
context("members")

test_that("members functions return expected format", {
  skip_on_cran()

  # Members search
  # msabbot <- hansard_members_search(search = "*abbot*", verbose = TRUE)
  # expect_length(msabbot, 12)
  # expect_type(msabbot, "list")
  # expect_true(tibble::is_tibble(msabbot))

  # Lookup by member id
  mabbot <- hansard_members(172, verbose = TRUE)
  expect_length(mabbot, 13)
  expect_type(mabbot, "list")
  expect_true(tibble::is_tibble(mabbot))

  memempt <- hansard_members(verbose = TRUE)
  expect_length(memempt, 12)
  expect_type(memempt, "list")
  expect_true(tibble::is_tibble(memempt))

  camem <- hansard_commons_members(verbose = TRUE)
  expect_length(camem, 12)
  expect_type(camem, "list")
  expect_true(tibble::is_tibble(camem))

  lamem <- hansard_lords_members(verbose = TRUE)
  expect_length(lamem, 9)
  expect_type(lamem, "list")
  expect_true(tibble::is_tibble(lamem))

  lint <- hansard_lords_interests(530, verbose = TRUE)
  expect_length(lint, 8)
  expect_type(lint, "list")
  expect_true(tibble::is_tibble(lint))


  a <- members()
  b <- members_search()
  expect_equal(a, b)
})
