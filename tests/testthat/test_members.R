library(hansard)
context("members")

test_that("members functions return expected format", {
  skip_on_cran()

  # Members search

  mschris <- members_search('chris')

  expect_length(mschris, 12)
  expect_type(mschris, "list")
  expect_is(mschris, "data.frame")

  #Lookup by member id

  mabbot <- members(172)

  expect_length(mabbot, 13)
  expect_type(mabbot, "list")
  expect_is(mabbot, "data.frame")

  memempt <- members()
  expect_length(memempt, 12)
  expect_type(memempt, "list")
  expect_is(memempt, "data.frame")

  camem <- commons_members()

  expect_length(camem, 12)
  expect_type(camem, "list")
  expect_is(camem, "data.frame")

  lamem <- lords_members()

  expect_length(lamem, 9)
  expect_type(lamem, "list")
  expect_is(lamem, "data.frame")

  lint <- lords_interests(530)

  expect_length(lint, 8)
  expect_type(lint, "list")
  expect_is(lint, "data.frame")

})
