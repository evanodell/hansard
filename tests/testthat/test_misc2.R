library(hansard)
context("papers, publications, sessions, tv")

test_that("papers laid, publications, sessions, tv  functions return expected format", {

  skip_on_cran()
  skip_on_travis()

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

  sessdays <- sessions_info(days = TRUE, start_date = "2017-01-01", end_date = "2017-01-10")
  expect_length(sessdays, 7)
  expect_type(sessdays, "list")
  expect_true(tibble::is_tibble(sessdays))

  sess <- sessions_info(days = FALSE)
  expect_length(sess, 9)
  expect_type(sess, "list")
  expect_true(tibble::is_tibble(sess))

})