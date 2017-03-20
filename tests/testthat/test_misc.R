library(hansard)
context("papers, publications, sessions, tv")

test_that("papers laid, publications, sessions, tv  functions return expected format", {
  skip_on_cran()

  papl <- papers_laid(withdrawn = FALSE, house = 'commons', start_date='2016-12-01', end_date="2017-02-02")
  expect_length(papl, 16)
  expect_type(papl, "list")
  expect_is(papl, "data.frame")

  pldec <- papers_laid(withdrawn = FALSE, house = 'commons', start_date='2015-12-01', end_date="2015-12-31")
  expect_length(pldec, 16)
  expect_type(pldec, "list")
  expect_is(pldec, "data.frame")
  expect_equal(nrow(pldec),123)

  publ <- publication_logs(683267)
  expect_length(publ, 11)
  expect_type(publ, "list")
  expect_is(publ, "data.frame")
  expect_equal(nrow(publ),1)

  publjan <- publication_logs(start_date='2016-01-01', end_date='2016-01-31')
  expect_length(publjan, 5)
  expect_type(publjan, "list")
  expect_is(publjan, "data.frame")
  expect_equal(nrow(publjan),172)

  sessdays <- sessions_info(days=TRUE, start_date='2017-01-01', end_date='2017-02-28')
  expect_length(sessdays, 7)
  expect_type(sessdays, "list")
  expect_is(sessdays, "data.frame")

  sess <- sessions_info(days=FALSE)
  expect_length(sess, 9)
  expect_type(sess, "list")
  expect_is(sess, "data.frame")

  #TV Programmes

  tvcoms <- tv_programmes('commons', start_date='2016-12-10', end_date = '2017-02-14')
  expect_length(tvcoms, 8)
  expect_type(tvcoms, "list")
  expect_is(tvcoms, "data.frame")

  tvlords <- tv_programmes('lords', start_date='2016-12-10', end_date = '2017-02-14')
  expect_length(tvlords, 8)
  expect_type(tvlords, "list")
  expect_is(tvlords, "data.frame")

  tvclips <- tv_clips(4591)
  expect_length(tvclips, 6)
  expect_type(tvclips, "list")
  expect_is(tvclips, "data.frame")

  chan <- tv_channels()
  expect_length(chan, 3)
  expect_type(chan, "list")
  expect_is(chan, "data.frame")

})
