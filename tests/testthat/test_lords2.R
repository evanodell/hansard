library(hansard)
context("lords part 2 functions")


test_that("lords functions return expected format", {

  skip_on_cran()

  # Divisions
  ldivsum <- hansard_lords_divisions(division_id = 705891, summary = TRUE, tidy = TRUE, verbose = TRUE)
  expect_length(ldivsum, 11)
  expect_type(ldivsum, "list")
  expect_true(tibble::is_tibble(ldivsum))

  ldiv <- hansard_lords_divisions(division_id = 705891, summary = FALSE, tidy = TRUE, verbose = TRUE)
  expect_length(ldiv, 19)
  expect_type(ldiv, "list")
  expect_true(tibble::is_tibble(ldiv))
  expect_equal(nrow(ldiv),466)

  ldivdec <- hansard_lords_divisions(division_id = NULL, FALSE, start_date = "2016-12-01", end_date = "2016-12-31", verbose = TRUE)
  expect_length(ldivdec, 5)
  expect_type(ldivdec, "list")
  expect_true(tibble::is_tibble(ldivdec))

  # Written Questions
  lwq <- hansard_lords_written_questions(peer_id = c(3526,4176), answering_department = c('cabinet', 'Transport'), start_date = "2017-01-01", end_date = "2018-08-18", verbose=TRUE)
  expect_length(lwq, 12)
  expect_type(lwq, "list")
  expect_true(tibble::is_tibble(lwq))
  expect_equal(nrow(lwq), 49)

  # Attendance
  la <- hansard_lords_attendance(session_id = 706178, verbose = TRUE)
  expect_length(la, 8)
  expect_type(la, "list")
  expect_true(tibble::is_tibble(la))

  # lanull <- hansard_lords_attendance(start_date = "2016-03-01", verbose = TRUE)
  # expect_length(lanull, 4)
  # expect_type(lanull, "list")
  # expect_true(tibble::is_tibble(lanull))

})
