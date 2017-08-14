library(hansard)
context("lords part 2 functions")


test_that("lords functions return expected format", {

  skip_on_cran()

  # Divisions
  ldivsum <- hansard_lords_divisions(division_id = 705891, summary = TRUE, tidy = TRUE)
  expect_length(ldivsum, 11)
  expect_type(ldivsum, "list")
  expect_true(tibble::is_tibble(ldivsum))

  ldiv <- hansard_lords_divisions(division_id = 705891, summary = FALSE, tidy = FALSE)
  expect_length(ldiv, 19)
  expect_type(ldiv, "list")
  expect_true(tibble::is_tibble(ldiv))

  ldivdec <- hansard_lords_divisions(division_id = NULL, FALSE, start_date = "2016-12-01", end_date = "2016-12-31")
  expect_length(ldivdec, 5)
  expect_type(ldivdec, "list")
  expect_true(tibble::is_tibble(ldivdec))

  # Written Questions
  lwq <- hansard_lords_written_questions(peer_id = 3526, answering_department = "cabinet")
  expect_length(lwq, 12)
  expect_type(lwq, "list")
  expect_true(tibble::is_tibble(lwq))

  # Attendance
  la <- hansard_lords_attendance(session_id = 706178)
  expect_length(la, 8)
  expect_type(la, "list")
  expect_true(tibble::is_tibble(la))

  lanull <- hansard_lords_attendance(start_date = "2016-03-01")
  expect_length(lanull, 4)
  expect_type(lanull, "list")
  expect_true(tibble::is_tibble(lanull))

})
