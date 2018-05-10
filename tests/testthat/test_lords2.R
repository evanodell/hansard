library(hansard)
context("lords part 2 functions")


test_that("lords functions return expected format", {
  skip_on_cran()

  # Divisions
  ldivsum <- hansard_lords_divisions(
    division_id = 705891, summary = TRUE,
    tidy = TRUE, verbose = TRUE
  )
  expect_length(ldivsum, 11)
  expect_type(ldivsum, "list")
  expect_true(tibble::is_tibble(ldivsum))

  ldiv <- hansard_lords_divisions(
    division_id = 705891, summary = FALSE,
    tidy = TRUE, verbose = TRUE
  )
  expect_length(ldiv, 19)
  expect_type(ldiv, "list")
  expect_true(tibble::is_tibble(ldiv))
  expect_equal(nrow(ldiv), 466)

  ldivdec <- hansard_lords_divisions(
    division_id = NULL, FALSE,
    start_date = "2016-12-01",
    end_date = "2016-12-31", verbose = TRUE
  )
  expect_length(ldivdec, 5)
  expect_type(ldivdec, "list")
  expect_true(tibble::is_tibble(ldivdec))

  # Written Questions
  lwq <- hansard_lords_written_questions(
    peer_id = c(3526, 4176), answering_department = c("cabinet", "Transport"),
    start_date = "2017-01-01", end_date = "2017-08-19", verbose = TRUE
  )
  expect_length(lwq, 12)
  expect_type(lwq, "list")
  expect_true(tibble::is_tibble(lwq))
  expect_equal(nrow(lwq), 48)

  # Attendance
  lasess <- hansard_lords_attendance_session(
    session_id = 706178,
    verbose = TRUE
  )
  expect_length(lasess, 8)
  expect_type(lasess, "list")
  expect_true(tibble::is_tibble(lasess))

  ladate <- hansard_lords_attendance_date(
    date = "2016-03-01",
    verbose = TRUE
  )
  expect_length(ladate, 6)
  expect_type(ladate, "list")
  expect_true(tibble::is_tibble(ladate))


  lord_sessions <- lords_sessions(
    start_date = "2017-01-01",
    end_date = "2017-01-31"
  )
  expect_length(lord_sessions, 4)
  expect_type(lord_sessions, "list")
  expect_true(tibble::is_tibble(lord_sessions))
  expect_true(nrow(lord_sessions) == 15)
})
