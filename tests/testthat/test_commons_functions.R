
library(hansard)
context("commons functions")

test_that("commons functions return expected format", {
    skip_on_cran()
 skip_on_travis()

    xcaq <- commons_answered_questions(answering_department = "health", answered_by = "4019", start_date = "2017-02-01")
    expect_length(xcaq, 10)
    expect_type(xcaq, "list")
    expect_true(tibble::is_tibble(xcaq))

    # Divisions

    xcd <- commons_divisions(division_id = 694163, summary = FALSE)
    expect_length(xcd, 5)
    expect_type(xcd, "list")
    expect_true(tibble::is_tibble(xcd))

    xcds <- commons_divisions(division_id = 694163, summary = TRUE)
    expect_length(xcds, 17)
    expect_type(xcds, "list")
    expect_true(tibble::is_tibble(xcds))

    xcdall <- commons_divisions(start_date = "2017-02-01", end_date = "2017-03-01")
    expect_length(xcdall, 5)
    expect_type(xcdall, "list")
    expect_true(tibble::is_tibble(xcdall))

    # Divisions by Date
    cdd <- commons_division_date("2017-02-24")
    expect_length(cdd, 5)
    expect_type(cdd, "list")
    expect_true(tibble::is_tibble(cdd))
    expect_equal(nrow(cdd), 5)

    xcoqt <- commons_oral_question_times(session = "2016/17", question_id = "685697")
    expect_length(xcoqt, 14)
    expect_type(xcoqt, "list")
    expect_true(tibble::is_tibble(xcoqt))

    xcoqs <- commons_oral_question_times(session = "2015/16")
    expect_length(xcoqs, 14)
    expect_type(xcoqs, "list")
    expect_true(tibble::is_tibble(xcoqs))
    expect_equal(nrow(xcoqs), 313)

    xcoqe <- commons_oral_questions(mp_id = 4019, answering_department = "education", tidy = FALSE)
    expect_length(xcoqe, 24)
    expect_type(xcoqe, "list")
    expect_true(tibble::is_tibble(xcoqe))

    xcwq <- commons_written_questions(mp_id = 410, "cabinet office")
    expect_length(xcwq, 12)
    expect_type(xcwq, "list")
    expect_true(tibble::is_tibble(xcwq))

    xcte <- commons_terms(search = "estate")
    expect_length(xcte, 20)
    expect_type(xcte, "list")
    expect_true(tibble::is_tibble(xcte))

    xctec <- commons_terms(search = "estate", class = "ORG")
    expect_length(xctec, 19)
    expect_type(xctec, "list")
    expect_true(tibble::is_tibble(xctec))

})
