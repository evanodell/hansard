
library(hansard)
context("commons functions")

test_that("commons functions return expected format", {
    skip_on_cran()

    xcaq <- hansard_commons_answered_questions(answering_department = "health", answered_by = "4019", start_date = "2017-02-01", end_date = "2017-02-02", verbose=TRUE)
    expect_length(xcaq, 10)
    expect_type(xcaq, "list")
    expect_true(tibble::is_tibble(xcaq))

    # Divisions

    xcd <- hansard_commons_divisions(division_id = 694163, summary = FALSE)
    expect_length(xcd, 6)
    expect_type(xcd, "list")
    expect_true(tibble::is_tibble(xcd))

    xcds <- hansard_commons_divisions(division_id = 694163, summary = TRUE, verbose=TRUE)
    expect_length(xcds, 17)
    expect_type(xcds, "list")
    expect_true(tibble::is_tibble(xcds))

    xcdall <- hansard_commons_divisions(start_date = "2017-02-01", end_date = "2017-03-01")
    expect_length(xcdall, 5)
    expect_type(xcdall, "list")
    expect_true(tibble::is_tibble(xcdall))

    # Divisions by Date
    cdd <- commons_division_date("2017-02-24")
    expect_length(cdd, 5)
    expect_type(cdd, "list")
    expect_true(tibble::is_tibble(cdd))
    expect_equal(nrow(cdd), 5)

    ## Questions
    xcoqt <- hansard_commons_oral_question_times(session = "2016/17", question_id = "685697", verbose=TRUE)
    expect_length(xcoqt, 14)
    expect_type(xcoqt, "list")
    expect_true(tibble::is_tibble(xcoqt))

    xcoqs <- hansard_commons_oral_question_times(session = "2015/16")
    expect_length(xcoqs, 14)
    expect_type(xcoqs, "list")
    expect_true(tibble::is_tibble(xcoqs))
    expect_equal(nrow(xcoqs), 313)

    xcoqe <- hansard_commons_oral_questions(mp_id = 4019, answering_department = "education", tidy = FALSE)
    expect_length(xcoqe, 24)
    expect_type(xcoqe, "list")
    expect_true(tibble::is_tibble(xcoqe))

    xcoqtidy <- hansard_commons_oral_questions(mp_id = 4019, answering_department = "education", tidy = TRUE, tidy_style = "period.case", verbose=TRUE)
    expect_length(xcoqtidy, 24)
    expect_type(xcoqtidy, "list")
    expect_true(tibble::is_tibble(xcoqtidy))

    xcoqe <- hansard_tidy(xcoqe, tidy_style = "period.case")
    expect_true(xcoqe[[1]][[1]]==xcoqtidy[[1]][[1]])

    xcwq <- hansard_commons_written_questions(mp_id = 410, "cabinet office", verbose=TRUE)
    expect_length(xcwq, 12)
    expect_type(xcwq, "list")
    expect_true(tibble::is_tibble(xcwq))

    # Commons Terms
    xcte <- hansard_commons_terms(search = "estate")
    expect_length(xcte, 20)
    expect_type(xcte, "list")
    expect_true(tibble::is_tibble(xcte))

    xctec <- hansard_commons_terms(search = "estate", class = "ORG", verbose=TRUE)
    expect_length(xctec, 19)
    expect_type(xctec, "list")
    expect_true(tibble::is_tibble(xctec))

})
