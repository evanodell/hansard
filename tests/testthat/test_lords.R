library(hansard)
context("lords functions")


test_that("lords functions return expected format", {
    skip_on_cran()
    
    # Voting Records
    
    lvotesall <- lord_vote_record(530, lobby = "all", start_date = "2017-01-01")
    expect_length(lvotesall, 11)
    expect_type(lvotesall, "list")
    expect_true(tibble::is_tibble(lvotesall))
    
    lvotesnot <- lord_vote_record(530, lobby = "notcontent", start_date = "2017-01-01")
    expect_length(lvotesnot, 10)
    expect_type(lvotesnot, "list")
    expect_true(tibble::is_tibble(lvotesnot))
    
    lvotescont <- lord_vote_record(530, lobby = "content", start_date = "2017-01-01")
    expect_length(lvotescont, 10)
    expect_type(lvotescont, "list")
    expect_true(tibble::is_tibble(lvotescont))
    
    # Amendments
    lamend <- lords_amendments(decision = "Agreed", start_date = "2017-03-17")
    expect_length(lamend, 31)
    expect_type(lamend, "list")
    expect_true(tibble::is_tibble(lamend))
    
    lamendnu <- lords_amendments(decision = NULL, start_date = "2017-03-17", end_date = "2017-03-20")
    expect_length(lamendnu, 29)
    expect_type(lamendnu, "list")
    expect_true(tibble::is_tibble(lamendnu))
    
    # Divisions
    ldivsum <- lords_divisions(division_id = 705891, summary = TRUE, tidy = FALSE)
    expect_length(ldivsum, 11)
    expect_type(ldivsum, "list")
    expect_true(tibble::is_tibble(ldivsum))
    
    ldiv <- lords_divisions(division_id = 705891, summary = FALSE, tidy = FALSE)
    expect_length(ldiv, 6)
    expect_type(ldiv, "list")
    expect_true(tibble::is_tibble(ldiv))
    
    ldivdec <- lords_divisions(division_id = NULL, FALSE, start_date = "2016-12-01", end_date = "2016-12-31")
    expect_length(ldivdec, 5)
    expect_type(ldivdec, "list")
    expect_true(tibble::is_tibble(ldivdec))
    
    # Written Questions
    lwq <- lords_written_questions(peer_id = 3526, answering_department = "cabinet")
    expect_length(lwq, 12)
    expect_type(lwq, "list")
    expect_true(tibble::is_tibble(lwq))
    
    # Attendance
    la <- lords_attendance(session_id = 706178)
    expect_length(la, 8)
    expect_type(la, "list")
    expect_true(tibble::is_tibble(la))
    
    lanull <- lords_attendance(start_date = "2016-03-01")
    expect_length(lanull, 4)
    expect_type(lanull, "list")
    expect_true(tibble::is_tibble(lanull))
    
})

