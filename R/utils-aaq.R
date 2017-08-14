


# all_answered_questions_multiple -----------------------------------------



aaq_multi <- function(mp_id, tabling_mp_id, house, answering_body, start_date, end_date, extra_args) {

  mp_id_list <- as.list(mp_id)

  dat <- lapply(mp_id_list, all_answered_questions)

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}
