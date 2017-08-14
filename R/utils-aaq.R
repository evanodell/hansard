


# all_answered_questions_multiple -----------------------------------------



aaq_multi <- function(mp_id, tabling_mp_id, house, answering_body, start_date, end_date, extra_args) {

  mp_id_list <- as.list(mp_id)

  dat <- vector("list", length(mp_id_list))

  for (i in 1:length(mp_id_list)) {

    dat[[i]] <- hansard::all_answered_questions(mp_id = mp_id_list[[i]], tabling_mp_id=tabling_mp_id, house=house, answering_body=answering_body, end_date = end_date, start_date = start_date, extra_args = extra_args, tidy = FALSE)

  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df

}
