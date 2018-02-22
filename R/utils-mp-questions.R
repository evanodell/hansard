
# mp_question multi function ----------------------------------------------

mp_question_multi <- function(mp_id, question_type, start_date,
                              end_date, extra_args,  verbose) {

    mp_id_list <- as.list(mp_id)

    dat <- vector("list", length(mp_id_list))

    seq_list <- seq(from = 1, to = length(mp_id_list), by = 1)

    for (i in seq_along(seq_list)) {

        dat[[i]] <- hansard::mp_questions(mp_id = mp_id_list[[i]],
                                          question_type = question_type,
                                          end_date = end_date,
                                          start_date = start_date,
                                          extra_args = extra_args,
                                          verbose = verbose,
                                          tidy = FALSE,
                                          tidy_style = "snake_case")

    }

    dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

    df <- dplyr::bind_rows(dat)

    names(df)[names(df) == "_about"] <- "about"

    df

}

## MP question tidying

mp_question_tidy <- function(df, tidy_style) {

    if (nrow(df) > 0) {

        df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

        df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

        df$AnswerDate._datatype <- "POSIXct"

        df$dateTabled._datatype <- "POSIXct"

        df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

        df$AnsweringBody <- unlist(df$AnsweringBody)

        df$tablingMember._about <- stringi::stri_replace_all_fixed(
          df$tablingMember._about,
          "http://data.parliament.uk/members/", "",
          vectorize_all = FALSE)

    }

    df <- hansard_tidy(df, tidy_style)

    df

}



