


# all_answered_questions_multiple -----------------------------------------

aaq_multi <- function(mp_id, tabling_mp_id, house, answering_body,
                      start_date, end_date, extra_args, verbose) {
  if (is.null(mp_id) == TRUE) {
    mp_id_list <- NA
  } else {
    mp_id_list <- as.list(mp_id)
  }

  if (is.null(tabling_mp_id) == TRUE) {
    tabling_mp_id_list <- NA
  } else {
    tabling_mp_id_list <- as.list(tabling_mp_id)
  }

  if (is.null(answering_body) == TRUE) {
    answering_body_list <- NA
  } else {
    answering_body_list <- as.list(answering_body)
  }

  search_grid <- expand.grid(mp_id_list, tabling_mp_id_list,
    answering_body_list,
    stringsAsFactors = FALSE
  )

  names(search_grid)[names(search_grid) == "Var1"] <- "answering_mp"
  names(search_grid)[names(search_grid) == "Var2"] <- "tabling_mp"
  names(search_grid)[names(search_grid) == "Var3"] <- "department"

  # search_grid

  dat <- vector("list", nrow(search_grid))

  seq_list <- seq(from = 1, to = nrow(search_grid), by = 1)

  for (i in seq_along(seq_list)) {
    dat[[i]] <- hansard::all_answered_questions(
      mp_id = search_grid$answering_mp[[i]],
      tabling_mp_id = search_grid$tabling_mp[[i]],
      house = house,
      answering_body = search_grid$department[[i]],
      end_date = end_date,
      start_date = start_date,
      extra_args = extra_args,
      tidy = FALSE,
      verbose = verbose
    )
  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df
}



# all_answered_questions tidying -----------------------------------------
aaq_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    names(df)[names(df) == "_about"] <- "about"

    df$answer._about <- NULL

    names(df) <- gsub("^answer\\.", "", names(df), perl = TRUE)

    df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

    if ("groupedQuestionUIN" %in% colnames(df)) {
      if ("groupedQuestionUIN._value" %in% colnames(df)) {
        df$groupedQuestionUIN <- ifelse(df$groupedQuestionUIN == "NULL",
          df$groupedQuestionUIN._value, df$groupedQuestionUIN
        )

        df$groupedQuestionUIN._value <- NULL
      }

      df$groupedQuestionUIN <- gsub(
        "`_value` = ", "",
        df$groupedQuestionUIN
      )

      df$groupedQuestionUIN <- as.list(df$groupedQuestionUIN)
    }

    df$answeringMember._about <- gsub(
      "http://data.parliament.uk/members/",
      "",
      df$answeringMember._about
    )

    df$tablingMember._about <- gsub(
      "http://data.parliament.uk/members/",
      "",
      df$tablingMember._about
    )

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$questionFirstAnswered <- df$questionFirstAnswered

    df <- tidyr::unnest_(df, "questionFirstAnswered")

    names(df)[names(df) == "_value"] <- "answerDateTime"

    df$`_datatype` <- NULL

    df <- move_me(df, c("answerDateTime"), "after", "dateOfAnswer._value")

    df$answerDateTime <- gsub("T", " ", df$answerDateTime)

    df$answerDateTime <- as.POSIXct(
      lubridate::parse_date_time(
        df$answerDateTime,
        "Y-m-d H:M:S"
      )
    )

    df$dateOfAnswer._datatype <- "POSIXct"

    if ("attachment" %in% colnames(df)) {
      seq_list <- seq(from = 1, to = nrow(df), by = 1)

      for (i in seq_along(seq_list)) {
        if (is.null(names(df$attachment[[i]])) == FALSE) {
          names(df$attachment[[i]])[names(df$attachment[[i]]) ==
            "_about"] <- "about"

          names(df$attachment[[i]])[names(df$attachment[[i]]) ==
            "fileName._value"] <- "fileName"
        }
      }
    }

    df$legislature <- dplyr::bind_rows(df$legislature) # get legislature info

    df$legislature.prefLabel._value <- df$legislature$prefLabel._value

    df$legislature_about <- df$legislature$`_about`

    df$legislature_about <- gsub(
      "http://data.parliament.uk/terms/",
      "", df$legislature_about
    )

    df$legislature <- NULL # delete now superfluous column
  }

  df <- hansard_tidy(df, tidy_style)

  df
}



# Function to rearrange columns -------------------------------------------

## Courtesy of A5C1D2H2I1M1N2O1R2T1 on StackOverflow, question 18339370

move_me <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)

  x <- switch(where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp) - 1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    }
  )
  x
}
