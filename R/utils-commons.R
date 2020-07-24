
# commons_oral_questions_multi -----------------------------------------

commons_oral_questions_multi <- function(mp_id, answering_department,
                                         start_date, end_date,
                                         extra_args, verbose) {
  if (is.null(mp_id)) {
    mp_id_list <- NA
  } else {
    mp_id_list <- as.list(mp_id)
  }

  if (is.null(answering_department)) {
    dep_list <- NA
  } else {
    dep_list <- as.list(answering_department)
  }

  search_grid <- expand.grid(dep_list, mp_id_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid) == "Var1"] <- "department"
  names(search_grid)[names(search_grid) == "Var2"] <- "member"

  dat <- vector("list", nrow(search_grid))

  seq_list <- seq(from = 1, to = nrow(search_grid), by = 1)

  for (i in seq_along(seq_list)) {
    dat[[i]] <- hansard::commons_oral_questions(
      mp_id = search_grid$member[[i]],
      answering_department = search_grid$department[[i]],
      end_date = end_date,
      start_date = start_date,
      extra_args = extra_args,
      verbose = verbose,
      tidy = FALSE
    )
  }

  dat <- dat[sapply(dat, function(d) !is.null(d))]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df
}



# commons_written_questions_multi -----------------------------------------

commons_written_questions_multi <- function(mp_id, answering_department,
                                            start_date, end_date,
                                            extra_args, verbose) {
  if (is.null(mp_id)) {
    mp_id_list <- NA
  } else {
    mp_id_list <- as.list(mp_id)
  }

  if (is.null(answering_department)) {
    dep_list <- NA
  } else {
    dep_list <- as.list(answering_department)
  }

  search_grid <- expand.grid(dep_list, mp_id_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid) == "Var1"] <- "department"
  names(search_grid)[names(search_grid) == "Var2"] <- "member"

  dat <- vector("list", nrow(search_grid))

  seq_list <- seq(from = 1, to = nrow(search_grid), by = 1)

  for (i in seq_along(seq_list)) {
    dat[[i]] <- hansard::commons_written_questions(
      mp_id = search_grid$member[[i]],
      answering_department = search_grid$department[[i]],
      end_date = end_date,
      start_date = start_date,
      extra_args = extra_args,
      verbose = verbose,
      tidy = FALSE
    )
  }

  dat <- dat[sapply(dat, function(d) !is.null(d))]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df
}



# commons_answered_questions_multiple -----------------------------------------

caq_multi <- function(answering_department, answered_by,
                      start_date, end_date, extra_args, verbose) {
  if (is.null(answered_by)) {
    mp_id_list <- NA
  } else {
    mp_id_list <- as.list(answered_by)
  }

  if (is.null(answering_department)) {
    dep_list <- NA
  } else {
    dep_list <- as.list(answering_department)
  }

  search_grid <- expand.grid(dep_list, mp_id_list, stringsAsFactors = FALSE)

  names(search_grid)[names(search_grid) == "Var1"] <- "department"
  names(search_grid)[names(search_grid) == "Var2"] <- "member"

  dat <- vector("list", nrow(search_grid))

  seq_list <- seq(from = 1, to = nrow(search_grid), by = 1)

  for (i in seq_along(seq_list)) {
    dat[[i]] <- hansard::commons_answered_questions(
      answering_department = search_grid$department[[i]],
      answered_by = search_grid$member[[i]],
      end_date = end_date,
      start_date = start_date,
      extra_args = extra_args,
      verbose = verbose,
      tidy = FALSE
    )
  }

  dat <- dat[sapply(dat, function(d) !is.null(d))]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df
}



# commons_answered_questions_multiple_tidy -----------------------------------------

caq_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$dateOfAnswer._value <- as.POSIXct(df$dateOfAnswer._value)

    df$dateOfAnswer._datatype <- "POSIXct"

    df$answeringMember._about <- gsub(
      "http://data.parliament.uk/members/", "", df$answeringMember._about
    )

    df$AnsweringBody <- unlist(df$AnsweringBody)
  }

  df <- hansard_tidy(df, tidy_style)

  df
}


# commons divisions date -----------------------------------------
# First tidying function

cdd_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)

  df
}



# commons_divs tidying -----------------------------------------



cd_tidy <- function(df, tidy_style, summary) {
  if (summary) {
    df$type <- gsub("http://data.parliament.uk/schema/parl#", "", df$type)

    df$type <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", df$type)
  } else {
    df$`_about` <- gsub("http://data.parliament.uk/resources/", "", df$`_about`)

    df$voteId <- gsub("/.*$", "", df$`_about`)

    df$`_about` <- gsub("^.*/*/", "", df$`_about`)

    names(df)[names(df) == "_about"] <- "number"

    df <- tidyr::unnest(df, cols = "member")

    df$type <- gsub(
      "http://data.parliament.uk/schema/parl#", "",
      df$type
    )

    df$type <- gsub(
      "([[:lower:]])([[:upper:]])", "\\1_\\2",
      df$type
    )
  }

  names(df) <- snakecase::to_any_case(names(df), case = tidy_style)

  df
}

## commons_written_questions utilities -----------------------------------------


cwq_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

    df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

    df$dateTabled._datatype <- "POSIXct"

    df$AnswerDate._datatype <- "POSIXct"

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMember._about <- gsub(
      "http://data.parliament.uk/members/", "", df$tablingMember._about
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}



## commons_oral_questions_times utilities -----------------------------------------


coqt_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$session <- as.character(df$session)

    df <- tidyr::unnest_wider(df, "AnswerBody", "_")

    df$AnswerDateTime._value <- gsub("T", " ", df$AnswerDateTime._value)

    df$AnswerDateTime._value <- lubridate::parse_date_time(
      df$AnswerDateTime._value, "Y-m-d H:M:S"
    )

    df$AnswerDateTime._datatype <- "POSIXct"

    df$date._value <- gsub("T", " ", df$date._value)

    df$date._value <- lubridate::parse_date_time(
      df$date._value, "Y-m-d H:M:S"
    )

    df$date._datatype <- "POSIXct"

    df$modified._value <- gsub("T", " ", df$modified._value)

    df$modified._value <- lubridate::parse_date_time(
      df$modified._value, "Y-m-d H:M:S"
    )

    df$modified._datatype <- "POSIXct"

    df$`_about` <- gsub(
      "http://data.parliament.uk/resources/", "",
      df$`_about`
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}


## commons_oral_questions utilities -----------------------------------------

coq_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$AnswerDateTime._value <- gsub("T", " ", df$AnswerDateTime._value)

    df$AnswerDateTime._value <- lubridate::parse_date_time(
      df$AnswerDateTime._value, "Y-m-d H:M:S"
    )

    df$AnswerDateTime._datatype <- "POSIXct"

    df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

    df$AnswerDate._datatype <- "POSIXct"

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMember._about <- gsub(
      "http://data.parliament.uk/members/", "", df$tablingMember._about
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
