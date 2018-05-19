
# lords_written_questions_multiple -----------------------------------------
# lords Written questions multiple function

lwq_multi <- function(answering_department, peer_id,
                      start_date, end_date, extra_args, verbose) {
  if (is.null(peer_id) == TRUE) {
    mp_id_list <- NA
  } else {
    mp_id_list <- as.list(peer_id)
  }

  if (is.null(answering_department) == TRUE) {
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
    dat[[i]] <- hansard::lords_written_questions(
      answering_department = search_grid$department[[i]],
      peer_id = search_grid$member[[i]],
      end_date = end_date,
      start_date = start_date,
      extra_args = extra_args,
      verbose = verbose,
      tidy = FALSE
    )
  }

  dat <- dat[sapply(dat, function(d) is.null(d) == FALSE)]

  df <- dplyr::bind_rows(dat)

  names(df)[names(df) == "_about"] <- "about"

  df
}


# lords Written questions tidy function
# ------------------------------------------- Tidy up Lords Written Questions

lwq_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$dateTabled._value <- as.POSIXct(df$dateTabled._value)

    df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)

    df$dateTabled._datatype <- "POSIXct"

    df$AnswerDate._value <- "POSIXct"

    df$AnsweringBody <- unlist(df$AnsweringBody)

    df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)

    df$tablingMember._about <- gsub(
      "http://data.parliament.uk/members/", "",
      df$tablingMember._about
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}



# lords divisions tidy function -------------------------------------------

lords_division_tidy <- function(df, division_id, summary, tidy_style) {
  if (nrow(df) > 0) {
    if (is.null(division_id) == TRUE) {
      df$date._datatype <- "POSIXct"

      df$date._value <- as.POSIXct(df$date._value)
    } else {
      if (summary == FALSE) {
        df <- ldsum_tidy(df, tidy_style)
      }
    }
  }

  df <- hansard_tidy(df, tidy_style)

  df
}


# lords division summary tidy function
# -------------------------------------------

ldsum_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

    df$vote.type <- gsub(
      "http://data.parliament.uk/schema/parl#", "",
      df$vote.type
    )

    df$vote.type <- gsub(
      "([[:lower:]])([[:upper:]])", "$1_$2",
      df$vote.type
    )

    df$vote.member <- unlist(df$vote.member)

    df$vote.member <- gsub(
      "http://data.parliament.uk/resources/members/api/lords/id/", "",
      df$vote.member
    )

    if (tidy_style == "camelCase") {
      df$vote.type <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2",
        df$vote.type,
        perl = TRUE
      )

      substr(df$vote.type, 1, 1) <- tolower(substr(df$vote.type, 1, 1))
    } else if (tidy_style == "period.case") {
      df$vote.type <- gsub("_", ".", df$vote.type)

      df$vote.type <- tolower(df$vote.type)
    } else {
      df$vote.type <- tolower(df$vote.type)
    }
  }

  df
}

# lords vote record tidy function -------------------------------------------


lord_vote_record_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$vote <- as.factor(df$vote)

    df$date._datatype <- as.factor(df$date._datatype)

    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)

  df
}

# lords amendments tidy function -------------------------------------------

lords_amendments_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$bill.date._value <- as.POSIXct(df$bill.date._value)

    df$bill.date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)

  df
}

# lords attendance tidy function -------------------------------------------

lords_attendance_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)

  df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

  df
}


# lords_interests_tidy2 ---------------------------------------------------- For
# single peers data


lords_interests_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    if ("amendedDate" %in% colnames(df)) {
      seq_list <- seq(from = 1, to = nrow(df), by = 1)

      for (i in seq_along(seq_list)) {
        if (is.null(df$amendedDate[[i]]) == FALSE) {
          df$amendedDate[[i]] <- df$amendedDate[[i]][["_value"]]
        }
      }

      df$amendedDate[df$amendedDate == "NULL"] <- NA

      df$amendedDate <- do.call("c", df$amendedDate)

      df$amendedDate <- as.POSIXct(df$amendedDate)
    }

    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"

    df$registeredLate._value <- as.logical(df$registeredLate._value)

    df$registeredLate._datatype <- "Logical"

    names(df)[names(df) == "X_about"] <- "registeredInterestNumber"

    names(df)[names(df) == "_about"] <- "registeredInterestNumber"

    df$registeredInterestNumber <- gsub(
      ".*registeredinterest/", "",
      df$registeredInterestNumber
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}

# lords_interests_tidy2 ----------------------------------------------------
# For all peers data

lords_interests_tidy2 <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    seq_list <- seq(from = 1, to = nrow(df), by = 1)

    for (i in seq_along(seq_list)) {
      df[i, ]$hasRegisteredInterest[[1]] <- as.data.frame(
        df[i, ]$hasRegisteredInterest
      )
    }

    seq_list <- seq(from = 1, to = nrow(df), by = 1)

    for (i in seq_along(seq_list)) {
      if ("amendedDate" %in% colnames(df[i, ]$hasRegisteredInterest[[1]])) {
        # as not all entries have been amended

        seq_list <- seq(from = 1, to = nrow(df), by = 1)

        for (i in seq_along(seq_list)) {
          # because apply wasn't working with all the NULL values

          if (is.null(
            df[i, ]$hasRegisteredInterest[[1]]$amendedDate[[x]]) == FALSE) {
            ## If there is an amended date value, extract it

            df[i, ]$hasRegisteredInterest[[1]]$amendedDate[[x]] <-
              df[i, ]$hasRegisteredInterest[[1]]$amendedDate[[x]][["_value"]]
          }
        }

        df[i, ]$
          hasRegisteredInterest[[1]]$
          amendedDate[df[i, ]$
          hasRegisteredInterest[[1]]$
          amendedDate == "NULL"] <- NA
        # Change the NULL values to NA to be more R like

        df[i, ]$hasRegisteredInterest[[1]]$amendedDate <- do.call(
          "c", df[i, ]$hasRegisteredInterest[[1]]$amendedDate
        )
        ## Extract dates from list into 'normal' vector column

        df[i, ]$hasRegisteredInterest[[1]]$amendedDate <- as.POSIXct(
          df[i, ]$hasRegisteredInterest[[1]]$amendedDate
        )
      } #

      df[i, ]$hasRegisteredInterest[[1]]$date._value <- as.POSIXct(
        df[i, ]$hasRegisteredInterest[[1]]$date._value
      )

      df[i, ]$hasRegisteredInterest[[1]]$date._datatype <- "POSIXct"

      df[i, ]$hasRegisteredInterest[[1]]$registeredLate._value <- as.logical(
        df[i, ]$hasRegisteredInterest[[1]]$registeredLate._value
      )

      df[i, ]$hasRegisteredInterest[[1]]$registeredLate._datatype <- "Logical"

      names(df[i, ]$hasRegisteredInterest[[1]])[
        names(df[i, ]$hasRegisteredInterest[[1]]) == "X_about"
      ] <-
        "registeredInterestNumber"

      names(df[i, ]$hasRegisteredInterest[[1]])[names(
        df[i, ]$hasRegisteredInterest[[1]]
      ) == "_about"] <-
        "registeredInterestNumber"

      df[i, ]$hasRegisteredInterest[[1]]$registeredInterestNumber <-
        gsub(
          ".*registeredinterest/", "",
          df[i, ]$hasRegisteredInterest[[1]]$registeredInterestNumber
        )

      df[i, ]$hasRegisteredInterest[[1]] <- hansard_tidy(
        df[i, ]$hasRegisteredInterest[[1]], tidy_style
      )
    }
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
