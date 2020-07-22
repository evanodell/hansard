

#' Individual MP voting records
#'
#' Accepts an ID number for a member of the House of Commons, and returns a
#' tibble of their votes.
#'
#'
#' @param mp_id The ID number of a member of the House of Commons.
#'
#' @param lobby Accepts one of `'all'`, `'aye'` or `'no'`.
#' `'aye'` returns votes where the MP voted `'aye'`, `'no'`
#' returns votes where the MP voted `'no'`, `'all'` returns all
#' available votes by the MP. This parameter is not case sensitive.
#' Defaults to `'all'`.
#'
#' @param session The parliamentary session to return votes from, in
#' `'YYYY/YY'` format. Defaults to `NULL`.
#'
#' @param start_date Only includes divisions on or after this date. Accepts
#' character values in `'YYYY-MM-DD'` format, and objects of class
#' `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything
#' else that can be coerced to a date with `as.Date()`. Defaults to
#' `'1900-01-01'`.
#'
#' @param end_date Only includes divisions on or before this date. Accepts
#' character values in `'YYYY-MM-DD'` format, and objects of class
#' `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything
#' else that can be coerced to a date with `as.Date()`. Defaults to
#' the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the voting record of the given MP.
#' @export
#' @examples
#' \dontrun{
#' x <- mp_vote_record(172, lobby = "all")
#'
#' x <- mp_vote_record(172, lobby = "aye")
#'
#' x <- mp_vote_record(172, lobby = "no")
#'
#' x <- mp_vote_record(172, session = "2016/17")
#' }
#'
mp_vote_record <- function(mp_id = NULL, lobby = "all", session = NULL,
                           start_date = "1900-01-01", end_date = Sys.Date(),
                           extra_args = NULL, tidy = TRUE,
                           tidy_style = "snake", verbose = TRUE) {
  if (is.null(mp_id)) {
    stop("mp_id must not be empty", call. = FALSE)
  }

  if (!is.null(extra_args)) {
    extra_args <- utils::URLencode(extra_args)
  }

  if (!is.null(session)) {
    session_query <- paste0("&session=", session)
  } else {
    session_query <- ""
  }

  lobby <- tolower(lobby)

  dates <- paste0(
    "&_properties=date&max-date=",
    as.Date(end_date), "&min-date=",
    as.Date(start_date)
  )

  if (lobby == "aye") {
    baseurl <- paste0(url_util, "commonsdivisions/aye.json?mnisId=")

    veb(verbose)

    query <- paste0(baseurl, mp_id, dates, session_query, extra_args)

    url_aye <- jsonlite::fromJSON(paste0(
      query, "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(url_aye$result$totalResults / 100)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  } else if (lobby == "no") {
    baseurl <- paste0(url_util, "commonsdivisions/no.json?mnisId=")

    veb(verbose)

    query <- paste0(baseurl, mp_id, dates, session_query, extra_args)

    url_no <- jsonlite::fromJSON(paste0(
      query, "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(url_no$result$totalResults / 100)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  } else {
    if (verbose == TRUE) {
      message("Retrieving aye votes:")
    }

    df_aye <- mp_vote_record(
      mp_id = mp_id, lobby = "aye",
      session = session, start_date = start_date,
      end_date = end_date, extra_args = extra_args,
      tidy = FALSE, tidy_style = tidy_style,
      verbose = verbose
    )

    df_aye$vote <- "aye"

    if (verbose == TRUE) {
      message("Retrieving no votes:")
    }

    df_no <- mp_vote_record(
      mp_id = mp_id, lobby = "no",
      session = session, start_date = start_date,
      end_date = end_date, extra_args = extra_args,
      tidy = FALSE, tidy_style = tidy_style,
      verbose = verbose
    )

    df_no$divisionNumber <- NULL

    df_no$vote <- "no"

    df <- dplyr::bind_rows(df_aye, df_no)

    if (tidy == TRUE) {
      df$vote <- as.factor(df$vote)
    }
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df$date._datatype <- "POSIXct"

      df$date._value <- as.POSIXct(df$date._value)

      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname mp_vote_record
#' @export
hansard_mp_vote_record <- mp_vote_record
