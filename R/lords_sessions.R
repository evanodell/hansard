

#' Lords sessions
#'
#' Returns the session code and other basic details for individual
#' House of Lords sittings. Note that this API does not appear to have been
#' updated with data after 2017-01-31.
#'
#'
#' @param start_date Only includes sessions starting on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date Only includes sessions ending on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' @seealso [lords_attendance_session()]
#' @seealso [sessions_info()]
#'
#' @inheritParams all_answered_questions
#' @export
#' @examples
#' \dontrun{
#'
#' a <- lords_sessions(start_date = "2017-01-01", end_date = "2017-01-31")
#' }
#'
lords_sessions <- function(start_date = "1900-01-01", end_date = Sys.Date(),
                           tidy = TRUE, tidy_style = "snake",
                           verbose = TRUE) {
  baseurl <- paste0(url_util, "lordsattendances.json?")

  dates <- paste0(
    "&min-date=", as.Date(start_date),
    "&max-date=", as.Date(end_date)
  )

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  attend <- jsonlite::fromJSON(paste0(
    baseurl, dates,
    "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(attend$result$totalResults / 100)

  query <- paste0(baseurl, dates)

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- lords_attendance_tidy(df, tidy_style)
    }

    df
  }
}



#' @rdname lords_sessions
#' @export
hansard_lords_sessions <- lords_sessions
