
#' Bulk Epetition data
#'
#' Get data on all epetitions submitted to parliament, with the label, sponsor,
#' number of signatures, date created and epetition ID. For greater detail on
#' indidivual epetitions, see [epetition()].
#'
#'
#' @param min_signatures The minimum number of signatures required for
#' inclusion in the tibble. Defaults to 1.
#'
#' @param max_signatures The maximum number of signatures required for
#' inclusion in the tibble. If `NULL`, there is no maximum number of
#' signatures. Defaults to `NULL`.
#'
#' @param status The status of the petition, either `'open'` or
#' `'closed'`. If `NULL`, returns all petitions both open and
#' closed. Defaults to `NULL`.
#'
#' @param start_date Only includes epetitions created on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date Only includes epetitions created on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or a
#' nything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on electronic petitions
#' submitted to parliament.
#' @seealso [epetition()]
#'
#' @export
#' @examples
#' \dontrun{
#' x <- epetition_tibble()
#' 
#' y <- epetition_tibble(max_signatures = 500)
#' 
#' z <- epetition_tibble(start_date = "2016-12-01", end_date = "2017-04-25")
#' }
#' 
epetition_tibble <- function(min_signatures = 1, max_signatures = NULL,
                             status = NULL, start_date = "1900-01-01",
                             end_date = Sys.Date(), extra_args = NULL,
                             tidy = TRUE, tidy_style = "snake_case",
                             verbose = TRUE) {
  dates <- paste0(
    "&max-created=", as.Date(end_date),
    "&min-created=", as.Date(start_date)
  )

  status_query <- ifelse(
    is.null(status) == TRUE,
    "",
    paste0("&status=", status)
  )

  signature_query <- ifelse(
    is.null(max_signatures) == TRUE,
    paste0("&min-numberOfSignatures=", min_signatures),
    paste0(
      "&min-numberOfSignatures=", min_signatures,
      "&max-numberOfSignatures=", max_signatures
    )
  )

  baseurl <- paste0(url_util, "epetitions.json?")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  petition <- jsonlite::fromJSON(paste0(
    baseurl, status_query,
    signature_query, dates,
    extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(petition$result$totalResults / 100)

  query <- paste0(
    baseurl, status_query, signature_query,
    dates, extra_args, "&_pageSize=100&_page="
  )

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- epetition_tibble_tidy(df, tidy_style) ## in utils-epetition.R
    }

    df
  }
}


#' @rdname epetition_tibble
#' @export

hansard_epetition_tibble <- epetition_tibble
