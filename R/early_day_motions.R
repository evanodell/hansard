

#' Early day motion data
#'
#' Return data on the content, signatories, and sponsors of early day
#' motions (EDMS).
#'
#' Early Day Motion IDs reset for each parliamentary session, so not including
#' a query for \code{session} but including an \code{edm_id} will return
#' multiple early day motions with the same ID code from different
#' parliamentary sessions.
#'
#'
#' @param edm_id Accepts the ID number of an early day motion, and returns
#' data on that motion. If \code{NULL}, returns all available Early Day
#' Motions. Note that there, are as of 2017-06-15, 43,330 early day motions
#' on listed in the API, so requesting all early day motions without other
#' parameters is slow and very demanding on the API itself.
#' Defaults to \code{NULL}.
#'
#' @param session Accepts a parliamentary session, in \code{'yyyy/yy'} format.
#' Defaults to \code{NULL}.
#'
#' @param start_date Only includes early day motions tabled on or after
#' this date. Accepts character values in \code{'YYYY-MM-DD'} format, and
#' objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#' \code{POSIXlt} or anything else that can be coerced to a date with
#' \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#'
#' @param end_date Only includes early day motions tabled on or before
#' this date. Accepts character values in \code{'YYYY-MM-DD'} format,
#' and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#' \code{POSIXlt} or anything else that can be coerced to a date with
#' \code{as.Date()}. Defaults to the current system date.
#'
#' @param signatures The minimum number of signatures required for inclusion
#' in the tibble. Defaults to 1.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the content, signatories and sponsors of
#' all or a specified early day motions.
#'
#' @seealso \code{\link{mp_edms}}
#' @export
#' @examples \dontrun{
#'
#' # Returns all EDMs with a given ID
#' x <- early_day_motions(edm_id = 1073)
#'
#' # Return a specific early day motion by ID
#' x <- early_day_motions(edm_id = 1073, session='2017/19')
#' }


early_day_motions <- function(edm_id = NULL, session = NULL,
                              start_date = "1900-01-01",
                              end_date = Sys.Date(), signatures = 1,
                              extra_args = NULL, tidy = TRUE,
                              tidy_style = "snake_case", verbose = TRUE) {
  edm_query <- ifelse(
    is.null(edm_id) == FALSE,
    paste0("&edmNumber=", edm_id),
    ""
  )

  session_query <- ifelse(
    is.null(session) == FALSE,
    paste0("&session.=", session),
    ""
  )

  dates <- paste0(
    "&_properties=dateTabled&max-dateTabled=", as.Date(end_date),
    "&min-dateTabled=", as.Date(start_date)
  )

  sig_min <- paste0("&min-numberOfSignatures=", signatures)

  baseurl <- paste0(url_util, "edms")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  edms <- jsonlite::fromJSON(paste0(
    baseurl, ".json?", edm_query, dates,
    session_query, sig_min, extra_args,
    "&_pageSize=1"
  ), flatten = TRUE)

  jpage <- floor(edms$result$totalResults / 100)

  query <- paste0(
    baseurl, ".json?", edm_query, dates, session_query, sig_min,
    extra_args, "&_pageSize=100&_page="
  )

  df <- edm_loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- edm_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname early_day_motions
#' @export
hansard_early_day_motions <- early_day_motions
