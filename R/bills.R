

#' Bill data
#'
#' Imports data on House of Commons and House of Lords bills.
#'
#' 
#' @param ID The ID of a given bill to return data on. If \code{NULL},
#' returns all bills, subject to other parameters. Defaults to \code{NULL}.
#'
#' 
#' @param amendments If \code{TRUE}, returns all bills with amendments,
#' subject to other parameters. Defaults to \code{FALSE}.
#'
#' 
#' @param start_date Only includes bills introduced on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#'
#' 
#' @param end_date Only includes bills introduced on or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#'
#' @inheritParams all_answered_questions
#'
#' @return A tibble with details on bills before the House of Lords
#' and the House of Commons.
#'
#' @seealso \code{\link{bill_stage_types}}
#' @seealso \code{\link{bill_publications}}
#' @export
#'
#' @examples \dontrun{
#' # Download data on all bills
#' x <- bills()
#'
#' # Download data on all bill amendments
#' x <- bills(amendments = TRUE)
#'
#' # Download data on a specific bills
#' x <- bills(1719)
#'
#' # Download data on all bills introduced after a given date
#' x <- bills(start_date = '2016-01-01')
#' }

bills <- function(ID = NULL, amendments = FALSE, start_date = "1900-01-01",
                  end_date = Sys.Date(), extra_args = NULL, tidy = TRUE,
                  tidy_style = "snake_case", verbose = TRUE) {
  dates <- paste0(
    "&_properties=date&max-date=",
    as.Date(end_date),
    "&min-date=",
    as.Date(start_date)
  )

  id_query <- ifelse(is.null(ID) == FALSE, paste0("&identifier=", ID), "")

  amend_query <- ifelse(amendments, "withamendments.json?", ".json?")

  baseurl <- paste0(url_util, "bills")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  bills <- jsonlite::fromJSON(paste0(
    baseurl, amend_query, dates, id_query,
    extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(bills$result$totalResults / 100)

  query <- paste0(
    baseurl, amend_query,
    dates, id_query, extra_args,
    "&_pageSize=100&_page="
  )

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- bills_tidy(df, tidy_style) ### in utils-bills.R
    }

    df
  }
}


#' @rdname bills
#' @export
hansard_bills <- bills
