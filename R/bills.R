

#' Bill data
#'
#' Imports data on House of Commons and House of Lords bills.
#'
#'
#' @param ID The ID of a given bill to return data on. If `NULL`,
#' returns all bills, subject to other parameters. Defaults to `NULL`.
#'
#'
#' @param amendments If `TRUE`, returns all bills with amendments,
#' subject to other parameters. Defaults to `FALSE`.
#'
#'
#' @param start_date Only includes bills introduced on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#'
#' @param end_date Only includes bills introduced on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#'
#' @inheritParams all_answered_questions
#'
#' @return A tibble with details on bills before the House of Lords
#' and the House of Commons.
#'
#' @seealso [bill_stage_types()]
#' @seealso [bill_publications()]
#' @export
#'
#' @examples
#' \dontrun{
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
#' x <- bills(start_date = "2016-01-01")
#' }
#'
bills <- function(ID = NULL, amendments = FALSE, start_date = "1900-01-01",
                  end_date = Sys.Date(), extra_args = NULL, tidy = TRUE,
                  tidy_style = "snake", verbose = TRUE) {
  dates <- paste0(
    "&_properties=date&max-date=",
    as.Date(end_date),
    "&min-date=",
    as.Date(start_date)
  )

  id_query <- ifelse(!is.null(ID), paste0("&identifier=", ID), "")

  amend_query <- ifelse(amendments, "withamendments.json?", ".json?")

  query <- paste0(
    url_util, "bills", amend_query,
    dates, id_query, extra_args
  )

  df <- loop_query(query, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- bills_tidy(df, tidy_style) ### in utils-bills.R
    }
    df
  }
}


#' @rdname bills
#' @export
hansard_bills <- bills
