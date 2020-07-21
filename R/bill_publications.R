

#' Bill Publications
#'
#' Returns details of all publications associated with a specific bill
#' or all bills.
#'
#'
#' @param ID The ID of a specific bill to return publications for.
#' If `NULL`, returns all bill publications subject to other parameters.
#' Defaults to `NULL`.
#'
#'
#' @param publication_type The type of bill publication to return, in the form
#' of a string. For a character vector of bill publication types, see
#' [bill_publication_types()]. If `NULL`, returns all
#' publications of all types, subject to other parameters.
#' Defaults to `NULL`.
#'
#'
#' @param start_date Only includes bill publications on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#'
#' @param end_date Only includes bill publicationson or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#'
#' @inheritParams all_answered_questions
#'
#' @return A tibble with details on bill publications.
#' @export
#' @seealso [bill_stage_types()]
#' @seealso [bills()]
#' @seealso [bill_publication_types()]
#'
#' @examples
#' \dontrun{
#' # Requesting a specific publication
#' x <- bill_publications(ID = 752025)
#'
#' # Requesting all publications after a given date
#' y <- bill_publications(start_date = "2018-01-01")
#' }
bill_publications <- function(ID = NULL, publication_type = NULL,
                              start_date = "1900-01-01", end_date = Sys.Date(),
                              extra_args = NULL, tidy = TRUE,
                              tidy_style = "snake", verbose = FALSE) {
  baseurl <- paste0(url_util, "billpublications.json?")

  dates <- paste0(
    "&max-date=",
    as.Date(end_date),
    "&min-date=",
    as.Date(start_date)
  )

  bill_id <- ifelse(is.null(ID),
    "",
    paste0("&bill=http://data.parliament.uk/resources/", ID)
  )

  if (is.null(publication_type)) {
    pub_query <- ""
  } else {
    pub_query <-  utils::URLencode(paste0(
      "&publicationType=",
      publication_type
    ))
  }

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  bills <- jsonlite::fromJSON(paste0(
    baseurl, bill_id, dates,
    extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(bills$result$totalResults / 100)

  query <- paste0(
    baseurl, bill_id, dates, extra_args
  )

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- bills_tidy(df, tidy_style) # in utils-bills.R
    }

    df
  }
}

#' @rdname bill_publications
#' @export
hansard_bill_publications <- bill_publications
