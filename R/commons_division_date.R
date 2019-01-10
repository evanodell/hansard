#' House of Commons Division Dates
#'
#' Returns a tibble with the divisions (votes) in the
#' House of Commons on a given date.
#'
#' @param date Returns all divisions on a given date.
#' Defaults to `NULL`.
#'
#' @param extra_args Additional parameters to pass to API.
#' Defaults to `NULL`.
#'
#' @param tidy Logical parameter. If TRUE, fixes the variable names in the
#' tibble to remove special characters and superfluous text, and converts
#' the variable names to a consistent style. Defaults to `TRUE`.
#'
#' @param tidy_style The style to convert variable names to, if
#' `tidy = TRUE`. Accepts one of `'snake_case'`, `'camelCase'`
#' and `'period.case'`. Defaults to `'snake_case'`.
#'
#' @param verbose If `TRUE`, returns data to console on the progress of
#' the API request. Defaults to `FALSE`.
#'
#' @return A tibble with the dates of divisions in the House of Commons.
#' @export
#' @examples
#' \dontrun{
#' # all commons divisions on 19 April 2017
#' x <- commons_division_date("2017-04-19")
#' }
#' 
commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE,
                                  tidy_style = "snake_case", verbose = TRUE) {
  if (is.null(date) == TRUE) {
    stop("Please include a date.", call. = FALSE)
  } else {
    date <- paste0("&date=", as.character(date))

    baseurl <- paste0(url_util, "commonsdivisions")

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    divis <- jsonlite::fromJSON(paste0(
      baseurl, ".json?", date,
      extra_args, "&_pageSize=1"
    ))

    jpage <- floor(divis$result$totalResults / 100)

    query <- paste0(
      baseurl, ".json?", date, extra_args,
      "&_pageSize=100&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {
      message("The request did not return any data.
                    Please check your parameters.")
    } else {
      if (tidy == TRUE) {
        df <- cdd_tidy(df, tidy_style) ## utils-commons.R
      }

      df
    }
  }
}

#' @rdname commons_division_date
#' @export
hansard_commons_division_date <- commons_division_date
