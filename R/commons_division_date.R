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
#' @inheritParams all_answered_questions
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
                                  tidy_style = "snake", verbose = TRUE) {
  if (is.null(date)) {
    stop("Please include a date.", call. = FALSE)
  } else {
    date <- paste0("&date=", as.character(date))

    veb(verbose)

    query <- paste0(url_util, "commonsdivisions.json?", date, extra_args)

    jpage <- jpage_func(query)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {
      message("The request did not return any data.
                    Please check your parameters.")
    } else {
      if (tidy) {
        df <- cdd_tidy(df, tidy_style) ## utils-commons.R
      }

      df
    }
  }
}

#' @rdname commons_division_date
#' @export
hansard_commons_division_date <- commons_division_date
