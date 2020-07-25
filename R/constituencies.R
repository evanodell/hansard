
#' House of Commons constituencies
#'
#' Imports data on House of Commons constituencies, returning a tibble of all
#' current and/or former Westminster constituencies, subject to parameters.
#'
#' @param current If `TRUE`, returns only current constituencies. If
#' `FALSE`, returns only former constituencies. If `NULL`, returns
#' all current and former constituencies. Defaults to `NULL`.
#' @inheritParams all_answered_questions
#' @return A tibble with details of Westminster constituencies.
#' @export
#' @examples
#' \dontrun{
#' x <- constituencies()
#'
#' y <- constituencies(current = FALSE)
#' }
#'
constituencies <- function(current = NULL, extra_args = NULL, tidy = TRUE,
                           tidy_style = "snake", verbose = TRUE) {
  if (is.null(current)) {
    current_query <- ""
  } else if (current) {
    current_query <- "&exists-endedDate=false"
  } else if (!current) {
    current_query <- "&exists-endedDate=true"
  } else {
    current_query <- ""
  }

  query <- paste0(url_util, "constituencies.json?", extra_args, current_query)

  df <- loop_query(query, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- cons_tidy(df, current, tidy_style)
    }

    df
  }
}


#' @rdname constituencies
#' @export
hansard_constituencies <- constituencies
