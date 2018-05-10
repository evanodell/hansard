
#' House of Commons constituencies
#'
#' Imports data on House of Commons constituencies, returning a tibble of all
#' current and/or former Westminster constituencies, subject to parameters.
#' @param current If \code{TRUE}, returns only current constituencies. If
#' \code{FALSE}, returns only former constituencies. If \code{NULL}, returns
#' all current and former constituencies. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with details of Westminster constituencies.
#' @export
#' @examples \dontrun{
#' x <- constituencies()
#'
#' y <- constituencies(current = FALSE)
#' }


constituencies <- function(current = NULL, extra_args = NULL, tidy = TRUE,
                           tidy_style = "snake_case", verbose = TRUE) {
  baseurl <- paste0(url_util, "constituencies.json?")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  conts <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

  jpage <- floor(conts$result$totalResults / 500)

  if (is.null(current)) {
    current_query <- ""
  } else if (current==TRUE) {
    current_query <- "&exists-endedDate=false"
  } else if (current==FALSE) {
    current_query <- "&exists-endedDate=true"
  } else {
    current_query <- ""
  }

  query <- paste0(baseurl, extra_args, current_query, "&_pageSize=500&_page=")

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- cons_tidy(df, current, tidy_style)
    }

    df
  }
}


#' @rdname constituencies
#' @export
hansard_constituencies <- constituencies
