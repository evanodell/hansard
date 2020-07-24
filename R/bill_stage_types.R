#' Bill Stage Types
#'
#' Returns a tibble with all possible bill stage types.
#'
#' @inheritParams all_answered_questions
#' @return A tibble of bill stage types.
#' @seealso [bills()]
#' @seealso [bill_publications()]
#' @export
#' @examples
#' \dontrun{
#' x <- bill_stage_types()
#' }
#'
bill_stage_types <- function(tidy = TRUE, tidy_style = "snake",
                             verbose = TRUE) {
  stages <- jsonlite::fromJSON(
    "http://lda.data.parliament.uk/billstagetypes.json?_pageSize=100",
    flatten = TRUE
  )

  df <- tibble::as_tibble(stages$result$items)

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname bill_stage_types
#' @export
hansard_bill_stage_types <- bill_stage_types
