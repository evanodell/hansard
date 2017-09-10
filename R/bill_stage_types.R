#' Bill Stage Types
#'
#' Returns a tibble with all possible bill stage types.
#' @inheritParams all_answered_questions
#' @return A tibble of bill stage types.
#' @seealso \code{\link{bills}}
#' @export
#' @examples \dontrun{
#' x <- bill_stage_types()
#' }

bill_stage_types <- function(tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  stages <- jsonlite::fromJSON("http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500", flatten = TRUE)

  df <- tibble::as_tibble(stages$result$items)

  if (nrow(df) == 0 && verbose==TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      df <- hansard_tidy(df, tidy_style)

    }

      df

  }

}


#' @rdname bill_stage_types
#' @export
hansard_bill_stage_types <- function(tidy = TRUE, tidy_style = "snake_case", verbose = FALSE){

  df <- bill_stage_types(tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}
