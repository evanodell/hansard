#' bill_stage_types
#'
#' Returns a tibble with all possible bill stage types.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @keywords bills
#' @seealso \code{\link{bills}}
#' @export
#' @examples \dontrun{
#' x <- bill_stage_types()
#' }

bill_stage_types <- function(tidy = TRUE, tidy_style = "snake_case") {

  stages <- jsonlite::fromJSON("http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500", flatten = TRUE)

  df <- tibble::as_tibble(stages$result$items)

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      df <- hansard::hansard_tidy(df, tidy_style)

      df

    } else {

      df

    }

  }

}




#' @rdname bill_stage_types
#' @export
hansard_bill_stage_types <- function(tidy = TRUE, tidy_style = "snake_case"){

  df <- bill_stage_types(tidy = tidy, tidy_style = tidy_style)

  df

}
