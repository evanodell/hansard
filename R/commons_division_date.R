#' House of Commons Division Dates
#'
#' Returns a tibble with the divisions of the House of Commons on a given date
#' @param date Returns all divisions on a given date. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If TRUE, returns data to console on the progress of the API request. Defaults to FALSE.
#' @return  A tibble with the dates of divisions in the House of Commons.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- commons_division_date('2017-04-19')
#' }


commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  if (is.null(date) == TRUE) {
    df <- commons_divisions()
  } else {
    date <- as.character(date)
    date <- paste0("&date=", date)

    baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

    if(verbose==TRUE){message("Connecting to API")}

    divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, extra_args))

    jpage <- floor(divis$result$totalResults/divis$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, "&_page=", i, extra_args), flatten = TRUE)
      if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
      pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    if (nrow(df) == 0 && verbose==TRUE) {
      message("The request did not return any data. Please check your search parameters.")
    } else {

      if (tidy == TRUE) {

        df$date._value <- as.POSIXct(df$date._value)

        df$date._datatype <- "POSIXct"

        df <- hansard_tidy(df, tidy_style)

        df

      } else {

        df

      }
    }
  }
}

#' @rdname commons_division_date
#' @export
hansard_commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- commons_division_date(date=date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
