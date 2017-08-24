#' House of Commons Division Dates
#'
#' Returns a tibble with the divisions (votes) in the House of Commons on a given date.
#' @param date Returns all divisions on a given date. Defaults to \code{NULL}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with the dates of divisions in the House of Commons.
### @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- commons_division_date('2017-04-19')
#' }


commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  if (is.null(date) == TRUE) {

    stop("Please include a date.", call. = FALSE)

  } else {

    date <- paste0("&date=", as.character(date))

    baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

    if(verbose==TRUE){message("Connecting to API")}

    divis <- jsonlite::fromJSON(paste0(baseurl, ".json?", date, extra_args))

    jpage <- floor(divis$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, "&_page=", i, extra_args), flatten = TRUE)
      if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
      pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
      message("The request did not return any data. Please check your search parameters.")
    } else {

      if (tidy == TRUE) {

        df <- cdd_tidy(df, tidy_style)

      }

        df

    }
  }
}

#' @rdname commons_division_date
#' @export
hansard_commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- commons_division_date(date=date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
