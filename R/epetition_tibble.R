
#' Get data on epetitions submitted to parliament, with the label, sponsor, number of signatures, date created and epetition ID.
#'
#' @param min_signatures The minimum number of signatures required for inclusion in the tibble. Defaults to 1.
#' @param max_signatures The maximum number of signatures required for inclusion in the tibble. If NULL, there no maximum number of signatures. Defaults to NULL.
#' @param status The status of the petition, either 'open' or 'closed'. If NULL, returns all petitions both open and closed. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on electronic petitions submitted to parliament.
#' @seealso \code{\link{epetition}}
#' @keywords ePetitions
#' @export
#' @examples \dontrun{
#'
#' x <- epetition_tibble()
#'
#' y <- epetition_tibble(max_signatures=500)
#'
#' z <- epetition_tibble(start_date="2016-12-01", end_date="2017-04-25")
#'
#'}


epetition_tibble <- function(min_signatures=1, max_signatures=NULL, status=NULL, start_date="1900-01-01", end_date=Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  dates <- paste0("&max-created=", as.Date(end_date), "&min-created=", as.Date(start_date))

  if(is.null(status)==TRUE){
    status_query <- NULL
  } else {
    status_query <- paste0("&status=", status)
  }

  min_sig_query <- paste0("&min-numberOfSignatures=", min_signatures)

  if(is.null(max_signatures)==TRUE){
    max_sig_query <- NULL
  } else {
    max_sig_query <- paste0("&max-numberOfSignatures=", max_signatures)
  }

  baseurl <- "http://lda.data.parliament.uk/epetitions.json?_pageSize=500"

  message("Connecting to API")

  petition <- jsonlite::fromJSON(paste0(baseurl, status_query,min_sig_query, max_sig_query, dates, extra_args), flatten = TRUE)

  jpage <- floor(petition$result$totalResults/petition$result$itemsPerPage)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, status_query,min_sig_query, max_sig_query, dates, extra_args, "&_page=", i),flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", jpage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      df$created._value <- gsub("T", " ", df$created._value)

      df$created._value <- lubridate::parse_date_time(df$created._value, "Y-m-d H:M:S")

      df$created._datatype <- "POSIXct"

      df$status <- as.factor(df$status)

      df <- hansard::hansard_tidy(df, tidy_style)

    }

    df

  }
}


#' @rdname epetition_tibble
#' @export

hansard_epetition_tibble <- function(min_signatures=1, max_signatures=NULL, status=NULL, start_date="1900-01-01", end_date=Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- epetition_tibble(min_signatures=min_signatures, max_signatures=max_signatures, status=status, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}
