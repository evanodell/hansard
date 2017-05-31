
#' Imports data on Parliamentary Sessions. Note that due to the date format used by the API, if \code{days}==TRUE and the \code{end_date} and \code{start_date } parameters are not set to the default values, the function downloads all available data and then subsets the tibble between the two given dates.
#' @param days If TRUE, returns data for all available days. If FALSE, returns data on each parliamentary session. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on parliamentary sessions.
#' @keywords Parliamentary Sessions
#' @export
#' @examples \dontrun{
#'
#' x <- sessions_info(days=TRUE)
#'
#' y <- sessions_info(days=FALSE)
#'
#' }

sessions_info <- function(days = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    if (days == FALSE) {
        dates <- paste0("&_properties=startDate&max-startDate=", as.Date(end_date), "&min-startDate=", as.Date(start_date ))
        query <- NULL
    } else {
        dates <- NULL
        query <- "/days"
    }

    baseurl <- "http://lda.data.parliament.uk/sessions"

    message("Connecting to API")

    session <- jsonlite::fromJSON(paste0(baseurl, query, ".json?_pageSize=500", dates, extra_args))

    jpage <- round(session$result$totalResults/session$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?_pageSize=500&_page=", i, dates, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (days == TRUE) {
        df$date._value <- as.Date(df$date._value)
        df <- df[df$date._value <= as.Date(end_date) & df$date._value >= as.Date(start_date ), ]
    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            if (days == FALSE) {

                df$endDate._value <- as.Date(df$endDate._value)

                df$startDate._value <- as.Date(df$startDate._value)

                df$endDate._datatype <- "Date"

                df$startDate._datatype <- "Date"

            } else {

                df$date._value <- as.Date(df$date._value)

                df$date._datatype <- "Date"
            }

            df <- hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}
