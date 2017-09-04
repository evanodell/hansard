
#' Parliamentary Session data
#'
#' Imports data on Parliamentary Sessions. Note that due to the date format used by the API, if \code{days==TRUE} and the \code{end_date} and \code{start_date} parameters are not set to the default values, the function downloads all available data and then subsets the tibble between the two given dates.
#' @param days If \code{TRUE}, returns data for all available days. If \code{FALSE}, returns data on each parliamentary session. If \code{TRUE} and non-default \code{start_date} and/or \code{end_date} parameters are requested, the function must retrieve all days and subset based on the values passed to \code{start_date} and \code{end_date}. Defaults to \code{FALSE}.
#' @param start_date Only includes sessions starting on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes sessions ending on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on parliamentary sessions.
#'
#' @export
#' @examples \dontrun{
#' x <- sessions_info(days=TRUE)
#'
#' y <- sessions_info(days=FALSE)
#' }

sessions_info <- function(days = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

    if (days == FALSE) {

      query <- paste0(".json?&max-endDate=", as.Date(end_date), "&min-startDate=", as.Date(start_date))

    } else {

      query <- "/days.json?"

    }

    baseurl <- "http://lda.data.parliament.uk/sessions"

    if(verbose==TRUE){message("Connecting to API")}

    session <- jsonlite::fromJSON(paste0(baseurl, query, extra_args))

    jpage <- floor(session$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (days == TRUE) {
        df$date._value <- as.POSIXct(df$date._value)
        df <- df[df$date._value <= as.Date(end_date) & df$date._value >= as.Date(start_date), ]
    }

    if (nrow(df) == 0 && verbose==TRUE) {

        message("The request did not return any data. Please check your search parameters.")

    } else {

        if (tidy == TRUE) {## move to external utils file?

            if (days == FALSE) {

                df$endDate._value <- as.POSIXct(df$endDate._value)

                df$startDate._value <- as.POSIXct(df$startDate._value)

                df$endDate._datatype <- "POSIXct"

                df$startDate._datatype <- "POSIXct"

            } else {

                df$date._value <- as.POSIXct(df$date._value)

                df$date._datatype <- "POSIXct"
            }

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}


#' @rdname sessions_info
#' @export
hansard_sessions_info <- function(days = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- sessions_info(days=days, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}
