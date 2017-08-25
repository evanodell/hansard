

#' House of Lords attendance
#'
#' Imports data on House of Lords attendance. Please note that the attendance data is not as tidy as some of the others that are accessible through this API, and so additional work to prepare this data in a way that you want may be required.
#' @param session_id The ID of the House of Lords session. If \code{NULL}, returns a list of all sessions. Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on the lords who attended a given session.
#'
#' @export
#' @examples \dontrun{
#' x <- lords_attendance(session_id = 706178)
#' }

lords_attendance <- function(session_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(session_id) == FALSE) {
        query <- paste0("/", session_id, ".json?")
    } else {
        query <- ".json?"
    }

    dates <- paste0("&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    baseurl <- "http://lda.data.parliament.uk/lordsattendances"

    if(verbose==TRUE){message("Connecting to API")}

    attend <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    if (is.null(session_id) == FALSE) {

        df <- tibble::as_tibble(as.data.frame(attend$result$primaryTopic))

    } else {

        jpage <- floor(attend$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

          df <- lords_attendance_tidy(df, tidy_style)

        }

          df

    }
}


#' @rdname lords_attendance
#' @export
hansard_lords_attendance <- function(session_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lords_attendance(session_id = session_id, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

  }
