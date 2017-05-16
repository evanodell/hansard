

#' Imports data on House of Lords attendance. Please note that the attendance data is not as tidy as some of the others that are accessible through this API, and so additional work to prepare this data in a way that you want may be required.
#' @param session_id The ID of the House of Lords session. If NULL, returns a list of all sessions. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE, tidy_style="snake_case". Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return Returns a tibble with details on the lords who attended a given session.
#' @keywords House of Lords Attendance
#' @export
#' @examples \dontrun{
#'
#' x <- lords_attendance(session_id = 706178)
#' }
#'
lords_attendance <- function(session_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(session_id) == FALSE) {
        query <- paste0("/", session_id, ".json?")
    } else {
        query <- ".json?_pageSize=500"
    }

    dates <- paste0("&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    baseurl <- "http://lda.data.parliament.uk/lordsattendances"

    message("Connecting to API")

    attend <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    if (is.null(session_id) == FALSE) {

        df <- as.data.frame(attend$result$primaryTopic)

        df <- tibble::as_tibble(df)

    } else {

        jpage <- round(attend$result$totalResults/attend$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df <- tibble::as_tibble(df)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

            names(df)[names(df) == "x_about"] <- "about"

            names(df)[names(df) == "x_value"] <- "value"

            df

        } else {

            df

        }

    }
}
