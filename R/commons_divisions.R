
#' Imports data on House of Commons divisions.
#'
#' @param division_id The id of a particular vote. If empty, returns a tibble with information on all commons divisions. Defaults to NULL.
#' @param summary If TRUE, returns a small tibble summarising a division outcome. Otherwise returns a tibble with details on how each MP voted. Has no effect if `division_id` is empty. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to current system date. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with the results of divisions in the House of Commons.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_divisions()
#'
#' x <- commons_divisions(division_id = 694163, summary = FALSE)
#'
#' }

commons_divisions <- function(division_id = NULL, summary = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=",as.Date(start_date))

    if (is.null(division_id) == TRUE) {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, extra_args))

        jpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else if (is.null(division_id) == FALSE) {

        division_id <- as.character(division_id)

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json?", dates, extra_args), flatten = TRUE)

        if (summary == TRUE) {

            df <- divis$result$primaryTopic

            df$AbstainCount <- df$AbstainCount$`_value`
            df$AyesCount <- df$AyesCount$`_value`
            df$Didnotvotecount <- df$Didnotvotecount$`_value`
            df$Errorvotecount <- df$Errorvotecount$`_value`
            df$Noesvotecount <- df$Noesvotecount$`_value`
            df$Noneligiblecount <- df$Noneligiblecount$`_value`
            df$vote <- NULL
            df$Margin <- df$Margin$`_value`
            df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
            df$date <- df$date$`_value`

            df <- tibble::as_tibble(df)

        } else {

            df <- tibble::as_tibble(divis$result$primaryTopic$vote)

        }

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$date._value <- as.Date(df$date._value)

            df$date._datatype <- "Date"

            df <- hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }

}


#' Returns a tibble with the dates of House of Commons divisions.
#' @param date Returns all divisions on a given date. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with the dates of divisions in the House of Commons.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- commons_division_date('2017-04-19')
#' }
#'
commons_division_date <- function(date = NULL, extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(date) == TRUE) {
        df <- commons_divisions()
    } else {
        date <- as.character(date)
        date <- paste0("&date=", date)

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, extra_args))

        jpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df <- tibble::as_tibble(df)

        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {

            if (tidy == TRUE) {

                df$date._value <- as.Date(df$date._value)

                df$date._datatype <- "Date"

                df <- hansard_tidy(df, tidy_style)

                df

            } else {

                df

            }

        }
    }

}
