
#' Imports data on House of Lords Amendments. Returns a tibble with all available House of Lords amendments.
#' @param decision The decision on the amendments. Accepts one of 'Withdrawn', 'Agreed', 'Disagreed', 'Pending', 'NotMoved', 'Disposed'. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on amendments proposed by the House of Lords.
#' @keywords House of Lords Amendments
#' @export
#' @examples \dontrun{
#'
#' x <- lords_amendments()
#'
#' }

lords_amendments <- function(decision = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    dates <- paste0("&min-bill.date=", as.Date(start_date), "&max-bill.date=", as.Date(end_date))

    if (is.null(decision) == FALSE) {
        decision_query <- paste0("&decision=", decision)
    } else {
        decision_query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

    message("Connecting to API")

    ammend <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, extra_args), flatten = TRUE)

    jpage <- round(ammend$result$totalResults/ammend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}
