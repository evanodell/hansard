

#' Imports data on House of Commons and House of Lords bills
#' @param ID The ID of a given bill to return data on. If NULL, returns all bills, subject to other parameters. Defaults to NULL.
#' @param amendments If TRUE, returns all bills with amendments. Defaults to FALSE.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in "YYYY-MM-DD" format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on bills before the House of Lords and the House of Commons.
#' @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- bills()
#'
#' x <- bills(amendments=TRUE)
#'
#' x <- bills(1719)
#'
#' }

bills <- function(ID = NULL, amendments = FALSE, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=",as.Date(start_date))

    if (is.null(ID) == FALSE) {
        id_query <- paste0("&identifier=", ID)
    } else {
        id_query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/bills"

    if (amendments == TRUE) {
        query <- "withamendments.json?_pageSize=500"
    } else {
        query <- ".json?_pageSize=500"
    }

    message("Connecting to API")

    bills <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args), flatten = TRUE)

    jpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, id_query, extra_args, "&_page=", i), flatten = TRUE)
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



#' bill_stage_types
#'
#' Returns a tibble with all possible bill stage types.
#' @keywords bills
#' @rdname bills
#' @export
#' @examples \dontrun{
#' x <- bill_stage_types()
#' }

bill_stage_types <- function(tidy = TRUE) {

    stages <- jsonlite::fromJSON("http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500", flatten = TRUE)

    df <- tibble::as_tibble(stages$result$items)

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
