

#' publication_logs
#'
#' Imports data on Publication Logs
#' @param ID Publication ID. Defaults to NULL. If not null, requests a tibble with information on the given publication.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords Publication Logs
#' @export
#' @examples \dontrun{
#' # x <- publication_logs(683267)
#' }

publication_logs <- function(ID = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    if (is.null(ID) == FALSE) {
        query <- paste0("/", ID, ".json?")
    } else {
        query <- ".json?&_pageSize=500"
    }

    dates <- paste0("&_properties=publicationDate&max-publicationDate=", end_date, "&min-publicationDate=", start_date)

    baseurl <- "http://lda.data.parliament.uk/publicationlogs"

    message("Connecting to API")

    logs <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)

    if (is.null(ID) == FALSE) {

        df <- tibble::as_tibble(logs$result$primaryTopic)

    } else {

        jpage <- round(logs$result$totalResults/logs$result$itemsPerPage, digits = 0)

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

            df <- hansard_tidy(df)

            df

        } else {
            
            df

        }

    }
}
