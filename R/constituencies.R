
#' Imports data on House of Commons constituencies, returning a tibble of all current and former Westminster constituencies.
#' @param current If TRUE, returns only current constituencies. If FALSE, returns all current and former constituencies. Due to the structure of the API, this function has to request all available data, and then subset that data. Defaults to TRUE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details of Westminster constituencies.
#' @keywords Constituencies
#' @export
#' @examples \dontrun{
#'
#' x <- constituencies()
#'
#' x <- constituencies(current = FALSE)
#'
#' }
#'

constituencies <- function(current = TRUE, extra_args = NULL, tidy = TRUE) {

    baseurl <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"

    message("Connecting to API")

    conts <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    df$endedDate._value <- as.Date(df$endedDate._value)
    df$startedDate._value <- as.Date(df$startedDate._value)

    if (current == TRUE) {
        df <- df[is.na(df$endedDate._value) == TRUE, ]
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
