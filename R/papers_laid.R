
#' papers_laid
#'
#' Imports data on Papers Laid
#' @param withdrawn If TRUE, only returns withdrawn papers. Defaults to FALSE.
#' @param house The house the paper was laid in. Accepts 'commons' and 'lords'. If NULL, returns both House of Commons and House of Lords. Defaults to NULL.
#' @param start_date The earliest paper laying date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest paper laying date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords Papers Laid
#' @export
#' @examples \dontrun{
#' x <- papers_laid(withdrawn = FALSE, house = 'commons')
#'
#' x <- papers_laid(withdrawn = TRUE, house = NULL,)
#' }
#'

papers_laid <- function(withdrawn = FALSE, house = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    house <- tolower(house)

    if (house == "commons") {
        house <- "&legislature.prefLabel=House of Commons"
        house <- utils::URLencode(house)
    } else if (house == "lords") {
        house <- "&legislature.prefLabel=House of Lords"
        house <- utils::URLencode(house)
    } else {
        house <- NULL
    }

    if (withdrawn == TRUE) {
        query <- "&withdrawn=true"
    } else {
        query <- "&withdrawn=false"
    }

    dates <- paste0("&max-ddpModified=", end_date, "&min-ddpModified=", start_date)

    baseurl <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"

    message("Connecting to API")

    papers <- jsonlite::fromJSON(paste0(baseurl, query, house, dates, extra_args), flatten = TRUE)

    jpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, house, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

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
