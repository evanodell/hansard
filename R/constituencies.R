
#' constituencies
#'
#' Imports data on House of Commons constituencies, returning a data frame of all current and former Westminster constituencies.
#' @param current If TRUE, returns only current constituencies. If FALSE, returns all current and former constituencies. Due to the structure of the API, this function has to request all available data, and then subset that data.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords Constituencies
#' @export
#' @examples \dontrun{
#'
#' x <- constituencies()
#'
#' }
#'

constituencies <- function(current=TRUE, extra_args = NULL) {

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

    df$endedDate._value <- as.Date(df$endedDate._value)
    df$startedDate._value <- as.Date(df$startedDate._value)

    if(current==TRUE){
      df <- df[is.na(df$endedDate._value)==TRUE,]
    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
