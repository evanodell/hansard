

#' publication_logs
#'
#' Imports data on Publication Logs
#' @param all Returns a data frame with all available Publication Logs. Defaults to TRUE.
#' @keywords Publication Logs
#' @export
#' @examples \dontrun{
#' # x <- publication_logs()
#' }

publication_logs <- function(all = TRUE) {

    baseurl_logs <- "http://lda.data.parliament.uk/publicationlogs.json?_pageSize=500"

    logs <- jsonlite::fromJSON(baseurl_logs)

    logsJpage <- round(logs$result$totalResults/logs$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:logsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_logs, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", logsJpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}

