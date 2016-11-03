#### PUBLICATION INFORMATION


#' Publication Logs
#'
#' This imports data on Publication Logs
#' @param all Imports all available Publication Logs Defaults to TRUE.
#' @keywords Publication Logs
#' @export
#' @examples
#' x <- publication_logs()


### 19 PUBLICATION LOGS

publication_logs <- function(all = TRUE) {

    baseurl_logs <- "http://lda.data.parliament.uk/publicationlogs.json?_pageSize=500"

    logs <- jsonlite::fromJSON("http://lda.data.parliament.uk/publicationlogs.json?_pageSize=500")

    logsJpage <- round(logs$result$totalResults/logs$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:logsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_logs, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", logsJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

    df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}

