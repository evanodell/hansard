#### PUBLICATION INFORMATION


#' Publication Logs
#'
#' This imports data on Publication Logs
#' @param all Imports all available Publication Logs Defaults to TRUE.
#' @keywords Publication Logs
#' @export
#' @examples
#' publication_logs()


### 19 PUBLICATION LOGS

publication_logs <- function(all = TRUE) {

    baseurl_logs <- "http://lda.data.parliament.uk/publicationlogs.json"

    logs <- jsonlite::fromJSON("http://lda.data.parliament.uk/publicationlogs.json")

    logsJpage <- round(loge$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:logsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_logs, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}

