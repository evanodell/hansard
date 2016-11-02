
### 15 LORDS WRITTEN QUESTIONS

#' House of Lords Attendance
#'
#' This imports data on House of Lords written questions
#' @param all Imports all available House of Lords written questions Defaults to TRUE.
#' @keywords House of Lords Written Questions
#' @export
#' @examples
#' lords_written_questions()

lords_written_questions <- function(all = TRUE) {

    baseurl_lordsWrit <- "http://lda.data.parliament.uk/lordswrittenquestions.json"

    lordsWrit <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordswrittenquestions.json")

    lordsWritJpage <- round(lordsWrit$result$totalResults/lordsWrit$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsWritJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsWrit, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", lordsWritJpage)
        pages[[i + 1]] <- mydata$result$items
    }
}
