
### 17 PAPERS LAID


#' Papers Laid
#'
#' This imports data on Papers Laid
#' @param all Imports all available papers laid Defaults to TRUE.
#' @keywords Papers Laid
#' @export
#' @examples
#' papers_laid

papers_laid <- function(all = TRUE) {

    baseurl_papers <- "http://lda.data.parliament.uk/paperslaid.json"

    papers <- jsonlite::fromJSON("http://lda.data.parliament.uk/paperslaid.json")

    papersJpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:papersJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_papers, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", papersJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }
}


