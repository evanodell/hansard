
#' Papers Laid
#'
#' This imports data on Papers Laid
#' @param paperType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Imports all available papers laid
#' @keywords Papers Laid
#' @export
#' @examples \donttest{
#' x <- papers_laid('all') }
#'

papers_laid <- function(paperType = c("all")) {

    match.arg(paperType)

    if (paperType == "all") {

        baseurl_papers <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"

        papers <- jsonlite::fromJSON(baseurl_papers)

        papersJpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:papersJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_papers, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", papersJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}


