
#' papers_laid
#'
#' Imports data on Papers Laid
#' @param all Returns a data frame with all available papers laid.
#' @keywords Papers Laid
#' @export
#' @examples \dontrun{
#' x <- papers_laid('all') }
#'

papers_laid <- function(all) {

        baseurl <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"

        message("Connecting to API")

        papers <- jsonlite::fromJSON(baseurl)

        jpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


