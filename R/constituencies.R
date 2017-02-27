
#' constituencies
#'
#' Imports data on House of Commons constituencies
#' @param all Returns a data frame of all constituencies. Defaults to TRUE.
#' @keywords Constituencies
#' @export
#' @examples \dontrun{
#' x <- constituencies()
#' }
#'

constituencies <- function(all = TRUE) {

            baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"

            message("Connecting to API")

            conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json?_pageSize=500")

            contsJpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)

            pages <- list()

            for (i in 0:contsJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", contsJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
