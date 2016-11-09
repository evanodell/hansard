

#' Elections
#'
#' This imports data on elections
#' @param all Imports all available elections Defaults to TRUE.
#' @keywords Elections
#' @export
#' @examples \donttest{
#' x <- elections(all = TRUE)
#' # Returns a data frame with the date and type of all general and by-elections since 1945
#' }


elections <- function(all = TRUE) {
    # WORKING!

    baseurl_elect <- "http://lda.data.parliament.uk/elections.json?_pageSize=500"

    elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json?_pageSize=500")

    pages <- list()

    for (i in 0:0) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}
