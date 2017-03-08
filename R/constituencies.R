
#' constituencies
#'
#' Imports data on House of Commons constituencies
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords Constituencies
#' @export
#' @examples \dontrun{
#' x <- constituencies()
#' }
#'

constituencies <- function(extra_args=NULL) {

    baseurl <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"

    message("Connecting to API")

    conts <- jsonlite::fromJSON(paste0(baseurl, extra_args),flatten = TRUE)

    jpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
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
