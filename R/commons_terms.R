

#' commons_terms
#'
#' Imports the parliamentary thesaurus
#' @param all Returns a data frame with all definitions in the parliamentary thesaurus
#' @keywords parliamentary thesaurus
#' @export
#' @examples \dontrun{
#'
#' x <- commons_terms(all)
#'
#'}

commons_terms <- function(all=TRUE) {

    if (termsType == "all") {

        baseurl <- "http://lda.data.parliament.uk/terms.json?_pageSize=500"

        message("Connecting to API")

        terms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json?_pageSize=500")

        jpage <- round(terms$result$totalResults/terms$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:10) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
