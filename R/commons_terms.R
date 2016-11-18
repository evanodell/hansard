

#' commons_terms
#'
#' Imports the parliamentary thesaurus
#' @param termsType The type of data you want, allows the argument 'all'
#' @param all Returns a data frame with all definitions in the parliamentary thesaurus
#' @keywords parliamentary thesaurus
#' @export
#' @examples \dontrun{
#' x <- commons_terms('all')
#' head(x)
#'  }

commons_terms <- function(termsType = c("all")) {
    
    match.arg(termsType)
    
    if (termsType == "all") {
        
        baseurl_terms <- "http://lda.data.parliament.uk/terms.json?_pageSize=500"
        
        cTerms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json?_pageSize=500")
        
        cTermsJpage <- round(cTerms$result$totalResults/cTerms$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:10) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_terms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", cTermsJpage + 1)
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
