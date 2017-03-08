
#' election_results
#'
#' Imports data on general election results
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the results. If NULL, returns all available election results. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords Election Results
#' @export
#' @examples \dontrun{
#' x <- election_results(ID=382037)
#' }

election_results <- function(ID = NULL, extra_args = NULL) {
    
    baseurl <- "http://lda.data.parliament.uk/electionresults.json?electionId="
    
    message("Connecting to API")
    
    elect <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500", extra_args))
    
    if (elect$result$totalResults > elect$result$itemsPerPage) {
        
        jpage <- round(elect$result$totalResults/elect$result$itemsPerPage, digits = 0)
    } else {
        jpage <- 0
    }
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ID, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
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
