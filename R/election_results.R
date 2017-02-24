
#' election_results
#'
#' Imports data on general election results
#' @param resultType Accepts the arguments 'all' and 'ID'
#' @param all Returns general and by-election resuls for each consituency from the 2010 general election onwards.
#' @param ID Returns general and by-election resuls for each consituency from the 2010 general election onwards.
#' @keywords Election Results
#' @export
#' @examples \dontrun{
#' x <- election_results('all')
#'
#' x <- election_results('ID')
#' }

election_results <- function(resultType = c("all", "ID")) {
    
    match.arg(resultType)
    
    if (resultType == "all") {
        
        baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json?_pageSize=500"
        
        message("Connecting to API")
        
        electR <- jsonlite::fromJSON("http://lda.data.parliament.uk/electionresults.json?_pageSize=500")
        
        electRJpage <- round(electR$result$totalResults/electR$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:electRJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_electR, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", electRJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resultType == "ID") {
        
        electID <- readline("Enter the election ID: ")
        
        baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json?electionId="
        
        message("Connecting to API")
        
        electR <- jsonlite::fromJSON(paste0(baseurl_electR, electID, "&_pageSize=500"))
        
        if (electR$result$totalResults > electR$result$itemsPerPage) {
            
            electRJpage <- round(electR$result$totalResults/electR$result$itemsPerPage, digits = 0)
        } else {
            electRJpage <- 0
        }
        pages <- list()
        
        for (i in 0:electRJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_electR, electID, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", electRJpage + 1)
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
