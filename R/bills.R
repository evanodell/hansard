
#' bills
#'
#' Imports data on House of Commons and House of Lords bills
#' @param billType The type of data you want, allows the arguments 'ammended', 'publications' and 'stage_types'
#' @param ammended Imports a data frame with all ammended bills
#' @param stage_types Imports a data frame with all bill stage types
#' @param publications Imports a data frame with all bill publications
#' @keywords bills
#' @export
#' @examples \dontrun{
#' x <- bills('all')
#'
#' x <- bills('ammended')
#'
#' x <- bills('stage_types')
#' }
#' @note There are problems with the Bills API, as the JSON data available for some queries, including the query to return all bills currently before the house, is inconsistently formatted and cannot be parsed into a data frame.

bills <- function(billType = c("ammended", "stage_types", "publications")) {
    
    match.arg(billType)
    
    if (billType == "ammended") {
        # Working but return is weird
        
        baseurl <- "http://lda.data.parliament.uk/billswithamendments.json?_pageSize=500"
        
        message("Connecting to API")
        
        bills <- jsonlite::fromJSON(baseurl)
        
        if (bills$result$totalResults > bills$result$itemsPerPage) {
            
            jpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)
            
        } else {
            jpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (billType == "stage_types") {
        
        baseurl <- "http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500"
        
        message("Connecting to API")
        
        bills <- jsonlite::fromJSON(baseurl)
        
        jpage <- 0
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (billType == "publications") {
        
        baseurl <- "http://lda.data.parliament.uk/billpublications.json?_pageSize=500"
        
        message("Connecting to API")
        
        bills <- jsonlite::fromJSON(baseurl)
        
        jpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
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
