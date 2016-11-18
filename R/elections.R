

#' elections
#'
#' Imports data on elections
#' @param electType Allows the arguments 'all' and 'ID'
#' @param all Returns a data frame with the date and type of all general and by-elections since 1945.
#' @param ID Requests an election ID, and returns a data frame with the date and type of that election.
#' @keywords Elections
#' @export
#' @examples \dontrun{
#' x <- elections('all')
#'
#' x <- elections('ID')
#' }


elections <- function(electType = c("all", "ID")) {
    
    match.arg(electType)
    
    if (electType == "all") {
        
        baseurl_elect <- "http://lda.data.parliament.uk/elections.json?_pageSize=500"
        
        elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json?_pageSize=500")
        
        pages <- list()
        
        for (i in 0:0) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (electType == "ID") {
        
        electID <- readline("Enter the election ID: ")
        
        baseurl_elect <- "http://lda.data.parliament.uk/elections/"
        
        elect <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/elections/", electID, ".json"))
        
        mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/elections/", electID, ".json"), flatten = TRUE)
        
        df <- mydata$result$primaryTopic
        
        df <- as.data.frame(df)
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}
