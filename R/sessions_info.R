
#' sessions_info
#'
#' Imports data on Parliamentary Sessions
#' @param sesType The type of data being requested, allows the arguments 'all' and 'days'
#' @param all Imports information on all available parliamentary sessions
#' @param days Imports information on the days in all available parliamentary sessions
#' @keywords Parliamentary Sessions
#' @export
#' @examples \dontrun{
#'
#' x <- sessions_info('all')
#'
#' x <- sessions_info('days')
#' }

sessions_info <- function(sesType = c("all", "days")) {
    
    match.arg(sesType)
    
    if (sesType == "all") {
        
        baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json?_pageSize=500"
        
        sessionsJpage <- 0
        
        pages <- list()
        
        for (i in 0:sessionsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", sessionsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (sesType == "days") {
        
        baseurl_sessions <- "http://lda.data.parliament.uk/sessions/days.json?_pageSize=500"
        
        sessionsJpage <- 0
        
        pages <- list()
        
        for (i in 0:sessionsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", sessionsJpage + 1)
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
