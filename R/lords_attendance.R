
#' lords_attendance
#'
#' Imports data on House of Lords attendance. Please note that the attendance data frames are not as tidy as some of the others that are accessible through this API.
#' @param session_id The ID of the House of Lords session. If NULL, returns a list of all sessions. Defaults to NULL.
#' @keywords House of Lords Attendance
#' @export
#' @examples \dontrun{
#'
#' x <- lords_attendance(session_id = 706178)
#' }
#'
lords_attendance <- function(session_id = NULL) {
    
    if (is.null(session_id) == FALSE) {
        query <- paste0("resources/", session_id, ".json")
    } else {
        query <- "lordsattendances.json?_pageSize=500"
    }
    
    baseurl <- "http://lda.data.parliament.uk/"
    
    message("Connecting to API")
    
    attend <- jsonlite::fromJSON(paste0(baseurl, query), flatten = TRUE)
    
    if (is.null(session_id) == FALSE) {
        
        df <- as.data.frame(attend$result$primaryTopic)
        
    } else {
        
        jpage <- round(attend$result$totalResults/attend$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_page=", i), flatten = TRUE)
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

