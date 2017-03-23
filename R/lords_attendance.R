
#' lords_attendance
#'
#' Imports data on House of Lords attendance. Please note that the attendance data frames are not as tidy as some of the others that are accessible through this API.
#' @param session_id The ID of the House of Lords session. If NULL, returns a list of all sessions. Defaults to NULL.
#' @param start_date The earliest date to include in the data frame. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords House of Lords Attendance
#' @export
#' @examples \dontrun{
#'
#' x <- lords_attendance(session_id = 706178)
#' }
#'
lords_attendance <- function(session_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, 
    tidy = TRUE) {
    
    if (is.null(session_id) == FALSE) {
        query <- paste0("/", session_id, ".json?")
    } else {
        query <- ".json?_pageSize=500"
    }
    
    dates <- paste0("&max-date=", end_date, "&min-date=", start_date)
    
    baseurl <- "http://lda.data.parliament.uk/lordsattendances"
    
    message("Connecting to API")
    
    attend <- jsonlite::fromJSON(paste0(baseurl, query, dates, extra_args), flatten = TRUE)
    
    if (is.null(session_id) == FALSE) {
        
        df <- as.data.frame(attend$result$primaryTopic)
        
    } else {
        
        jpage <- round(attend$result$totalResults/attend$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, dates, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- dplyr::bind_rows(pages)
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            df <- hansard_tidy(df)
            
            df
            
        } else {
            
            df
            
        }
        
    }
}

