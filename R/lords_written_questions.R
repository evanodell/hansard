
#' lords_written_questions
#'
#' Imports data on House of Lords written questions
#' @param peer_id Requests a member ID and returns a data frame with all written questions asked by that member.
#' @param answering_department Accepts a string with a department name or partial name, and returns all written questions by that department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health.
#' @param start_date The earliest date to include in the data frame, if calling all divisions, using the date the question was tabled. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame, if calling all divisions, using the date the question was tabled. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords House of Lords Written Questions
#' @export
#' @examples \dontrun{
#' # Returns all written questions ever
#' x <- lords_written_questions()
#'
#' x <- lords_written_questions(peer_id = 3526, answering_department = 'cabinet')
#'
#' }

lords_written_questions <- function(peer_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), 
    extra_args = NULL, tidy = TRUE) {
    
    dates <- paste0("&_properties=dateTabled&max-dateTabled=", end_date, "&min-dateTabled=", start_date)
    
    if (is.null(peer_id) == FALSE) {
        peer_id <- paste0("&tablingMember=http://data.parliament.uk/members/", peer_id)
        
        peer_id <- utils::URLencode(peer_id)
    }
    
    if (is.null(answering_department) == FALSE) {
        
        query <- "/answeringdepartment"
        
        answering_department <- paste0("q=", answering_department)
        
        answering_department <- utils::URLencode(answering_department)
        
    } else {
        
        query <- NULL
        
    }
    
    baseurl <- "http://lda.data.parliament.uk/lordswrittenquestions"
    
    message("Connecting to API")
    
    writ <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, peer_id, dates, "&_pageSize=500", 
        extra_args), flatten = TRUE)
    
    jpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, peer_id, dates, "&_pageSize=500", 
            i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- dplyr::bind_rows(pages)
    
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
