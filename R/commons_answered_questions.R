
## Start and end dates do not appear to be working on this API as of 2017-02-24


#' commons_answered_questions
#'
#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a data frame.
#' @param answering_department Returns a data frame with all answered questions in the House of Commons from the given department. Defaults to NULL.
#' @param answered_by Returns a data frame with all answered questions in the House of Commons by the given MP. Defaults to NULL.
#' @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(answering_department = 'health', answered_by = '4019')
#'
#' }
## Still need to check

commons_answered_questions <- function(answering_department = NULL, answered_by = NULL) {
    
    
    # dates <-paste0('&_properties=dateOfAnswer&max-dateOfAnswer=',end_date, '&min-dateOfAnswer=',start_date)
    
    if (is.null(answered_by) == FALSE) {
        answered_by <- paste0("&answeringMember=http://data.parliament.uk/members/", answered_by)
    }
    
    
    if (is.null(answering_department) == FALSE) {
        
        query <- "/answeringdepartment"
        
        answering_department <- paste0("q=", answering_department)
        
    } else {
        
        query <- NULL
        
    }
    baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions"
    
    message("Connecting to API")
    
    answered <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500"), flatten = TRUE)
    
    jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, answered_by, "&_pageSize=500&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}


