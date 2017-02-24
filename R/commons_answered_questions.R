

#' commons_answered_questions
#'
#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a data frame.
#' @param date Returns a data frame with all answered questions in the House of Commons on the given date, in 'yyyy-mm-dd' format. Defaults to NULL.
#' @param department Returns a data frame with all answered questions in the House of Commons from the given department. Defaults to NULL.
#' @param answered_by Returns a data frame with all answered questions in the House of Commons by the given MP. Defaults to NULL.
#' @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(date = '2017-02-23', department = 'health', answered_by = '4019')
#'
#' }


commons_answered_questions <- function(date = NULL, department = NULL, answered_by = NULL) {
    
    if (is.null(date) == FALSE) {
        date <- as.character(date)
        date <- paste0("dateOfAnswer=", date)
    }
    
    if (is.null(department) == TRUE & is.null(answered_by) == TRUE) {
        
        if (is.null(date) == FALSE) {
            date <- paste0("&", date)
        }
        
        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"
        
        message("Connecting to API")
        
        answered <- jsonlite::fromJSON(paste0(baseurl, date), flatten = TRUE)
        
        jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, date, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
        
        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {
            df
        }
        
    } else if (is.null(department) == FALSE) {
        
        if (is.null(date) == FALSE) {
            date <- paste0("&", date)
        }
        
        ### Search by member id
        if (is.null(answered_by) == FALSE) {
            
            name_lookup <- paste0("http://lda.data.parliament.uk/members/", answered_by, ".json")
            
            member <- jsonlite::fromJSON(name_lookup, flatten = TRUE)
            
            member <- member$result$primaryTopic$fullName$`_value`
            
            answered_by <- paste0("&answeringMemberPrinted=", member)
            
            answered_by <- utils::URLencode(answered_by)
            
        }
        
        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions/answeringdepartment.json?q="
        
        message("Connecting to API")
        
        answered <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500"), flatten = TRUE)
        
        if (answered$result$totalResults > answered$result$itemsPerPage) {
            
            jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)
            
            pages <- list()
            
            for (i in 0:jpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500", "&_page=", 
                  i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", jpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }
            
            df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
            
            if (nrow(df) == 0) {
                message("The request did not return any data. Please check your search parameters.")
            } else {
                df
            }
            
        } else {
            
            message("Retrieving page 1 of 1")
            
            mydata <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500"), flatten = TRUE)
            
            df <- mydata$result$items
            
        }
        
    } else if (is.null(answered_by) == FALSE) {
        
        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"
        
        message("Connecting to API")
        
        answered <- jsonlite::fromJSON(paste0(baseurl, answered_by, ".json?", date, "&_pageSize=500"))
        
        jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, answered_by, ".json?", date, "&_pageSize=500&_page=", i), flatten = TRUE)
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
    
}

