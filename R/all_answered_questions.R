
#' all_answered_questions
#'
#' Imports data on answered parliamentary questions
#' @param mp_id Accepts a member ID, and returns a data frame with all available questions asked by that member. If NULL, returns  a data frame with all available answered questions.
#' @keywords Answered Questions
#' @import httr plyr jsonlite
#' @export
#' @examples \dontrun{
#'
#'x <- all_answered_questions(172)
#'
#' }

all_answered_questions <- function(mp_id = NULL) {
    
    if (is.null(mp_id) == TRUE) {
        
        baseurl <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"
        
        message("Connecting to API")
        
        all <- jsonlite::fromJSON("http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500")
        
        jpage <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else {
        
        mp_id <- as.character(mp_id)
        
        baseurl <- "http://lda.data.parliament.uk/questionsanswers.json?_pageSize=500&mnisId="
        
        message("Connecting to API")
        
        all <- jsonlite::fromJSON(paste0(baseurl, mp_id))
        
        if (all$result$totalResults/all$result$itemsPerPage > 1) {
            
            jpage <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)
            
        } else {
            
            jpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_page=", i), flatten = TRUE)
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
