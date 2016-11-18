
#' all_answered_questions
#'
#' Imports data on answered parliamentary questions
#' @param allAnsType The type of questions you want, accepts the arguments 'all' and 'askedBy'
#' @param all Returns a data frame with all available answered questions.
#' @param askedBy Requests a member ID, and returns a data frame with all available questions asked by that member.
#' @keywords Answered Questions
#' @import httr plyr jsonlite
#' @export
#' @examples \dontrun{
#'
#'x <- all_answered_questions('all')
#'
#' x <- all_answered_questions('askedBy')
#' }

all_answered_questions <- function(allAnsType = c("all", "askedBy")) {
    
    match.arg(allAnsType)
    
    if (allAnsType == "all") {
        
        baseurl_allAnswered <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"
        
        allAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500")
        
        allAnsweredJpage <- round(allAnswered$result$totalResults/allAnswered$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:allAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (allAnsType == "askedBy") {
        
        mp.id <- readline("Enter Member ID: ")
        mp.id <- URLencode(mp.id)
        
        baseurl_allAnswered <- "http://lda.data.parliament.uk/questionsanswers.json?_pageSize=500&mnisId="
        
        allAnswered <- jsonlite::fromJSON(paste0(baseurl_allAnswered, mp.id))
        
        if (allAnswered$result$totalResults/allAnswered$result$itemsPerPage > 1) {
            
            allAnsweredJpage <- round(allAnswered$result$totalResults/allAnswered$result$itemsPerPage, digits = 0)
            
        } else {
            allAnsweredJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:allAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, mp.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allAnsweredJpage + 1)
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
