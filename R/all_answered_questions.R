
#' Parliamentary Answered Questions
#'
#' This imports data on answered parliamentary questions
#' @param allAnsType The type of questions you want, accepts the arguments 'all' and 'askedBy'
#' @param all Imports data on all available answered questions.
#' @param askedBy Requests a member ID, and imports data on all available questions asked by that member
#' @keywords Answered Questions
#' @export
#' @examples \donttest{
#' x <- all_answered_questions('all')
#' #Returns all answered questions
#' #NOT RUN
#' # x <- all_answered_questions('all')
#' # head(x)
#'
#' x <- all_answered_questions('askedBy')
#' #NOT RUN
#' # x <- all_answered_questions('askedBy')
#' # ### RETURNS:
#' # Enter Member ID: 172
#' # head(x) #All questions asked by Diane Abbott }


### 18 ALL ANSWERED QUESTIONS - Not Done

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
    
}
