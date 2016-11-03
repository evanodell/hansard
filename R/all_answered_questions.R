
#' Parliamentary Answered Questions
#'
#' This imports data on answered parliamentary questions
#' @param all Imports data on all available answered questions Defaults to TRUE.
#' @keywords Answered Questions
#' @export
#' @examples
#' all_answered_questions(all=TRUE)
#' #Returns all answered questions


### 18 ALL ANSWERED QUESTIONS - Not Done

all_answered_questions <- function(all = TRUE) {

  baseurl_allAnswered <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"

  allAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500")

  allAnsweredJpage <- round(allAnswered$result$totalResults/allAnswered$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:allAnsweredJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", allAnsweredJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
