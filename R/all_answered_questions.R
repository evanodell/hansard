
#' Parliamentary Answered Questions
#'
#' This imports data on answered parliamentary questions
#' @param all Imports data on all available answered questions Defaults to TRUE.
#' @keywords Answered Questions
#' @export
#' @examples
#' all_answered_questions()


### 18 ALL ANSWERED QUESTIONS

all_answered_questions <- function(all = TRUE) {

  baseurl_allAnswered <- "http://lda.data.parliament.uk/answeredquestions.json"

  allAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/answeredquestions.json")

  allAnsweredJpage <- round(allAnswered$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:allAnsweredJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", allAnsweredJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
