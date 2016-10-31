### 6 COMMONS ANSWERED QUESTIONS

#' House of Commons Answered Questions
#'
#' This imports data on House of Commons answered questions
#' @param all Imports all available answered questions Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_answered_questions()


commons_answered_questions <- function(all = TRUE) {

  baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json"

  comAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsansweredquestions.json")

  comAnsweredJpage <- round(comAnswered$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:comAnsweredJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", comAnsweredJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
