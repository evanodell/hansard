### 2 WRITTEN QUESTIONS

#' House of Commons Bills Written Questions
#'
#' This imports data on House of Commons written questions
#' @param all Imports all available written questions Defaults to TRUE.
#' @keywords Written Questions
#' @export
#' @examples
#' commons_written_questions()



commons_written_questions <- function(all = TRUE) {

  baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  writ <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  writJpage <- round(writ$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:writJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i)
    pages[[i + 1]] <- mydata$result$items
  }
}
