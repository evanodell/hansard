
### 7 ORAL QUESTIONS TIMES

#' House of Commons Oral Question Times
#'
#' This imports data on House of Commons oral question times
#' @param all Imports all available bills Defaults to TRUE.
#' @keywords Oral Questions
#' @export
#' @examples
#' commons_oral_question_times()

commons_oral_question_times <- function(all = TRUE) {

  baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json"

  oralTimes <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json")

  oralTimesJpage <- round(oralTimes$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:oralTimesJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", oralTimesJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
