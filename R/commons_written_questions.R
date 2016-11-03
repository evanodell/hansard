### 2 WRITTEN QUESTIONS

#' House of Commons Bills Written Questions
#'
#' This imports data on House of Commons written questions
#' @param type The type of data you want, allows the arguments
#' @param all Imports all available written questions Defaults to TRUE.
#' @keywords Written Questions
#' @export
#' @examples
#' x <- commons_written_questions("all")
#' #Returns all written questions
#'
#' x <- commons_written_questions("department")
#' #Returns all written questions by department. The query acts as a search, so entering <health>
#' # will return all questions answered by the Department of Health
#'
#'
#' x <- commons_written_questions("dates")
#' #Returns all written questions tabled between two chosen dates
#'

commons_written_questions <- function(type =c("all","department","dates")) {

  match.arg(type)

  if(type=="all") {

  baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  writ <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:writJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", writJpage+1)
    pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="department") {

    answering.department <- readline("Enter the name of the answering department: ")

    baseurl_writ <- "commonswrittenquestions/answeringdepartment.json?q="

    writ <- jsonlite::fromJSON(paste0(baseurl_writ,answering.department,"&pageSize=500"))

    writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:writJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", writJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="dates") {

    start.date <- readline("Enter start date: ")

    end.date <- readline("Enter end date: ")

    baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions/tabled.json?startDate="

    writ <- jsonlite::fromJSON(paste0(baseurl_writ,start.date,"&endDate=", end.date,"&_pageSize=500"))

    writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:writJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_writ,start.date,"&endDate=", end.date,"&_pageSize=500", "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", writJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
