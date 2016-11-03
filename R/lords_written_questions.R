
### 15 LORDS WRITTEN QUESTIONS

#' House of Lords Written Questions
#'
#' This imports data on House of Lords written questions
#' @param type The type of data you want, allows the arguments "all", "department" and "dates"
#' @param all Imports all available written questions
#' @param department Imports all available written questions answered by a given department
#' @param dates Imports all available written questions from between two given dates
#' @keywords House of Lords Written Questions
#' @export
#' @examples
#' x <- lords_written_questions("all")
#'
#'
#' x <- lords_written_questions("department")
#'
#'
#' x <- lords_written_questions("dates")

lords_written_questions <- function(type =c("all", "department", "dates")) {

  match.arg(type)

  if(type=="all") {

    baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions.json"

    writ <- jsonlite::fromJSON(baseurl_writ)

    writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:writJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", writJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="department") {

    answering.department <- readline("Enter the name of the answering department: ")

    baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/answeringdepartment.json?q="

    writ <- jsonlite::fromJSON(paste0(baseurl_writ,answering.department,"&pageSize=500"))

    writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:writJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", writJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="dates") {

    start.date <- readline("Enter start date(yyyy-mm-dd): ")

    end.date <- readline("Enter end date (yyyy-mm-dd): ")

    baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/tabled.json?startDate="

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
