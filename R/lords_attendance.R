### 12 LORDS ATTENDANCE


#' House of Lords Attendance
#'
#' This imports data on House of Lords attendance
#' @param type Accepts arguments "all" and "date"
#' @param all Imports all available House of Lords attendance records
#' @param date Imports all available House of Lords attendance records for a given date
#' @keywords House of Lords Attendance
#' @export
#' @examples
#' x <- lords_attendance("all")
#' #Returns all House of Lords attendance
#'
#' x <- lords_attendance("date")
#' #Returns House of Lords attendance data for a given date

lords_attendance <- function(type =c("all","date")) {

  match.arg(type)

  if(type=="all") {

    baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500"

    lordsAttend <- jsonlite::fromJSON(" http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500")

    lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsAttendJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", lordsAttendJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }
  } else if(type=="date") {

    attend.date <- readline("Enter date (yyyy-mm-dd): ")

    baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances/date/"

    lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend,attend.date,".json?_pageSize=500"))

    pages <- list()

    for (i in 0:0) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", 1)
      pages[[i + 1]] <- mydata$result$items
    }
#  } else if(type=="ID") {

# Lords.ID <- readline("Enter lords ID number: ")

#    baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances.json?mnisId="

#    lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend,lords.ID,"&_pageSize=500"))

#    lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

#    pages <- list()

#    for (i in 0:lordsAttendJpage) {
#      mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
#      message("Retrieving page ", i+1, " of ", lordsAttendJpage+1)
#      pages[[i + 1]] <- mydata$result$items
#    }
  }



  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}

