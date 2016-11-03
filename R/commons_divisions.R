
### 3 Commons DIVISIONS - Mostly Done

#' House of Commons Divisions
#'
#' This imports data on House of Commons divisions
#' @param all Imports all available divisions. Defaults to TRUE.
#' @keywords divisions
#' @export
#' @examples
#' commons_divisions("all")
#'
#' commons_divisions("date")
#'
#' commons_divisions("no")
#'
#' commons_divisions("yes")
#'
#'

commons_divisions <- function(type =c("all", "date", "no",
                                      "yes")) {

  match.arg(type)

  if(type=="all") {

    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500"

    divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500")

    divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:divisJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", divisJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

  } else if (type=="date") {

    divis_date <- readline("Select division date: ")

    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/date/"

    divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/",divis_date,".json?_pageSize=500"))

    if(divis$result$itemsPerPage>divis$result$totalResults){
      divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
    } else {
      divisJpage <-0
    }

    pages <- list()

    for (i in 0:divisJpage) {
      mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/",divis_date,".json?_pageSize=500", "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", divisJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }  else if (type=="no") {

    mp.id <- readline("Enter Member ID: ")

    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

    divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId=",mp.id,"&_pageSize=500"))

    divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:divisJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", divisJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }  else if (type=="yes") {

    mp.id <- readline("Enter Member ID: ")

    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

    divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=",mp.id,"&_pageSize=500"))

    divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:divisJpage) {
      mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=",mp.id,"&_pageSize=500", "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", divisJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }
  }  #else if (type=="session") {

#    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500"

#    divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500")

#    divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

#    pages <- list()

#    for (i in 0:divisJpage) {
#      mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "&_page=", i), flatten = TRUE)
#      message("Retrieving page ", i+1, " of ", divisJpage+1)
#      pages[[i + 1]] <- mydata$result$items
#    }
#  }
  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}



