### Commons Questions


### 1 ORAL QUESTIONS


#' House of Commons Oral Questions
#'
#' This imports data on House of Commons oral questiosn
#' @param all Imports all available oral questions. Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_oral_questions()


## To DO!: Add functionality for date, time, etc of questions, the IF/ELSE bits

commons_oral_questions <- function(type=c("all","times")) {

    match.arg(type)

    if(type=="all") {

    baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500"

    oral <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500")

    # if(numpages=TRUE){
    oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
    # }else { oralJpage <- numpages }
    pages <- list()

    for (i in 0:oralJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_oral, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", oralJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

    } else if(type=="times"){

      baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500"

      oral <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500")

      # if(numpages=TRUE){
      oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
      # }else { oralJpage <- numpages }
      pages <- list()

      for (i in 0:oralJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_oral, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", oralJpage+1)
        pages[[i + 1]] <- mydata$result$items
      }
    }
  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}




