### Commons Questions


### 1 ORAL QUESTIONS


#' House of Commons Oral Questions
#'
#' This imports data on House of Commons oral questiosn
#' @param all Imports all available oral questions. Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#'
#' commons_questions("written")
#'
#'
#'
#' commons_questions("oral")


## To DO!: Add functionality for date, time, etc of questions, the IF/ELSE bits

commons_questions <- function(type=c("written","oral")) {

    baseurl_questions <- "http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500"

    questions <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500")

    # if(numpages=TRUE){
    questionsJpage <- round(questions$result$totalResults/questions$result$itemsPerPage, digits = 0)
    # }else { oralJpage <- numpages }
    pages <- list()

    for (i in 0:oralJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_questions, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", questionsJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }
}




