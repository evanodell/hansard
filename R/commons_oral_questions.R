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

commons_oral_questions <- function(all = TRUE) {

    baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json"

    oral <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

    # if(numpages=TRUE){
    oralJpage <- round(oral$result$totalResults/10 + 1, digits = 0)
    # }else { oralJpage <- numpages }

    pages <- list()

    for (i in 0:oralJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_oral, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}




