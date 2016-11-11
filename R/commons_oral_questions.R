
#' House of Commons Oral Questions
#'
#' Imports data on House of Commons oral questions
#' @param comsOralType The type of data you want, allows the arguments 'all' and 'times'
#' @param all Imports all available oral questions
#' @param times Imports the times of all available oral questions
#' @keywords bills
#' @export
#' @examples \donttest{
#' ### NOT RUN:
#' # x <- commons_oral_questions("all")
#' #
#' # x <- commons_oral_questions("times")
#' }



commons_oral_questions <- function(comsOralType = c("all", "times")) {

    match.arg(comsOralType)

    if (comsOralType == "all") {

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500"

        oral <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500")

        # if(numpages=TRUE){
        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        # }else { oralJpage <- numpages }
        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "times") {

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500"

        oral <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500")

        # if(numpages=TRUE){
        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        # }else { oralJpage <- numpages }
        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
}




