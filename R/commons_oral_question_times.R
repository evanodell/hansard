
#' commons_oral_question_times
#'
#' Imports data on House of Commons oral question times
#' @param cOralTimeType Accepts the arguments 'all' and 'ID'
#' @param all Returns a data frame with all of the oral question times.
#' @param ID Requests a requestion time ID, and returns a data frame of that question time.
#' @param session Requests a session in format yyyy/yy (e.g. 2016/17) and returns a data frame of all oral question times from that session
#' @keywords Oral Questions Time
#' @export
#' @examples \dontrun{
#' x <- commons_oral_question_times('all')
#'
#' x <- commons_oral_question_times('ID')
#'
#' x <- commons_oral_question_times('session')
#' }

commons_oral_question_times <- function(cOralTimeType = c("all", "ID", "session")) {

    match.arg(cOralTimeType)

    if (cOralTimeType == "all") {

        baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500"

        message("Connecting to API")

        oralTimes <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500")

        oralTimesJpage <- round(oralTimes$result$totalResults/oralTimes$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralTimesJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralTimesJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (cOralTimeType == "ID") {

        timeID <- readline("Please enter a question time ID: ")

        baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes/"

        message("Connecting to API")

        oralTimes <- jsonlite::fromJSON(paste0(baseurl_oralTimes, timeID, ".json"))

        mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, timeID, ".json"), flatten = TRUE)

        df <- mydata$result$primaryTopic

        df <- as.data.frame(df)

    } else if (cOralTimeType == "session") {

        sessionID <- readline("Please enter the session ID (yyyy/yy): ")

        sessionID <- URLencode(sessionID)

        baseurl_oralTimes <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?session="

        message("Connecting to API")

        oralTimes <- jsonlite::fromJSON(paste0(baseurl_oralTimes, sessionID, "&_pageSize=500"))

        if (oralTimes$result$totalResults > oralTimes$result$itemsPerPage) {

            oralTimesJpage <- round(oralTimes$result$totalResults/oralTimes$result$itemsPerPage, digits = 0)

        } else {
            oralTimesJpage <- 0
        }

        pages <- list()

        for (i in 0:oralTimesJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, sessionID, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralTimesJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
