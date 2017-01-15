
#' commons_oral_questions
#'
#' Imports data on House of Commons oral questions
#' @param comsOralType The type of data you want, allows the arguments 'all', 'times', 'daysTabled', 'daysAnswered', 'askedBy' and 'session'
#' @param all Imports all available oral questions
#' @param times Imports the times of all available oral questions
#' @param daysTabled Requests two dates, and returns a data frame with all oral questions asked between those two dates
#' @param daysAnswered Requests two dates, and returns a data frame with all oral questions answered between those two dates
#' @param askedBy Requests a member ID and returns a data frame with all oral questions asked by that member
#' @param session Requests a session ID and returns a data frame with all oral questions asked in that session
#' @param department Requests a Requests a department name, and returns all oral questions answered by that department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health
#' @keywords bills
#' @export
#' @examples \dontrun{
#' x <- commons_oral_questions('all')
#'
#' x <- commons_oral_questions('times')
#'
#' x <- commons_oral_questions('daysTabled')
#'
#' x <- commons_oral_questions('daysAnswered')
#'
#' x <- commons_oral_questions('askedBy')
#'
#' x <- commons_oral_questions('session')
#'
#' x <- commons_oral_questions('department')
#'
#' }

commons_oral_questions <- function(comsOralType = c("all", "times", "daysTabled", "daysAnswered", "askedBy", "session", "department")) {

    match.arg(comsOralType)

    if (comsOralType == "all") {

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500"

        message("Connecting to API")

        oral <- jsonlite::fromJSON(baseurl_commons_oral_questions)

        # if(numpages=TRUE){
        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        # }else { oralJpage <- numpages }
        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_commons_oral_questions, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "times") {

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestiontimes.json?_pageSize=500"

        message("Connecting to API")

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

    } else if (comsOralType == "daysTabled") {

        start.date <- readline("Enter start date (yyyy-mm-dd): ")

        end.date <- readline("Enter end date (yyyy-mm-dd): ")

        start.date <- URLencode(start.date)

        end.date <- URLencode(end.date)

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestions/tabled.json?startDate="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=",
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "daysAnswered") {

        start.date <- readline("Enter start date (yyyy-mm-dd): ")

        end.date <- readline("Enter end date (yyyy-mm-dd): ")

        start.date <- URLencode(start.date)

        end.date <- URLencode(end.date)

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestions/answerDate.json?startDate="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=",
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "askedBy") {

        mp.id <- readline("Enter Member ID: ")

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, mp.id, "&_pageSize=500"))

        if (oral$result$totalResults > oral$result$itemsPerPage) {
            oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        } else {
            oralJpage <- 0
        }

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "session") {

        oral_session <- readline("Enter session (yyyy/yy): ")
        oral_session <- URLencode(oral_session)

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsdivisions.json?session="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, oral_session, "&_pageSize=500"))

        if (oral$result$itemsPerPage < oral$result$totalResults) {
            oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        } else {
            oralJpage <- 0
        }

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, oral_session, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (comsOralType == "department") {

        answering.department <- readline("Enter the name of the answering department: ")
        answering.department <- URLencode(answering.department)

        baseurl_commons_oral_questions <- "http://lda.data.parliament.uk/commonsoralquestions/answeringdepartment.json?q="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, answering.department, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, answering.department, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}




