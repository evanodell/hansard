

#' commons_answered_questions
#'
#' Imports data on House of Commons answered questions
#' @param comsAnsType The type of data you want, allows the arguments 'all', 'date', 'department', 'answeredBy', 'recent'
#' @param all Returns a data frame with all answered questions in the House of Commons
#' @param date Returns a data frame with all answered questions in the House of Commons on the given date
#' @param department Returns a data frame with all answered questions in the House of Commons from the given department
#' @param answeredBy Returns a data frame with all answered questions in the House of Commons by the given MP
#' @keywords bills
#' @export
#' @examples \dontrun{
#' x <- commons_answered_questions('all')
#'
#' x <- commons_answered_questions('date')
#'
#' x <- commons_answered_questions('department')
#'
#' x <- commons_answered_questions('answeredBy')
#' }

commons_answered_questions <- function(comsAnsType = c("all", "date", "department", "answeredBy")) {

    match.arg(comsAnsType)

    if (comsAnsType == "all") {

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

        comAnswered <- jsonlite::fromJSON(baseurl_comAnswered)

        message("Connecting to API")

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsAnsType == "date") {

        qDate <- readline("Enter date (yyyy-mm-dd): ")
        qDate <- URLencode(qDate)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?date="

        message("Connecting to API")

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500"))

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsAnsType == "department") {

        qDepartment <- readline("Enter department: ")
        qDepartment <- URLencode(qDepartment)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeringdepartment.json?q="

        message("Connecting to API")

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDepartment, "&_pageSize=500"))

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDepartment, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (comsAnsType == "answeredBy") {

        qAnsweredBy <- readline("Enter MP ID: ")
        qAnsweredBy <- URLencode(qAnsweredBy)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

        message("Connecting to API")

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json?_pageSize=500"))

        if(comAnswered$result$totalResults>comAnswered$result$itemsPerPage){

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        } else {
          comAnsweredJpage <- 0
        }

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json?_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
