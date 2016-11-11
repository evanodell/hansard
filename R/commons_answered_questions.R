

#' House of Commons Answered Questions
#'
#' Imports data on House of Commons answered questions
#' @param comsAnsType The type of data you want, allows the arguments 'all', 'date', 'department', 'answeredBy', 'recent'
#' @param all Imports all available answered questions
#' @param date Imports all available answered questions on a particular date
#' @param department Imports all available answered questions by answering department
#' @param answeredBy Imports all available answered questions by answering MP
#' @keywords bills
#' @export
#' @examples \donttest{
#' #### NOT RUN:
#' # x <- commons_answered_questions('all')
#' # Returns a data frame with all answered questions in the House of Commons
#'
#' # x <- commons_answered_questions('date')
#' # Returns a data frame with all answered questions in the House of Commons on the given date
#'
#' # x <- commons_answered_questions('department')
#' # Returns a data frame with all answered questions in the House of Commons from the given department
#'
#' # x <- commons_answered_questions('answeredBy')
#' # Returns a data frame with all answered questions in the House of Commons by the given MP
#' }

commons_answered_questions <- function(comsAnsType = c("all", "date", "department", "answeredBy")) {

    match.arg(comsAnsType)

    if (comsAnsType == "all") {
        ## WORKING!
        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

        comAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500")

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsAnsType == "date") {
        ## WORKING!

        qDate <- readline("Enter date (yyyy-mm-dd): ")
        qDate <- URLencode(qDate)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?date="

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500"))

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDate, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        # } else if (comsAnsType=='uin'){ ##UNKNOWN qUin <- readline('Enter the question's UIN: ') baseurl_comAnswered <-
        # 'http://lda.data.parliament.uk/commonsansweredquestions.json' comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered,
        # '?uin=', qUin)) MAYBE ONE DAY I'LL GET AROUND TO ADDING THE ABILITY TO SEARCH BY UIN

    } else if (comsAnsType == "department") {

        qDepartment <- readline("Enter department: ")
        qDepartment <- URLencode(qDepartment)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeringdepartment.json?q="

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDepartment))

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDepartment, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (comsAnsType == "answeredBy") {

        qAnsweredBy <- readline("Enter MP ID: ")
        qAnsweredBy <- URLencode(qAnsweredBy)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json"))

        comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json", "?_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
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
