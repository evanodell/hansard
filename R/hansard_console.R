#' hansard_console
#'
#' Code that powers the `hansard_basic` function
#' @keywords internal Hansard API
#' @noRd
 #' @examples \dontrun{
#' }

#' all_answered_questions
#'
#' @param allAnsType The type of questions you want, accepts the arguments 'all' and 'askedBy'
#' @param all Returns a data frame with all available answered questions.
#' @param askedBy Requests a member ID, and returns a data frame with all available questions asked by that member.
#' @keywords internal Answered Questions
#' @noRd
#' @examples \dontrun{
#'
#'x <- all_answered_questions('all')
#'
#' x <- all_answered_questions('askedBy')
#' }

#'
all_answered_questions_console <- function(allAnsType = c("all", "askedBy")) {

    match.arg(allAnsType)

    if (allAnsType == "all") {

        baseurl_allAnswered <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"

        message("Connecting to API")

        allAnswered <- jsonlite::fromJSON("http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500")

        allAnsweredJpage <- round(allAnswered$result$totalResults/allAnswered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (allAnsType == "askedBy") {

        mp.id <- readline("Enter Member ID: ")
        mp.id <- URLencode(mp.id)

        baseurl_allAnswered <- "http://lda.data.parliament.uk/questionsanswers.json?_pageSize=500&mnisId="

        message("Connecting to API")

        allAnswered <- jsonlite::fromJSON(paste0(baseurl_allAnswered, mp.id))

        if (allAnswered$result$totalResults/allAnswered$result$itemsPerPage > 1) {

            allAnsweredJpage <- round(allAnswered$result$totalResults/allAnswered$result$itemsPerPage, digits = 0)

        } else {
            allAnsweredJpage <- 0
        }

        pages <- list()

        for (i in 0:allAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allAnswered, mp.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allAnsweredJpage + 1)
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




#' commons_terms_console
#'
#' @param termsType The type of data you want, allows the argument 'all'
#' @param all Returns a data frame with all definitions in the parliamentary thesaurus


#' @keywords internal parliamentary thesaurus
#' @noRd
#' @examples \dontrun{
#' x <- commons_terms_console('all')
#' head(x)
#'  }
commons_terms_console <- function(termsType = c("all")) {

    match.arg(termsType)

    if (termsType == "all") {

        baseurl_terms <- "http://lda.data.parliament.uk/terms.json?_pageSize=500"

        message("Connecting to API")

        cTerms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json?_pageSize=500")

        cTermsJpage <- round(cTerms$result$totalResults/cTerms$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:10) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_terms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", cTermsJpage + 1)
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



#' bills_console
#'
#' Requests data on House of Commons and House of Lords bills
#' @param billType The type of data you want, allows the arguments 'ammended', 'publications' and 'stageTypes'
#' @param ammended Imports a data frame with all ammended bills
#' @param stageTypes Imports a data frame with all bill stage types
#' @param publications Imports a data frame with all bill publications
#' @keywords internal bills

#' @noRd
#' @examples \dontrun{
#' x <- bills('all')
#'
#' x <- bills('ammended')
#'
#' x <- bills('stageTypes')
#' }
#' @note There are problems with the Bills API, as the JSON data available for some queries, including the query to return all bills currently before the house, is inconsistently formatted and cannot be parsed into a data frame.


bills_console <- function(billType = c("ammended", "stageTypes", "publications")) {

    match.arg(billType)

    if (billType == "ammended") {
        # Working but return is weird

        baseurl_bills <- "http://lda.data.parliament.uk/billswithamendments.json?_pageSize=500"

        message("Connecting to API")

        bills <- jsonlite::fromJSON(baseurl_bills)

        if (bills$result$totalResults > bills$result$itemsPerPage) {

            billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

        } else {
            billsJpage <- 0
        }

        pages <- list()

        for (i in 0:billsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", billsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (billType == "stageTypes") {

        baseurl_bills <- "http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500"

        message("Connecting to API")

        bills <- jsonlite::fromJSON(baseurl_bills)

        billsJpage <- 0

        pages <- list()

        for (i in 0:billsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", billsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (billType == "publications") {

        baseurl_bills <- "http://lda.data.parliament.uk/billpublications.json?_pageSize=500"

        message("Connecting to API")

        bills <- jsonlite::fromJSON(baseurl_bills)

        billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:billsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", billsJpage + 1)
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





#' commons_answered_questions_console
#'
#' Requests data on House of Commons answered questions
#' @param comsAnsType The type of data you want, allows the arguments 'all', 'date', 'department', 'answeredBy', 'recent'
#' @param all Returns a data frame with all answered questions in the House of Commons
#' @param date Returns a data frame with all answered questions in the House of Commons on the given date
#' @param department Returns a data frame with all answered questions in the House of Commons from the given department
#' @param answeredBy Returns a data frame with all answered questions in the House of Commons by the given MP
#' @keywords internal bills
#' @noRd
#' @examples \dontrun{
#' x <- commons_answered_questions('all')
#'
#' x <- commons_answered_questions('date')
#'
#' x <- commons_answered_questions('department')
#'
#' x <- commons_answered_questions('answeredBy')
#' }

commons_answered_questions_console <- function(comsAnsType = c("all", "date", "department", "answeredBy")) {

    match.arg(comsAnsType)

    if (comsAnsType == "all") {

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

        message("Connecting to API")

        comAnswered <- jsonlite::fromJSON(baseurl_comAnswered)

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
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qDepartment, "&_pageSize=500&_page=", i),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", comAnsweredJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (comsAnsType == "answeredBy") {

        qAnsweredBy <- readline("Enter MP ID: ")
        qAnsweredBy <- URLencode(qAnsweredBy)

        baseurl_comAnswered <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

        message("Connecting to API")

        comAnswered <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json?_pageSize=500"))

        if (comAnswered$result$totalResults > comAnswered$result$itemsPerPage) {

            comAnsweredJpage <- round(comAnswered$result$totalResults/comAnswered$result$itemsPerPage, digits = 0)

        } else {
            comAnsweredJpage <- 0
        }

        pages <- list()

        for (i in 0:comAnsweredJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_comAnswered, qAnsweredBy, ".json?_pageSize=500&_page=",
                i), flatten = TRUE)
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


#' commons_divisions_console
#'
#' Requests data on House of Commons divisions
#' @param comsDivType The type of data you want, allows the arguments 'all', 'date', 'aye', 'no', 'voteSummary', 'voteFull', 'uinSummary', 'uinFull' and 'session'. Defaults to 'all'.
#' @param all Returns a data frame with all available divisions.
#' @param date Requests a date in yyyy-mm-dd format and returns a data frame with all available divisions on that date.
#' @param aye Returns a data frame with all divisions where a given MP voted aye.
#' @param no Returns a data frame with all divisions where a given MP voted no.
#' @param voteSummary Requests a division ID, and returns a summary of results of that division in a data frame.
#' @param voteFull Requests a division ID, and returns a data frame with details on how each individual member voted.
#' @param uinSummary Requests a division UIN and returns a data frame with a summary of results of that division.
#' @param uinFull Requests a division UIN and returns a data frame with the full results of that division.
#' @param session Requests a session in yyyy/yy format (e.g. 2016/17) and returns a data frame with all divisions in that session.
#' @keywords internal divisions
#' @noRd
#' @examples \dontrun{
#' x <- commons_divisions_console('all')
#'
#' x <- commons_divisions_console('date')
#'
#' x <- commons_divisions_console('no')
#'
#' x <- commons_divisions_console('aye')
#'
#' x <- commons_divisions_console('voteSummary')
#'
#' x <- commons_divisions_console('voteFull')
#'
#' x <- commons_divisions_console('session')
#'
#' x <- commons_divisions_console('uinSummary')
#'
#' x <- commons_divisions_console('uinFull')
#' }

commons_divisions_console <- function(comsDivType = c("all", "date", "aye", "no", "voteSummary", "voteFull",
    "session", "uinSummary", "uinFull")) {

    match.arg(comsDivType)

    if (comsDivType == "all") {

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500"

        message("Connecting to API")

        divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500")

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "date") {

        divis_date <- readline("Enter division date (yyyy-mm-dd): ")
        divis_date <- URLencode(divis_date)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/date/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/", divis_date,
            ".json?_pageSize=500"))

        if (divis$result$itemsPerPage < divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, divis_date, ".json?_pageSize=500", "&_page=",
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "no") {

        mp.id <- readline("Enter Member ID: ")
        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId=",
            mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "aye") {

        mp.id <- readline("Enter Member ID: ")
        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=",
            mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "voteSummary") {

        vote.ID <- readline("Enter vote ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, vote.ID, ".json"))

        df <- divis$result$primaryTopic

        df$AbstainCount <- df$AbstainCount$`_value`
        df$AyesCount <- df$AyesCount$`_value`
        df$Didnotvotecount <- df$Didnotvotecount$`_value`
        df$Errorvotecount <- df$Errorvotecount$`_value`
        df$Noesvotecount <- df$Noesvotecount$`_value`
        df$Noneligiblecount <- df$Noneligiblecount$`_value`
        df$vote <- NULL
        df$Margin <- df$Margin$`_value`
        df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
        df$date <- df$date$`_value`

        df <- as.data.frame(df)

    } else if (comsDivType == "voteFull") {

        vote.ID <- readline("Enter vote ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, vote.ID, ".json"))

        df <- divis$result$primaryTopic$vote

        df

    } else if (comsDivType == "session") {

        divis_session <- readline("Enter session (yyyy/yy): ")
        divis_session <- URLencode(divis_session)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?session="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_session, "&_pageSize=500"))

        if (divis$result$itemsPerPage < divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, divis_session, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "uinSummary") {

        divis_uin <- readline("Enter division UIN (format: CD:yyyy-mm-dd:uin): ")
        divis_uin <- URLencode(divis_uin)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?uin="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_uin))

        df <- divis$result$items
        # Cleaning up the Data Frame
        df$AbstainCount <- df$AbstainCount$`_value`
        df$AyesCount <- df$AyesCount$`_value`
        df$Didnotvotecount <- df$Didnotvotecount$`_value`
        df$Errorvotecount <- df$Errorvotecount$`_value`
        df$Noesvotecount <- df$Noesvotecount$`_value`
        df$Noneligiblecount <- df$Noneligiblecount$`_value`
        df$vote <- NULL
        df$Margin <- df$Margin$`_value`
        df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
        df$date <- df$date$`_value`

        df <- as.data.frame(df)

        df

    } else if (comsDivType == "uinFull") {

        divis_uin <- readline("Enter division UIN: ")
        divis_uin <- URLencode(divis_uin)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?uin="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_uin, "&_pageSize=500"))

        df <- divis$result$items$vote

        df

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}


#' commons_division_date
#'
#' Accepts an ID number for a member of the house of commons, and returns a data frame of all divisions where they voted aye.
#' @param date The ID number of a member of the House of Commons.


#' @keywords internal divisions
#' @noRd
 #' @examples \dontrun{
#' x <- commons_division_date(2016-10-10)
#' }
#'


commons_division_date_console <- function(date) {

    baseurl_date <- "http://lda.data.parliament.uk/commonsdivisions/date/"

    message("Connecting to API")

    url_date <- jsonlite::fromJSON(paste0(baseurl_date, date, ".json?_pageSize=500"), flatten = TRUE)

    if (url_date$result$itemsPerPage < url_date$result$totalResults) {
        dateJPage <- round(url_date$result$totalResults/url_date$result$itemsPerPage, digits = 0)
    } else {
        dateJPage <- 0
    }

    pages <- list()

    for (i in 0:dateJPage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_date, date, "&_pageSize=500&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", dateJPage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}




#' commons_oral_question_times_console
#'
#' Requests data on House of Commons oral question times
#' @param cOralTimeType Accepts the arguments 'all' and 'ID'
#' @param all Returns a data frame with all of the oral question times.
#' @param ID Requests a requestion time ID, and returns a data frame of that question time.
#' @param session Requests a session in format yyyy/yy (e.g. 2016/17) and returns a data frame of all oral question times from that session



#' @keywords internal Oral Questions Time
#' @noRd
 #' @examples \dontrun{
#' x <- commons_oral_question_times_console('all')
#'
#' x <- commons_oral_question_times_console('ID')
#'
#' x <- commons_oral_question_times_console('session')
#' }

commons_oral_question_times_console <- function(cOralTimeType = c("all", "ID", "session")) {

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
            mydata <- jsonlite::fromJSON(paste0(baseurl_oralTimes, sessionID, "&_pageSize=500", "&_page=", i),
                flatten = TRUE)
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




#' commons_oral_questions_console
#'
#' Requests data on House of Commons oral questions
#' @param comsOralType The type of data you want, allows the arguments 'all', 'times', 'daysTabled', 'daysAnswered', 'askedBy' and 'session'
#' @param all Imports all available oral questions
#' @param times Imports the times of all available oral questions
#' @param daysTabled Requests two dates, and returns a data frame with all oral questions asked between those two dates
#' @param daysAnswered Requests two dates, and returns a data frame with all oral questions answered between those two dates
#' @param askedBy Requests a member ID and returns a data frame with all oral questions asked by that member
#' @param session Requests a session ID and returns a data frame with all oral questions asked in that session
#' @param department Requests a Requests a department name, and returns all oral questions answered by that department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health



#' @keywords internal bills
#' @noRd
 #' @examples \dontrun{
#' x <- commons_oral_questions_console('all')
#'
#' x <- commons_oral_questions_console('times')
#'
#' x <- commons_oral_questions_console('daysTabled')
#'
#' x <- commons_oral_questions_console('daysAnswered')
#'
#' x <- commons_oral_questions_console('askedBy')
#'
#' x <- commons_oral_questions_console('session')
#'
#' x <- commons_oral_questions_console('department')
#'
#' }

commons_oral_questions_console <- function(comsOralType = c("all", "times", "daysTabled", "daysAnswered", "askedBy",
    "session", "department")) {

    match.arg(comsOralType)

    if (comsOralType == "all") {

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?_pageSize=500"

        message("Connecting to API")

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

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions/tabled.json?startDate="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "daysAnswered") {

        start.date <- readline("Enter start date (yyyy-mm-dd): ")

        end.date <- readline("Enter end date (yyyy-mm-dd): ")

        start.date <- URLencode(start.date)

        end.date <- URLencode(end.date)

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions/answerDate.json?startDate="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, start.date, "&endDate=", end.date, "&_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsOralType == "askedBy") {

        mp.id <- readline("Enter Member ID: ")

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="

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

        baseurl_oral <- "http://lda.data.parliament.uk/commonsdivisions.json?session="

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

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions/answeringdepartment.json?q="

        message("Connecting to API")

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, answering.department, "&_pageSize=500"))

        oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, answering.department, "&_pageSize=500&_page=",
                i), flatten = TRUE)
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




#' commons_written_questions_console
#'
#' Requests data on House of Commons written questions
#' @param comsWritType The type of data you want, allows the arguments 'all', 'department', 'dates' and 'askedBy'
#' @param all Returns a data frame with all written questions
#' @param department Requests a department name, and returns all written questions by department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health
#' @param dates Requests two dates and imports a data frame with all available written questions from between the two dates.
#' @param askedBy Requests a member ID and returns a data frame with all written questions asked by that member


#' @keywords internal House of Commons Written Questions
#'
#' @noRd
 #' @examples \dontrun{
#' x <- commons_written_questions_console('all')
#'
#' x <- commons_written_questions_console('department')
#'
#' x <- commons_written_questions_console('dates')
#'
#' x <- commons_written_questions_console('askedBy')
#' }

commons_written_questions_console <- function(comsWritType = c("all", "department", "dates", "askedBy")) {

    match.arg(comsWritType)

    if (comsWritType == "all") {

        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

        message("Connecting to API")

        writ <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsWritType == "department") {

        answering.department <- readline("Enter the name of the answering department: ")
        answering.department <- URLencode(answering.department)

        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions/answeringdepartment.json?q="

        message("Connecting to API")

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&pageSize=500"))

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&_pageSize=500&_page=",
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsWritType == "dates") {

        start.date <- readline("Enter start date (yyyy-mm-dd): ")

        end.date <- readline("Enter end date (yyyy-mm-dd): ")

        start.date <- URLencode(start.date)

        end.date <- URLencode(end.date)

        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions/tabled.json?startDate="

        message("Connecting to API")

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500"))

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsWritType == "askedBy") {

        mp.id <- readline("Enter Member ID: ")

        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="

        message("Connecting to API")

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, mp.id, "&_pageSize=500"))

        if (writ$result$totalResults > writ$result$itemsPerPage) {
            writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        } else {
            writJpage <- 0
        }

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
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





#' constituencies_console
#'
#' Requests data on House of Commons constituencies
#' @param contType The type of data you want, allows the arguments 'all'
#' @param all Returns a data frame of all constituencies. Defaults to TRUE.


#' @keywords internal Constituencies

#' @noRd
 #' @examples \dontrun{
#' x <- constituencies_console('all')
#' }
#'


constituencies_console <- function(contType = c("all")) {

    match.arg(contType)

    if (contType == "all")
        {

            baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json?_pageSize=500"

            message("Connecting to API")

            conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json?_pageSize=500")

            contsJpage <- round(conts$result$totalResults/conts$result$itemsPerPage, digits = 0)

            pages <- list()

            for (i in 0:contsJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", contsJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }

        }  # else if(contType=='ID') {#Working Weirdly

    # cont.ID <- readline('Enter the constituency ID: ')

    # cont.ID <- as.numeric(cont.ID)

    # baseurl_conts <- 'http://lda.data.parliament.uk/constituencies/'

    # conts <- jsonlite::fromJSON(paste0('http://lda.data.parliament.uk/constituencies/',cont.ID,'.json?'))

    # df<-conts$result

    # }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}





#' early_day_motions_console
#'
#' Requests data on early day motions
#' @param edmType The type of data you want, allows the arguments 'all', 'allSponsors', 'primarySponsor', 'signatures' and 'ID'
#' @param all Returns a data frame of all early day motions
#' @param allSponsors Requests a member's ID, and returns a data frame of early day motions where the given member is a sponsor
#' @param primarySponsor Requests a member's ID, and returns a data frame of early day motions where the given member is the primary sponsor
#' @param signatures Returns a data frame of all early day motion signatures.
#' @param ID Requests an Early Day Motion ID, and returns a data frame with information on that Motion.




#' @keywords internal EDM

#' @noRd
 #' @examples \dontrun{
#' x <- early_day_motions_console('all')
#'
#' x <- early_day_motions_console('allSponsors')
#'
#' x <- early_day_motions_console('all')
#'
#' x <- early_day_motions_console('primarySponsor')
#'
#' x <- early_day_motions_console('signatures')
#'
#' x <- early_day_motions_console('ID')
#' }


early_day_motions_console <- function(edmType = c("all", "allSponsors", "primarySponsor", "signatures", "ID")) {

    match.arg(edmType)

    if (edmType == "all") {

        baseurl_edms <- "http://lda.data.parliament.uk/edms.json?_pageSize=500"

        message("Connecting to API")

        edms <- jsonlite::fromJSON(baseurl_edms)

        edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

    } else if (edmType == "allSponsors") {

        mp.id <- readline("Enter Member ID: ")

        baseurl_edms <- "http://lda.data.parliament.uk/edms.json?mnisId="

        message("Connecting to API")

        edms <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500"))

        if (edms$result$totalResults > edms$result$itemsPerPage) {

            edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

        } else {
            edmsJpage <- 0
        }

        pages <- list()

        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

    } else if (edmType == "primarySponsor") {

        mp.id <- readline("Enter Member ID: ")

        baseurl_edms <- "http://lda.data.parliament.uk/edmbysponsor.json?mnisId="

        message("Connecting to API")

        edms <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500"))

        if (edms$result$totalResults > edms$result$itemsPerPage) {

            edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

        } else {
            edmsJpage <- 0
        }

        pages <- list()

        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

    } else if (edmType == "signatures") {

        baseurl_edms <- "http://lda.data.parliament.uk/edmsignatures.json?_pageSize=500"

        message("Connecting to API")

        edms <- jsonlite::fromJSON(baseurl_edms)

        edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned


    } else if (edmType == "ID") {

        edmsID <- readline("Enter an Early Day Motion ID: ")

        baseurl_edms <- "http://lda.data.parliament.uk/edms/"

        message("Connecting to API")

        edms <- jsonlite::fromJSON(paste0(baseurl_edms, edmsID, ".json"))

        list <- edms$result$primaryTopic

        list

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}




#' election_results_console
#'
#' Requests data on general election results
#' @param resultType Accepts the arguments 'all' and 'ID'
#' @param all Returns general and by-election resuls for each consituency from the 2010 general election onwards.
#' @param ID Returns general and by-election resuls for each consituency from the 2010 general election onwards.


#' @keywords internal Election Results


#' @noRd
 #' @examples \dontrun{
#' x <- election_results_console('all')
#'
#' x <- election_results_console('ID')
#' }

election_results_console <- function(resultType = c("all", "ID")) {

    match.arg(resultType)

    if (resultType == "all") {

        baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json?_pageSize=500"

        message("Connecting to API")

        electR <- jsonlite::fromJSON("http://lda.data.parliament.uk/electionresults.json?_pageSize=500")

        electRJpage <- round(electR$result$totalResults/electR$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:electRJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_electR, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", electRJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resultType == "ID") {

        electID <- readline("Enter the election ID: ")

        baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json?electionId="

        message("Connecting to API")

        electR <- jsonlite::fromJSON(paste0(baseurl_electR, electID, "&_pageSize=500"))

        if (electR$result$totalResults > electR$result$itemsPerPage) {

            electRJpage <- round(electR$result$totalResults/electR$result$itemsPerPage, digits = 0)
        } else {
            electRJpage <- 0
        }
        pages <- list()

        for (i in 0:electRJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_electR, electID, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", electRJpage + 1)
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



#' elections_console
#'
#' Requests data on elections
#' @param electType Allows the arguments 'all' and 'ID'
#' @param all Returns a data frame with the date and type of all general and by-elections since 1945.
#' @param ID Requests an election ID, and returns a data frame with the date and type of that election.


#' @keywords internal Elections


#' @noRd
 #' @examples \dontrun{
#' x <- elections_console('all')
#'
#' x <- elections_console('ID')
#' }


elections_console <- function(electType = c("all", "ID")) {

    match.arg(electType)

    if (electType == "all") {

        baseurl_elect <- "http://lda.data.parliament.uk/elections.json?_pageSize=500"

        message("Connecting to API")

        elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json?_pageSize=500")

        pages <- list()

        for (i in 0:0) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (electType == "ID") {

        electID <- readline("Enter the election ID: ")

        baseurl_elect <- "http://lda.data.parliament.uk/elections/"

        message("Connecting to API")

        elect <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/elections/", electID, ".json"))

        mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/elections/", electID, ".json"), flatten = TRUE)

        df <- mydata$result$primaryTopic

        df <- as.data.frame(df)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}


#' epetition_console
#'
#' Requests data on Epetitions
#' @param petType The type of data you want, allows the arguments 'all', 'byConstituency', 'petitionID', 'response', and 'idConstituency'
#' @param all Imports all available epetitions
#' @param byConstituency Imports all available epetition signatures by constituency
#' @param petitionID Requests a petition ID, and then returns information on that petition
#' @param response Requests a petition ID, and then returns the governments response to that petition
#' @param idConstituency Requests a petition ID, and then returns the signatures per constituency for that petition


#' @keywords internal ePetitions


#' @noRd
 #' @examples \dontrun{
#' x <- epetition_console('all')
#'
#' x <- epetition_console('byConstituency')
#'
#' x <- epetition_console('petitionID')
#'
#' # x <- epetition_console('response')
#'
#' # x <- epetition_console('idConstituency')
#'
#'}

epetition_console <- function(petType = c("all", "byConstituency", "petitionID", "response", "idConstituency")) {

    match.arg(petType)

    if (petType == "all") {

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions.json?_pageSize=500"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(baseurl_petition)

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "byConstituency") {

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/signaturesbyconstituency.json?_pageSize=500"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(baseurl_petition)

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "petitionID") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, ".json?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "response") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, "/governmentresponse.json?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "idConstituency") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, "/signaturesbyconstituency?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
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



#' lords_amendments_console
#'
#' Requests data on House of Lords Amendments
#' @param all Returns a data frame with all available House of Lords Amendments Defaults to TRUE.


#' @keywords internal House of Lords Amendments


#' @noRd
 #' @examples \dontrun{
#' x <- lords_amendments_console()
#' }

lords_amendments_console <- function(all = TRUE) {

    baseurl_lordsAmmend <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

    message("Connecting to API")

    lordsAmmend <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500")

    lordsAmmendJpage <- round(lordsAmmend$result$totalResults/lordsAmmend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsAmmendJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAmmend, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", lordsAmmendJpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}



#' lords_attendance_console
#'
#' Requests data on House of Lords attendance. Please note that the attendance data frames are not as tidy as some of the others that are accessible through this API.
#' @param lordsAttendType Accepts arguments 'all' and 'date'.
#' @param all Returns a data frame with all available House of Lords attendance records.
#' @param date Requests a date and returns a data frame with all available House of Lords attendance records for that date.


#' @keywords internal House of Lords Attendance


#' @noRd
 #' @examples \dontrun{
#' x <- lords_attendance_console('all')
#'
#' x <- lords_attendance_console('date')
#' }
#'
lords_attendance_console <- function(lordsAttendType = c("all", "date")) {

    match.arg(lordsAttendType)

    if (lordsAttendType == "all") {

        baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500"

        message("Connecting to API")

        lordsAttend <- jsonlite::fromJSON(baseurl_lordsAttend)

        lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:lordsAttendJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", lordsAttendJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (lordsAttendType == "date") {

        attend.date <- readline("Enter date (yyyy-mm-dd): ")

        # attend.date <- URLencode(attend.date)

        baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances/date/"

        message("Connecting to API")

        lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, attend.date, ".json"))

        pages <- list()

        for (i in 0:0) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, attend.date, ".json"), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", 1)
            pages[[i + 1]] <- mydata$result$items
        }
        # } else if(lordsAttendType=='ID') {

        # Lords.ID <- readline('Enter lords ID number: ')

        # baseurl_lordsAttend <- 'http://lda.data.parliament.uk/lordsattendances.json?mnisId='

        # lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend,lords.ID,'&_pageSize=500'))

        # lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

        # pages <- list()

        # for (i in 0:lordsAttendJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, '&_page=', i),
        # flatten = TRUE) message('Retrieving page ', i+1, ' of ', lordsAttendJpage+1) pages[[i + 1]] <-
        # mydata$result$items }
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}




#' lords_divisions_console
#'
#' Requests data on House of Lords divisions
#' #' @param type The type of data you want, allows the arguments 'all', 'date', 'notContent', 'content' and 'session'
#' @param lordsDivType Accepts arguments 'all', 'date', 'notContent' and 'content'
#' @param all Returns a data frame with all available divisions in the House of Lords.
#' @param date Requests a date, and then returns a data frame with all divisions on that date.
#' @param notContent Requests a member ID, and then returns a data frame with all divisions in which they have voted not content.
#' @param content Requests a member ID, and then returns a data frame with all divisions in which they have voted content.
#' @param session Requests a session in yyyy/yy format (e.g. 2016/17) and returns a data frame with all House of Lords divisions in that session.



#' @keywords internal Lords Divisions
#' @noRd
 #' @examples \dontrun{
#' x <- lords_divisions_console('all')
#'
#' x <- lords_divisions_console('date')
#'
#' x <- lords_divisions_console('notContent')
#'
#' x <- lords_divisions_console('content')
#'
#' x <- lords_divisions_console('session')
#' }

lords_divisions_console <- function(lordsDivType = c("all", "date", "notContent", "content", "session")) {

    match.arg(lordsDivType)

    if (lordsDivType == "all") {

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions.json?_pageSize=500"

        message("Connecting to API")

        divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsdivisions.json?_pageSize=500")

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "date") {

        divis_date <- readline("Select division date: ")

        divis_date <- URLencode(divis_date)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/date/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/date/", divis_date,
            ".json?_pageSize=500"))

        if (divis$result$itemsPerPage > divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/date/", divis_date,
                ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "notContent") {

        mp.id <- readline("Enter Member ID: ")

        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "content") {

        mp.id <- readline("Enter Member ID: ")

        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId=",
            mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/aye.json?mnisId=",
                mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "session") {

        divis_session <- readline("Enter session (yyyy/yy): ")
        divis_session <- URLencode(divis_session)

        baseurl_divis <- "http://lda.data.parliament.uk/lordssdivisions.json?session="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_session, "&_pageSize=500"))

        if (divis$result$itemsPerPage < divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, divis_session, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "voteSummary") {
        ### HERE DOWN ALL NEEDS WORK

        vote.ID <- readline("Enter vote ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, vote.ID, ".json"))

        df <- divis$result$primaryTopic

        df$AbstainCount <- df$AbstainCount$`_value`
        df$AyesCount <- df$AyesCount$`_value`
        df$Didnotvotecount <- df$Didnotvotecount$`_value`
        df$Errorvotecount <- df$Errorvotecount$`_value`
        df$Noesvotecount <- df$Noesvotecount$`_value`
        df$Noneligiblecount <- df$Noneligiblecount$`_value`
        df$vote <- NULL
        df$Margin <- df$Margin$`_value`
        df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
        df$date <- df$date$`_value`

        df <- as.data.frame(df)

    } else if (lordsDivType == "voteFull") {

        vote.ID <- readline("Enter vote ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/id/"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, vote.ID, ".json"))

        df <- divis$result$primaryTopic$vote

        df

    } else if (lordsDivType == "uinSummary") {

        divis_uin <- readline("Enter division UIN (format: CD:yyyy-mm-dd:uin): ")
        divis_uin <- URLencode(divis_uin)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions.json?uin="

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_uin))

        df <- divis$result$items
        # Cleaning up the Data Frame
        df$AbstainCount <- df$AbstainCount$`_value`
        df$AyesCount <- df$AyesCount$`_value`
        df$Didnotvotecount <- df$Didnotvotecount$`_value`
        df$Errorvotecount <- df$Errorvotecount$`_value`
        df$Noesvotecount <- df$Noesvotecount$`_value`
        df$Noneligiblecount <- df$Noneligiblecount$`_value`
        df$vote <- NULL
        df$Margin <- df$Margin$`_value`
        df$Suspendedorexpelledvotescount <- df$Suspendedorexpelledvotescount$`_value`
        df$date <- df$date$`_value`

        df <- as.data.frame(df)

        df

    } else if (lordsDivType == "uinFull") {

        divis_uin <- readline("Enter division UIN: ")
        divis_uin <- URLencode(divis_uin)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions.json?uin="

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, divis_uin, "&_pageSize=500"))

        df <- divis$result$items$vote

        df

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}



#' lords_vote_record_console
#'
#' Accepts an ID number for a member of the House of Lords, and returns a data frame of their votes. Provides similar functionality to the lords_divisions() function, but accepts member IDs as function parameters rather than requesting them from the console.
#' @param lord.id The ID number of a member of the House of Lords. To look up the ID number of a member of the House of Lords use the members_search() function.
#' @param lordsRecord Accepts the arguments 'all', 'content' and 'notContent'. Defaults to 'all'.
#' @param all Returns a data frame with all recorded votes for a given member of the house of lords.
#' @param content Returns a data frame with all divisions where a given lord voted aye.
#' @param notContent Returns a data frame with all divisions where a given lord voted no.



#' @keywords internal divisions
#' @noRd
 #' @examples \dontrun{
#' x <- lords_vote_record_console(172, 'all')
#' }
#'

lords_vote_record_console <- function(lord.id, lordsRecord = c("all", "content", "notContent")) {

    match.arg(lordsRecord)

    if (lordsRecord == "all") {

        message("Retrieving content votes:")
        df_content <- lords_vote_record_console(lord.id, "content")

        df_content$vote <- "content"
        message("Retrieving not content votes:")
        df_notContent <- lords_vote_record_console(lord.id, "notContent")

        df_notContent$divisionNumber <- NULL

        df_notContent$vote <- "notContent"

        common <- intersect(colnames(df_content), colnames(df_notContent))

        df <- rbind(subset(df_content, select = common), subset(df_notContent, select = common))

        df

    } else if (lordsRecord == "content") {

        baseurl_aye <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        message("Connecting to API")

        url_aye <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id), flatten = TRUE)

        if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
            ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        } else {
            ayeJPage <- 0
        }

        pages <- list()

        for (i in 0:ayeJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsRecord == "notContent") {

        baseurl_no <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        message("Connecting to API")

        url_no <- jsonlite::fromJSON(paste0(baseurl_no, lord.id), flatten = TRUE)

        if (url_no$result$itemsPerPage < url_no$result$totalResults) {
            noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        } else {
            noJPage <- 0
        }

        pages <- list()

        for (i in 0:noJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_no, lord.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", noJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    }

    df$divisionResult <- as.factor(df$divisionResult)
    df$officialContentsCount <- as.factor(df$officialContentsCount)
    df$officialNotContentsCount <- as.factor(df$officialNotContentsCount)
    df$date._value <- as.Date(df$date._value)

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}



#' lords_written_questions_console
#'
#' Requests data on House of Lords written questions
#' @param lordsWritType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Returns a data frame all written questions from the house of lords
#' @param department Requests a department, and then returns a data frame with all written questions answered by that department
#' @param dates Requests two dates, and returns a data frame with all available written questions from between the two given dates


#' @keywords internal House of Lords Written Questions

#' @noRd
 #' @examples \dontrun{
#' x <- lords_written_questions_console('all')
#'
#' # x <- lords_written_questions_console('department')
#'
#' # x <- lords_written_questions_console('dates')
#' }

lords_written_questions_console <- function(lordsWritType = c("all", "department", "dates")) {

    match.arg(lordsWritType)

    if (lordsWritType == "all") {

        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions.json"

        writ <- jsonlite::fromJSON(baseurl_writ)

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (lordsWritType == "department") {

        answering.department <- readline("Enter the name of the answering department: ")

        answering.department <- URLencode(answering.department)

        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/answeringdepartment.json?q="

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&pageSize=500"))

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (lordsWritType == "dates") {

        start.date <- readline("Enter start date(yyyy-mm-dd): ")

        end.date <- readline("Enter end date (yyyy-mm-dd): ")

        start.date <- URLencode(start.date)

        end.date <- URLencode(end.date)

        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/tabled.json?startDate="

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500"))

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
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


#' members_console
#'
#' Requests data on All Members of Parliament including the Lords and the Commons
#' @param house The type of data you want, allows the arguments 'all', 'commons', 'lords', 'lordsInterests'
#' @param all Returns a data frame with information on all members of Parliament, including both the House of Lords and the House of Commons. The data frame includes both current and previous members, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#' @param commons Returns a data frame with information on all members of the House of Commons. The data frame includes both current and previous members of the House of Commons, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#' @param lords Returns a data frame with all available members of the House of Lords.
#' @param lordsInterests  Requests a member ID, and returns a data frame of the registered interests of that member of the House of Lords.


#' @keywords internal All Members of Parliament

#' @noRd
 #' @examples \dontrun{
#' x <- members_console'all')
#'
#' x <- members_console'commons')
#'
#' x <- members_console'lords')
#'
#' b <- members_console'lordsInterests')
#'}

members_console <- function(house = c("all", "commons", "lords", "lordsInterests")) {

    match.arg(house)

    if (house == "all") {

        baseurl_allMems <- "http://lda.data.parliament.uk/members.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (house == "commons") {

        baseurl_allMems <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (house == "lords") {

        baseurl_allMems <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (house == "lordsInterests") {

        MP.ID <- readline("Enter the members ID number: ")

        MP.ID <- URLencode(MP.ID)

        baseurl_allMems <- "http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member="

        allMems <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member=",
            MP.ID))

        if (allMems$result$totalResults > allMems$result$itemsPerPage) {

            allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

            match.arg(house)

            if (house == "all") {

                baseurl_allMems <- "http://lda.data.parliament.uk/members.json?_pageSize=500"

                allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json?_pageSize=500")

                allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

                pages <- list()

                for (i in 0:allMemsJpage) {
                  mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
                  message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
                  pages[[i + 1]] <- mydata$result$items
                }

                df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

            } else if (house == "commons") {

                baseurl_allMems <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

                allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500")

                allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

                pages <- list()

                for (i in 0:allMemsJpage) {
                  mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
                  message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
                  pages[[i + 1]] <- mydata$result$items
                }

                df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

            } else if (house == "lords") {

                baseurl_allMems <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

                allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500")

                allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

                pages <- list()

                for (i in 0:allMemsJpage) {
                  mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
                  message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
                  pages[[i + 1]] <- mydata$result$items
                }

                df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

            } else if (house == "lordsInterests") {

                MP.ID <- readline("Enter the members ID number: ")

                MP.ID <- URLencode(MP.ID)

                baseurl_allMems <- "http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member="

                allMems <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member=",
                  MP.ID))

                if (allMems$result$totalResults > allMems$result$itemsPerPage) {

                  allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
                } else {
                  allMemsJpage <- 0
                }

                pages <- list()

                for (i in 0:allMemsJpage) {
                  mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, MP.ID, "&_page=", i), flatten = TRUE)
                  message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
                  pages[[i + 1]] <- mydata$result$items
                }

                df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
            }

            if (nrow(df) == 0) {
                message("The request did not return any data. Please check your search parameters.")
            } else {
                df
            }

            pages <- list()

            for (i in 0:allMemsJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, MP.ID, "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
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
}


#' papers_laid_console
#'
#' Requests data on Papers Laid
#' @param paperType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Returns a data frame with all available papers laid.


#' @keywords internal Papers Laid

#' @noRd
 #' @examples \dontrun{
#' x <- papers_laid_console('all') }
#'

papers_laid_console <- function(paperType = c("all")) {

    match.arg(paperType)

    if (paperType == "all") {

        baseurl_papers <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"

        message("Connecting to API")

        papers <- jsonlite::fromJSON(baseurl_papers)

        papersJpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:papersJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_papers, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", papersJpage + 1)
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






#' publication_logs_console
#'
#' Requests data on Publication Logs
#' @param all Returns a data frame with all available Publication Logs. Defaults to TRUE.



#' @keywords internal Publication Logs
#' @noRd
 #' @examples \dontrun{
#' # x <- publication_logs_console()
#' }

publication_logs_console <- function(all = TRUE) {

    baseurl_logs <- "http://lda.data.parliament.uk/publicationlogs.json?_pageSize=500"

    message("Connecting to API")

    logs <- jsonlite::fromJSON(baseurl_logs)

    logsJpage <- round(logs$result$totalResults/logs$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:logsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_logs, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", logsJpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}



#' research_briefings_console
#'
#' Requests data on  Parliamentary Research Briefings
#' @param resType The type of data you want, allows the arguments 'all', 'topics', 'types',
#' # 'byTopic','subTopic' and 'topicSubTopic'
#' @param all Imports a data frame with all available Parliamentary Research Briefings.
#' @param topics Imports a data frame with all Parliamentary Research Briefings topics.
#' @param types Imports a data frame with all Parliamentary Research Briefings types.
#' @param byTopic Requests a topic, and imports a data frame with all available Parliamentary Research Briefings on that topic
#' @param subTopic Requests a topic, and imports a data frame with all of the possible sub-topics for that topic.
#' @param topicSubTopic Requests a topic and a subtopic, and imports a data frame with all available Parliamentary Research Briefings on that subtopic



#' @keywords internal  Parliamentary Research Briefings
#' @noRd
 #' @examples \dontrun{
#' x <- research_briefings_console('all')
#'
#' x <- research_briefings_console('topics')
#'
#' x <- research_briefings_console('types')
#'
#' x <- research_briefings_console('byTopic')
#'
#' x <- research_briefings_console('subTopic')
#'
#' x <- research_briefings_console('topicSubTopic')
#' }

research_briefings_console <- function(resType = c("all", "topics", "types", "byTopic", "subTopic", "topicSubTopic")) {

    match.arg(resType)

    if (resType == "all") {

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json?_pageSize=500"

        message("Connecting to API")

        research <- jsonlite::fromJSON(baseurl_research)

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "topics") {

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500"

        message("Connecting to API")

        research <- jsonlite::fromJSON(baseurl_research)

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "types") {

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingtypes.json?_pageSize=500"

        message("Connecting to API")

        research <- jsonlite::fromJSON(baseurl_research)

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "byTopic") {

        message("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic:  ")
        topic <- URLencode(topic)

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)

        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"

        message("Connecting to API")

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "subTopic") {

        topic <- readline("Enter topic:  ")
        topic <- URLencode(topic)


        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)
        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingsubtopics/"

        message("Connecting to API")

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500", "&_page=", i),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "topicSubTopic") {

        print("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic: ")
        topic <- URLencode(topic)

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)

        }
        message("Sub-topics are case sensititve. To return list of sub-topics, enter yes.")
        subTopic <- readline("Enter sub-topic:  ")
        subTopic <- URLencode(subTopic)

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (subTopic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/",
                topic, ".json?_pageSize=500"))

            print(research$result$items$prefLabel$`_value`)

            subTopic <- readline("Enter sub-topic. For ease of use, copy and paste the sub-topic: ")
            subTopic <- URLencode(subTopic)

        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"

        message("Connecting to API")

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500"))

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
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




#' sessions_info_console
#'
#' Requests data on Parliamentary Sessions
#' @param sesType The type of data being requested, allows the arguments 'all' and 'days'
#' @param all Imports information on all available parliamentary sessions
#' @param days Imports information on the days in all available parliamentary sessions

#' @keywords internal Parliamentary Sessions
#' @noRd
 #' @examples \dontrun{
#'
#' x <- sessions_info_console('all')
#'
#' x <- sessions_info_console('days')
#' }

sessions_info_console <- function(sesType = c("all", "days")) {

    match.arg(sesType)

    if (sesType == "all") {

        baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json?_pageSize=500"

        message("Connecting to API")

        sessionsJpage <- 0

        pages <- list()

        for (i in 0:sessionsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", sessionsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (sesType == "days") {

        baseurl_sessions <- "http://lda.data.parliament.uk/sessions/days.json?_pageSize=500"

        url_sessions <- jsonlite::fromJSON(paste0(baseurl_sessions), flatten = TRUE)

        message("Connecting to API")

        sessionsJpage <- round(url_sessions$result$totalResults/url_sessions$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:sessionsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", sessionsJpage + 1)
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





#' tv_programmes_console
#'
#' Requests data on TV broadcasts
#' @param AVtype The type of data you want, allows the arguments 'TV' and 'clips'
#' @param TV Returns a data frame with details on all TV broadcasts produced by the Westminster Parliament.
#' @param clips Returns a data frame with details on all video clips produced by the Westminster Parliament.
#' @keywords internal TV

#' @noRd
 #' @examples \dontrun{
#'
#' x <- tv_programmes_console('TV')
#'
#'
#' x <- tv_programmes_console('clips')
#' }

tv_programmes_console <- function(AVtype = c("TV", "clips")) {

    match.arg(AVtype)

    if (AVtype == "TV") {

        baseurl_tv <- "http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500"

        tv <- jsonlite::fromJSON(baseurl_tv)

        message("Connecting to API")

        tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:tvJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", tvJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (AVtype == "clips") {

        baseurl_tv <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

        tv <- jsonlite::fromJSON(baseurl_tv)

        message("Connecting to API")

        tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:tvJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", tvJpage + 1)
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
