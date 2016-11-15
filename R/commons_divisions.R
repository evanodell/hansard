

#' House of Commons Divisions
#'
#' Imports data on House of Commons divisions
#' @param comsDivType The type of data you want, allows the arguments 'all', 'date', 'no', 'aye', 'voteSummary', 'voteFull', 'session', 'uinSummary' and 'uinFull'
#' @param all Returns a data frame with all available divisions.
#' @param date Returns a data frame with all available divisions on a date.
#' @param no Returns a data frame with all divisions where a given MP voted no.
#' @param aye Returns a data frame with all divisions where a given MP voted aye.
#' @param voteSummary Requests a division ID, and returns a summary of results of that division in a data frame.
#' @param voteFull Requests a division ID, and returns a data frame with details on how each individual member voted.
#' @param session Requests a session in yyyy/yy format (e.g. 2016/17) and returns a data frame with all divisions in that session.
#' @param uinSummary Requests a division UIN and returns a data frame with a summary of results of that division.
#' @param uinFull Requests a division UIN and returns a data frame with the full results of that division.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- commons_divisions('all')
#'
#' x <- commons_divisions('date')
#'
#' x <- commons_divisions('no')
#'
#' x <- commons_divisions('aye')
#'
#' x <- commons_divisions('voteSummary')
#'
#' x <- commons_divisions('voteFull')
#'
#' x <- commons_divisions('session')
#'
#' x <- commons_divisions('uinSummary')
#'
#' x <- commons_divisions('uinFull')
#' }

commons_divisions <- function(comsDivType = c("all", "date", "no", "aye", "voteSummary", "voteFull", "session", "uinSummary", "uinFull")) {

    match.arg(comsDivType)

    if (comsDivType == "all") {

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500"

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

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/", divis_date, ".json?_pageSize=500"))

        if (divis$result$itemsPerPage < divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, divis_date, ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (comsDivType == "no") {

        mp.id <- readline("Enter Member ID: ")
        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId=", mp.id, "&_pageSize=500"))

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

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=", mp.id, "&_pageSize=500"))

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

        divis <- jsonlite::fromJSON(paste0(baseurl_divis, vote.ID, ".json"))

        df <- divis$result$primaryTopic$vote

        df

    } else if (comsDivType == "session") {

        divis_session <- readline("Enter session (yyyy/yy): ")
        divis_session <- URLencode(divis_session)

        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?session="

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


#' Divisions where an MP voted aye
#'
#' Accepts an ID number for a member of the house of commons, and returns a data frame of all divisions where they voted aye.
#' @param mp.id The ID number of a member of the House of Commons
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- mp_aye(172)
#' }


mp_aye <- function(mp.id){

  if(is.numeric(mp.id)==FALSE){
    stop("The request did not return any data. Please check your search parameters.")
  }

  baseurl_aye <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

  url_aye <- jsonlite::fromJSON(paste0(baseurl_aye, mp.id, "&_pageSize=500"), flatten = TRUE)

  GET(url_aye)

  if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
    ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
  } else {
    ayeJPage <- 0
  }

  pages <- list()

  for (i in 0:ayeJPage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_aye, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

  df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {
    df
  }

}



#' Divisions where an MP voted no
#'
#' Accepts an ID number for a member of the house of commons, and returns a data frame of all divisions where they voted aye.
#' @param mp.id The ID number of a member of the House of Commons
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- mp_no(336)
#' }


mp_no <- function(mp.id){

  if(is.numeric(mp.id)==FALSE){
    stop("The request did not return any data. Please check your search parameters.")
  }

  baseurl_no <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

  url_no <- jsonlite::fromJSON(paste0(baseurl_no, mp.id, "&_pageSize=500"), flatten = TRUE)

  if (url_no$result$itemsPerPage < url_no$result$totalResults) {
    noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
  } else {
    noJPage <- 0
  }

  pages <- list()

  for (i in 0:noJPage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_no, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", noJPage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

  df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {
    df
  }

}

