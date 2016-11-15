

#' House of Lords Divisions
#'
#' Imports data on House of Lords divisions
#' #' @param type The type of data you want, allows the arguments "all", "date", "notContent", "content" and "session"
#' @param lordsDivType Accepts arguments "all", "date", "notContent" and "content"
#' @param all Returns a data frame with all available divisions in the House of Lords.
#' @param date Requests a date, and then returns a data frame with all divisions on that date.
#' @param notContent Requests a member ID, and then returns a data frame with all divisions in which they have voted not content.
#' @param content Requests a member ID, and then returns a data frame with all divisions in which they have voted content.
#' @param session Requests a session in yyyy/yy format (e.g. 2016/17) and returns a data frame with all House of Lords divisions in that session.
#' @keywords Lords Divisions
#' @export
#' @examples \dontrun{
#' x <- lords_divisions("all")
#'
#' x <- lords_divisions("date")
#'
#' x <- lords_divisions("notContent")
#'
#' x <- lords_divisions("content")
#'
#' x <- lords_divisions("session")
#' }

lords_divisions <- function(lordsDivType = c("all", "date", "notContent", "content", "session")) {

    match.arg(lordsDivType)

    if (lordsDivType == "all") {

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions.json?_pageSize=500"

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

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/date/", divis_date, ".json?_pageSize=500"))

        if (divis$result$itemsPerPage > divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/date/", divis_date, ".json?_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "notContent") {

        mp.id <- readline("Enter Member ID: ")

        mp.id <- URLencode(mp.id)

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

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

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId=", mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/aye.json?mnisId=", mp.id, "&_pageSize=500",
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    } else if (lordsDivType == "session") {

        divis_session <- readline("Enter session (yyyy/yy): ")
        divis_session <- URLencode(divis_session)

        baseurl_divis <- "http://lda.data.parliament.uk/lordssdivisions.json?session="

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

    } else if (lordsDivType == "voteSummary") { ###HERE DOWN ALL NEEDS WORK

        vote.ID <- readline("Enter vote ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/id/"

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


