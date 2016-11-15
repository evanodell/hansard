
#' lords_attendance
#'
#' Imports data on House of Lords attendance
#' @param lordsAttendType Accepts arguments 'all' and 'date'.
#' @param all Returns a data frame with all available House of Lords attendance records.
#' @param date Requests a date and returns a data frame with all available House of Lords attendance records for that date.
#' @keywords House of Lords Attendance
#' @export
#' @examples \dontrun{
#' x <- lords_attendance('all')
#'
#' x <- lords_attendance('date')
#' }
#'
lords_attendance <- function(lordsAttendType = c("all", "date")) {

    match.arg(lordsAttendType)

    if (lordsAttendType == "all") {

        baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500"

        lordsAttend <- jsonlite::fromJSON(" http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500")

        lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:lordsAttendJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", lordsAttendJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    } else if (lordsAttendType == "date") {

        attend.date <- readline("Enter date (yyyy-mm-dd): ")

        attend.date <- URLencode(attend.date)

        baseurl_lordsAttend <- "http://lda.data.parliament.uk/lordsattendances/date/"

        lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, attend.date, ".json?_pageSize=500"))

        pages <- list()

        for (i in 0:0) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", 1)
            pages[[i + 1]] <- mydata$result$items
        }
        # } else if(lordsAttendType=='ID') {

        # Lords.ID <- readline('Enter lords ID number: ')

        # baseurl_lordsAttend <- 'http://lda.data.parliament.uk/lordsattendances.json?mnisId='

        # lordsAttend <- jsonlite::fromJSON(paste0(baseurl_lordsAttend,lords.ID,'&_pageSize=500'))

        # lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

        # pages <- list()

        # for (i in 0:lordsAttendJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, '&_page=', i), flatten = TRUE)
        # message('Retrieving page ', i+1, ' of ', lordsAttendJpage+1) pages[[i + 1]] <- mydata$result$items }
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}

