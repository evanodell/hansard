### 12 LORDS ATTENDANCE


#' House of Lords Attendance
#'
#' This imports data on House of Lords attendance
#' @param all Imports all available House of Lords attendance records Defaults to TRUE.
#' @keywords House of Lords Attendance
#' @export
#' @examples
#' lords_attendance()

lords_attendance <- function(all = TRUE) {

    baseurl_lordsAttend <- " http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500"

    lordsAttend <- jsonlite::fromJSON(" http://lda.data.parliament.uk/lordsattendances.json?_pageSize=500")

    lordsAttendJpage <- round(lordsAttend$result$totalResults/lordsAttend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsAttendJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAttend, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", lordsAttendJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }
}

