

#' House of Lords Divisions
#'
#' This imports data on House of Lords divisions
#' #' @param type The type of data you want, allows the arguments 'all', 'date', 'no', 'aye'
#' @param lordsDivType Accepts arguments 'all', 'date', 'notContent' and 'content'
#' @param all Imports all available divisions.
#' @param date Imports all available divisions on a date.
#' @param notContent Imports all divisions where a given lord voted not content.
#' @param content Imports all divisions where a given lord voted content.
#' @keywords Lords Divisions
#' @export
#' @examples \donttest{
#' ### NOT RUN:
#' # x <- lords_divisions('all')
#' # Returns all divisions in the House of Lords
#'
#' # x <- lords_divisions('date')
#' # Requests a date, and then returns all divisions on that date
#'
#' # x <- lords_divisions('notContent')
#' # Requests a member ID, and then returns all divisions in which they have voted not content
#'
#' # x <- lords_divisions('content')
#' # Requests a member ID, and then returns all divisions in which they have voted content
#'}

lords_divisions <- function(lordsDivType = c("all", "date", "notContent", "content")) {

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

    } else if (lordsDivType == "date") {

        divis_date <- readline("Select division date: ")

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

    } else if (lordsDivType == "notContent") {
        # WORKING

        mp.id <- readline("Enter Member ID: ")

        baseurl_divis <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId=", mp.id, "&_pageSize=500"))

        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (lordsDivType == "content")
        {
            # WORKING

            mp.id <- readline("Enter Member ID: ")

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
        }  #else if (type=='session') {

    # baseurl_divis <- 'http://lda.data.parliament.uk/lordsdivisions.json?_pageSize=500'

    # divis <- jsonlite::fromJSON('http://lda.data.parliament.uk/lordsdivisions.json?_pageSize=500')

    # divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

    # pages <- list()

    # for (i in 0:divisJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_divis, '&_page=', i), flatten = TRUE)
    # message('Retrieving page ', i+1, ' of ', divisJpage+1) pages[[i + 1]] <- mydata$result$items } }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
}
