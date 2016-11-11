
#' House of Lords Written Questions
#'
#' Imports data on House of Lords written questions
#' @param lordsWritType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Imports all available written questions
#' @param department Imports all available written questions answered by a given department
#' @param dates Imports all available written questions from between two given dates
#' @keywords House of Lords Written Questions
#' @export
#' @examples \donttest{
#' # NOT RUN
#' # Returns all written questions from the house of lords
#' # x <- lords_written_questions('all')
#' # head(x)
#'
#' # NOT RUN
#' # Requests a department, and then returns all written questions answered by that department
#' # x <- lords_written_questions('department')
#' # Enter the name of the answering department: Health
#' # head(x) # Returns all answered questions where the answering department
#'           # includes 'Health' in its name
#' # NOT RUN
#' # Requests two dates, and then returns all written questions within those two dates
#' # x <- lords_written_questions('dates')
#' # x <- lords_written_questions('dates')
#' ####RETURNS:
#' # Enter start date(yyyy-mm-dd): 2016-10-10
#' # Enter start date(yyyy-mm-dd): 2016-10-15
#' # head(x) }

lords_written_questions <- function(lordsWritType = c("all", "department", "dates")) {

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

        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/tabled.json?startDate="

        writ <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500"))

        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=", i),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}
