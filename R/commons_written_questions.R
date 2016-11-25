
#' commons_written_questions
#'
#' Imports data on House of Commons written questions
#' @param comsWritType The type of data you want, allows the arguments 'all', 'department', 'dates' and 'askedBy'
#' @param all Returns a data frame with all written questions
#' @param department Requests a department name, and returns all written questions by department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health
#' @param dates Requests two dates and imports a data frame with all available written questions from between the two dates.
#' @param askedBy Requests a member ID and returns a data frame with all written questions asked by that member
#' @keywords House of Commons Written Questions
#' @export
#' @examples \dontrun{
#' x <- commons_written_questions('all')
#'
#' x <- commons_written_questions('department')
#'
#' x <- commons_written_questions('dates')
#'
#' x <- commons_written_questions('askedBy')
#' }

commons_written_questions <- function(comsWritType = c("all", "department", "dates", "askedBy")) {

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
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&_pageSize=500&_page=", i), flatten = TRUE)
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
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=",
                i), flatten = TRUE)
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
