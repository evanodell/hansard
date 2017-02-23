

#' commons_answered_questions
#'
#' Imports data on House of Commons answered questions

#' @param all Returns a data frame with all answered questions in the House of Commons. The default response, but is overriden by other parameters. Defaults to TRUE.
#' @param date Returns a data frame with all answered questions in the House of Commons on the given date. Defaults to NULL.
#' @param department Returns a data frame with all answered questions in the House of Commons from the given department. Defaults to NULL.
#' @param answered_by Returns a data frame with all answered questions in the House of Commons by the given MP. Defaults to NULL.
#' @keywords bills
#' @export
#' @examples \dontrun{
#' x <- commons_answered_questions('all')
#'
#' x <- commons_answered_questions('date')
#'
#' x <- commons_answered_questions('department')
#'
#' x <- commons_answered_questions('answered_by')
#' }

###Probably have to move the thing that creates the final data frame with each 'if' function
commons_answered_questions <- function(date = NULL, department = NULL, answered_by = NULL) {

  if (is.null(date)==FALSE) {
    date <- as.character(date)
    date <- paste0("&dateOfAnswer=",date)
  }

  if (is.null(department)==TRUE) {##Still to do
    department <- NULL
  } else {
    date <- paste0("&dateOfAnswer=",date)
  }

  if (is.null(answered_by)==TRUE) {##Still to do
    date <- NULL
  } else {
    date <- paste0("&dateOfAnswer=",date)
  }


    if (is.null(department)==TRUE & is.null(answered_by)==TRUE) {

        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

        message("Connecting to API")

        answered <- jsonlite::fromJSON(paste0(baseurl, date))

        jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(answered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }   else if (is.null(department)==FALSE) {

      ### Search by member id

      if (is.null(answered_by)==TRUE) {
        answered_by <- NULL
      } else {

      name_lookup <- paste0("http://lda.data.parliament.uk/members/", answered_by, ".json")

      member <- jsonlite::fromJSON(x, flatten = TRUE)

      member <- member$result$primaryTopic$fullName$`_value`

      answered_by <- paste0("&answeringMemberPrinted=", member)
      }

        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions.json?q="

        message("Connecting to API")

        answered <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500"))

        if (answered$result$totalResults > answered$result$itemsPerPage) {

            jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

        } else {

            jpage <- 0

          }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(answered, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (comsAnsType == "answered_by") {

        qAnsweredBy <- readline("Enter MP ID: ")
        qAnsweredBy <- URLencode(qAnsweredBy)

  else if (is.null(answered_by)==FALSE) {

    department <- URLencode(department)

    baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

    message("Connecting to API")

    answered <- jsonlite::fromJSON(paste0(baseurl, answered_by, date, "&_pageSize=500"))

    jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(answered, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i + 1, " of ", jpage + 1)
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
