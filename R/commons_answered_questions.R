

#' commons_answered_questions
#'
#' Imports data on House of Commons answered questions. If all parameters are left empty, imports all available answered questions in a data frame.
#' @param start_date The earliest date to include in the data frame. Defaults to "1900-01-01".
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @param department Returns a data frame with all answered questions in the House of Commons from the given department. Defaults to NULL.
#' @param answered_by Returns a data frame with all answered questions in the House of Commons by the given MP. Defaults to NULL.
#' @keywords bills
#' @export
#' @examples \dontrun{
#'
#' x <- commons_answered_questions(department = 'health', answered_by = '4019')
#'
#' }
##Still need to check

commons_answered_questions <- function(department = NULL, answered_by = NULL, start_date="1900-01-01", end_date=Sys.Date()) {


  dates <-paste0("&_properties=dateOfAnswer&max-dateOfAnswer=",end_date, "&min-dateOfAnswer=",start_date)


    if (is.null(department) == TRUE & is.null(answered_by) == TRUE) {

      if (is.null(date) == FALSE) {
        date <- paste0("&", date)
      }

      baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions.json?_pageSize=500"

      message("Connecting to API")

      answered <- jsonlite::fromJSON(paste0(baseurl, dates), flatten = TRUE)

      jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, dates, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {
            df
        }

    } else if (is.null(department) == FALSE) {

        ### Search by member id
        if (is.null(answered_by) == FALSE) {

            name_lookup <- paste0("http://lda.data.parliament.uk/members/", answered_by, ".json?")

            member <- jsonlite::fromJSON(name_lookup, flatten = TRUE)

            member <- member$result$primaryTopic$fullName$`_value`

            answered_by <- paste0("&answeringMemberPrinted=", member)

            answered_by <- utils::URLencode(answered_by)

        }

        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions/answeringdepartment.json?q="

        message("Connecting to API")

        answered <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, dates, "&_pageSize=500"), flatten = TRUE)

        if (answered$result$totalResults > answered$result$itemsPerPage) {

            jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

            pages <- list()

            for (i in 0:jpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500", "&_page=",
                  i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", jpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }

            df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

            if (nrow(df) == 0) {
                message("The request did not return any data. Please check your search parameters.")
            } else {
                df
            }

        } else {

            message("Retrieving page 1 of 1")

            mydata <- jsonlite::fromJSON(paste0(baseurl, department, answered_by, date, "&_pageSize=500"), flatten = TRUE)

            df <- mydata$result$items

        }

    } else if (is.null(answered_by) == FALSE) {

        baseurl <- "http://lda.data.parliament.uk/commonsansweredquestions/answeredby/"

        message("Connecting to API")

        answered <- jsonlite::fromJSON(paste0(baseurl, answered_by, ".json?", date, "&_pageSize=500"))

        jpage <- round(answered$result$totalResults/answered$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, answered_by, ".json?", date, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {
            df
        }

    }

}

