

#' commons_divisions
#'
#' Imports data on House of Commons divisions.
#' @param division_id The id of a particular vote. If empty, returns a data frame with information on all commons divisions. Defaults to NULL.
#' @param summary If TRUE, returns a small data frame summarising a division outcome. Has no effect if `division_id` is empty. Defaults to FALSE.
#' @param start_date The earliest date to include in the data frame, if calling all divisions. Defaults to "1900-01-01".
#' @param end_date The latest date to include in the data frame, if calling all divisions. Defaults to current system date.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_divisions(division_id = 694163, summary = FALSE)
#'
#' }

commons_divisions <- function(division_id = NULL, summary = FALSE, start_date="1900-01-01", end_date=Sys.Date()) {

  dates <-paste0("&_properties=date&max-date=",end_date, "&min-date=",start_date)

    if (is.null(division_id) == TRUE) {

      if (is.null(date) == FALSE) {
        date <- as.character(date)
        date <- paste0("&date=", date)
      }

      baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

      message("Connecting to API")

      divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates))

      jpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", dates, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
      }

      df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

      if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
      } else {

        df

      }

    } else if (is.null(division_id) == FALSE) {

      division_id <- as.character(division_id)

      baseurl <- "http://lda.data.parliament.uk/commonsdivisions/id/"

      message("Connecting to API")

      divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json", dates), flatten = TRUE)

      if (summary == TRUE) {

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

        } else {
            df <- as.data.frame(divis$result$primaryTopic$vote)

            df
        }

    }

}


#' commons_division_date
#'
#' Returns a data frames with
#' @param date Returns all divisions on a given date. Defaults to NULL.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- commons_division_date('2016-10-12')
#' }
#'

commons_division_date <- function(date = NULL) {

    if (is.null(date) == TRUE) {
        df <- commons_divisions()
    } else {
        date <- as.character(date)
        date <- paste0("&date=", date)

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

        message("Connecting to API")

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date))

        jpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?_pageSize=500", date, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {

            df

        }
    }

}

