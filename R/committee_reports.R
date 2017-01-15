#' committee_reports
#'
#' Returns committee reports.
#' @param id Returns the committee report of a given ID. If no ID number is entered, returns all available committee reports.
#' @param recommendations If TRUE, returns all reccomendations for the given report ID. Defaults to FALSE.
#' @keywords Committee Reports
#' @export
#' @examples \dontrun{
#' x <- committee_reports(id = NULL, recommendations = FALSE)
#' }

### API NOT OPEN!!!
committee_reports <- function(id = NULL, recommendations = FALSE) {

  if  (is.null(id)==TRUE) {

  baseurl_committee_reports <- "http://lda.data.parliament.uk/committeereports.json?_pageSize=500"

  message("Connecting to API")

  committee_reports_page <- jsonlite::fromJSON(baseurl_committee_reports)

  committee_reports_jpage <- round(committee_reports_page$result$totalResults/committee_reports_page$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:lordsAmmendJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_committee_reports, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", lordsAmmendJpage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

  df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {
    df
  }

  } else if  (is.null(id)==TRUE) {

    if (recommendations==TRUE) {
      recommendations <- "/recommendations"
    } else {
      recommendations <- NULL
    }

    baseurl_committee_reports <- "http://lda.data.parliament.uk/committeereports/"

    message("Connecting to API")

    committee_reports_page <- jsonlite::fromJSON(paste0(baseurl_committee_reports,id, recommendations, ".json"))

    committee_reports_jpage <- round(committee_reports_page$result$totalResults/committee_reports_page$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsAmmendJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_committee_reports, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i + 1, " of ", lordsAmmendJpage + 1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
    if (nrow(df) == 0) {
      message("The request did not return any data. Please check your search parameters.")
    } else{
      df
    }

  }

}
