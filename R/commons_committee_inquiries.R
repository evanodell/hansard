#' commons_committee_inquiries
#'
#' Returns commons committee inquiries
#' @param id Returns the committee report of a given ID. If no ID number is entered, returns all available committee reports.
#' @param recommendations If TRUE, returns all reccomendations for the given report ID. Defaults to FALSE.
#' @keywords Committee Reports
#' @export
#' @examples \dontrun{
#' x <- commons_committee_inquiries(id = NULL, recommendations = FALSE)
#' }

### API NOT OPEN!!!
### INCOMPLETE!

commons_committee_inquiries <- function(id=NULL, inquiries=FALSE, current=FALSE, nameid=NULL) {

    if  (is.null(id)==TRUE) {

      baseurl_commons_committee_inquiries <- "http://lda.data.parliament.uk/committeereports.json?_pageSize=500"

      message("Connecting to API")

      commons_committee_inquiries_page <- jsonlite::fromJSON(baseurl_committee_reports)

      commons_committee_inquiries_jpage <- round(commons_committee_inquiries_page$result$totalResults/commons_committee_inquiries_page$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:lordsAmmendJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_commons_committee_inquiries, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", commons_committee_inquiries_jpage + 1)
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
