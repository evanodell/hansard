### 5 BILLS

#' House of Commons Bills
#'
#' This imports data on House of Commons bills
#' @param all Imports all available bills Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_bills()


commons_bills <- function(all = TRUE) {

  baseurl_bills <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  billsJpage <- round(bills$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:billsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", billsJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
