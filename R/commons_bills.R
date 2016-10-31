### 5 BILLS

#' House of Commons Bills
#'
#' This imports data on House of Commons bills
#' @param all Imports all available bills Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' commons_bills()


commons_bills <- function(type) {

  if(!type %in% c("all", "ammendments", "sponsor",
                  "stage", "publications", "stage types"))
    ("Warning: Please select an API query")




  baseurl_bills <- "http://lda.data.parliament.uk/bills.json?_pageSize=500"

  bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/bills.json?_pageSize=500")

  billsJpage <- round(bills$result$itemsPerPage/bills$result$totalResults, digits = 0)

  pages <- list()

  for (i in 0:billsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", billsJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }

  ##Sponsor

  ##Ammendments

  ##Stage

  ##Publications

  ##Stage Types



}
