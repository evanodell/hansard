
### 13 LORDS AMMENDMENTS

#' House of Lords Ammendments
#'
#' This imports data on House of Lords Ammendments
#' @param all Imports all available House of Lords Ammendments Defaults to TRUE.
#' @keywords House of Lords Ammendments
#' @export
#' @examples
#' lords_ammendments()

lords_ammendments <- function(all = TRUE) {

  baseurl_lordsAmmend <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

  lordsAmmend <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500")

  lordsAmmendJpage <- round(lordsAmmend$result$totalResults/lordsAmmend$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:lordsAmmendJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAmmend, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", lordsAmmendJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }
}
