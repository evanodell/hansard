
### 16e LORDS REGISTERED INTERESTS

#' House of Lords Registered Interests
#'
#' This imports data on House of Lords registered interests
#' @param all Imports all available House of Lords registered interests Defaults to TRUE.
#' @keywords House of Lords Registered Interests
#' @export
#' @examples
#' lords_interest()

lords_interest <- function(all = TRUE) {

  baseurl_lordInterest <- "http://lda.data.parliament.uk/lordsregisteredinterests.json"

  lordInterest <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsregisteredinterests.json")

  lordInterestJpage <- round(lordInterest$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:lordInterestJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_lordInterest, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i)
    pages[[i + 1]] <- mydata$result$items
  }
}
