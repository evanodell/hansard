
### 16d COMMONS REGISTERED INTERESTS

#' House of Commons Registered Interests
#'
#' This imports data on House of Commons Registered Interests
#' @param all Imports all available House of Commons Registered Interests Defaults to TRUE.
#' @keywords House of Commons Registered Interests
#' @export
#' @examples
#' commons_interest()

commons_interest <- function(all = TRUE) {

  baseurl_comInterest <- "http://lda.data.parliament.uk/commonsregisteredinterests.json"

  comInterest <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsregisteredinterests.json")

  comInterestJpage <- round(comInterest$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:comInterestJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_comInterest, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", comInterestJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}

