


### 16c LORDS MEMBERS

#' House of Lords Members
#'
#' This imports data on House of Lords members
#' @param all Imports all available House of Lords members Defaults to TRUE.
#' @keywords House of Lords Members
#' @export
#' @examples
#' lords_members()

lords_members <- function(all = TRUE) {

  baseurl_lordMems <- "http://lda.data.parliament.uk/lordsmembers.json"

  lordMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json")

  lordMemsJpage <- round(lordMems$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:lordMemsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_lordsMems, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i)
    pages[[i + 1]] <- mydata$result$items
  }
}
