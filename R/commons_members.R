


### 16b COMMONS MEMBERS

#' House of Commons Members
#'
#' This imports data on House of Commons members
#' @param all Imports all available House of Commons members Defaults to TRUE.
#' @keywords House of Commons Members
#' @export
#' @examples
#' commons_members()

commons_members <- function(all = TRUE) {

  baseurl_comMems <- "http://lda.data.parliament.uk/commonsmembers.json"

  comMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json")

  comMemsJpage <- round(comMems$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:comMemsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_comMems, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", comMemsJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
