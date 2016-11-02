

### 11 ELECTIONS

#' Elections
#'
#' This imports data on elections
#' @param all Imports all available elections Defaults to TRUE.
#' @keywords Elections
#' @export
#' @examples
#' elections()


elections <- function(all = TRUE) {

  baseurl_elect <- "http://lda.data.parliament.uk/elections.json"

  elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json")

  electJpage <- round(elect$result$totalResults/elect$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:electJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", electJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
