

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

  baseurl_elect <- "http://lda.data.parliament.uk/elections.json?_pageSize=500"

  elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json?_pageSize=500")

  electJpage <- round(elect$result$totalResults/elect$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:electJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", electJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }
}
