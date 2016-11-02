
### 14 LORDS DIVISIONS

#' House of Lords Divisions
#'
#' This imports data on House of Lords divisions
#' @param all Imports all available House of Lords divisions Defaults to TRUE.
#' @keywords Lords Divisions
#' @export
#' @examples
#' lords_divisions()

lords_divisions <- function(all = TRUE) {

  baseurl_lordsDivs <- "http://lda.data.parliament.uk/lordsdivisions.json"

  lordsDivs <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsdivisions.json")

  lordsDivsJpage <- round(lordsDivs$result$totalResults/lordsDivs$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:lordsDivsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_lordsDivs, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", lordsDivsJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}


