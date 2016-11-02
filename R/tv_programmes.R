
### 4 TV PROGRAMMES

#' TV Programmes
#'
#' This imports data on TV broadcasts
#' @param all Imports data on all available TV Broadcasts Defaults to TRUE.
#' @keywords TV
#' @export
#' @examples
#' tv_programmes()


tv_programmes <- function(all = TRUE) {

  baseurl_av <- "http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500"

  av <- jsonlite::fromJSON("http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500")

  avJpage <- round(av$result$totalResults/av$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:avJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_av, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", avJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }
}
