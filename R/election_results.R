

### 10 ELECTION RESULTS

#' Elections Results
#'
#' This imports data on general election results
#' @param all Imports all available election results Defaults to TRUE.
#' @keywords Election Results
#' @export
#' @examples
#' election_results()



election_results <- function(all = TRUE) {

  baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json"

  electR <- jsonlite::fromJSON("http://lda.data.parliament.uk/electionresults.json")

  electRJpage <- round(electR$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:electRJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_electR, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i)
    pages[[i + 1]] <- mydata$result$items
  }
}
