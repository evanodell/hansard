

### 10 ELECTION RESULTS

#' Elections Results
#'
#' This imports data on general election results
#' @param all Imports all available election results Defaults to TRUE.
#' @keywords Election Results
#' @export
#' @examples
#' election_results(all=TRUE)
#' #Returns general and by-election resuls for each consituency from the 2010 general election onwards.



election_results <- function(all = TRUE) {

  baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json?_pageSize=500"

  electR <- jsonlite::fromJSON("http://lda.data.parliament.uk/electionresults.json?_pageSize=500")

  electRJpage <- round(electR$result$totalResults/electR$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:electRJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_electR, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", electRJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }

  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
