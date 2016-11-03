### 20 RESEARCH BRIEFINGS


#' Parliamentary Research Briefings
#'
#' This imports data on  Parliamentary Research Briefings
#' @param all Imports all available Parliamentary Research Briefings Defaults to TRUE.
#' @keywords  Parliamentary Research Briefings
#' @export
#' @examples
#' research_briefings()

research_briefings <- function(all = TRUE) {

  baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json?_pageSize=500"

  research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefings.json?_pageSize=500")

  researchJpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:researchJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", researchJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}



