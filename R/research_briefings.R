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

  baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json"

  research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefings.json")

  researchJpage <- round(research$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:researchJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_research, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", researchJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}



