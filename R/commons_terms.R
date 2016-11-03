### 22 TERMS

#' Parliamentary Thesaurus
#'
#' This imports the parliamentary thesaurus
#' @param all Imports information from the parliamentary thesaurus Defaults to TRUE.
#' @keywords parliamentary thesaurus
#' @export
#' @examples
#' commons_terms()

commons_terms <- function(all = TRUE) {

  baseurl_terms <- "http://lda.data.parliament.uk/terms.json?_pageSize=500"

  cTerms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json?_pageSize=500")

  cTermsJpage <- round(cTerms$result$totalResults/cTerms$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:cTermsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_terms, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", cTermsJpage+1)
    pages[[i + 1]] <- mydata$result$items
  }


  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
