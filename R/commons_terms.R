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

  baseurl_terms <- "http://lda.data.parliament.uk/terms.json"

  cTerms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json")

  cTermsJpage <- round(cTerms$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:cTermsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_terms, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i, " of ", cTermsJpage)
    pages[[i + 1]] <- mydata$result$items
  }
}
