
#' Early Day Motions
#'
#' This imports data on early day motions
#' @param all Imports all available EDMs. Defaults to TRUE.
#' @keywords EDM
#' @export
#' @examples
#' early_day_motions()

### 9 EARLY DAY MOTIONS

early_day_motions <- function(all = TRUE) {

  baseurl_edms <- "http://lda.data.parliament.uk/edms.json"

  edms <- jsonlite::fromJSON("http://lda.data.parliament.uk/edms.json")

  edmsJpage <- round(edms$result$totalResults/10 + 1, digits = 0)

  pages <- list()

  for (i in 0:edmsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "?_page=", i), flatten = TRUE)
    message("Retrieving page ", i)
    pages[[i + 1]] <- mydata$result$items
  }
}
