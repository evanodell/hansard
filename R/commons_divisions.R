
### 3 Commons DIVISIONS

#' House of Commons Divisions
#'
#' This imports data on House of Commons divisions
#' @param all Imports all available divisions. Defaults to TRUE.
#' @keywords divisions
#' @export
#' @examples
#' commons_divisions()

commons_divisions <- function(all = TRUE) {

    baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json"

    divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsdivisions.json")

    divisJpage <- round(divis$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:divisJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", divisJpage)
        pages[[i + 1]] <- mydata$result$items
    }
}



