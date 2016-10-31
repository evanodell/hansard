
### 8 CONSTITUENCIES

#' House of Commons Constituencies
#'
#' This imports data on House of Commons constituencies
#' @param all Imports all available constituencies information Defaults to TRUE.
#' @keywords Constituencies
#' @export
#' @examples
#' constituencies()

constituencies <- function(all = TRUE) {

    baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json"

    conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json")

    contsJpage <- round(conts$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:contsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", contsJpage)
        pages[[i + 1]] <- mydata$result$items
    }
}
