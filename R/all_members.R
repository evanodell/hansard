

### 16a ALL MEMBERS

#' All Members of Parliament
#'
#' This imports data on All Members of Parliament including the Lords and the Commons
#' @param all Imports all available members Defaults to TRUE.
#' @keywords All Members of Parliament
#' @export
#' @examples
#' all_members()

all_members <- function(all = TRUE) {

    baseurl_allMems <- "http://lda.data.parliament.uk/members.json"

    allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json")

    allMemsJpage <- round(allMems$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i, " of ", allMemsJpage)
        pages[[i + 1]] <- mydata$result$items
    }
}



