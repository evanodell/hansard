
### 21 SESSIONS

#' Parliamentary Sessions
#'
#' This imports data on Parliamentary Sessions
#' @param all Imports Parliamentary Sessions Defaults to TRUE.
#' @keywords Parliamentary Sessions
#' @export
#' @examples
#' sessions_info()

sessions_info <- function(all = TRUE) {

    baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json"

    sessions <- jsonlite::fromJSON("http://lda.data.parliament.uk/sessions.json")

    sessionsJpage <- round(sessions$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:sessionsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}



