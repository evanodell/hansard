
### 21 SESSIONS - NOT DONE

#' Parliamentary Sessions
#'
#' This imports data on Parliamentary Sessions
#' @param all Imports Parliamentary Sessions Defaults to TRUE.
#' @keywords Parliamentary Sessions
#' @export
#' @examples
#' x <- sessions_info()

sessions_info <- function(all = TRUE) {

    baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json?_pageSize=500"

    sessions <- jsonlite::fromJSON("http://lda.data.parliament.uk/sessions.json?_pageSize=500")

    sessionsJpage <- round(sessions$result$totalResults/sessions$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:sessionsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", sessionsJpage+1)
        pages[[i + 1]] <- mydata$result$items
    }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}



