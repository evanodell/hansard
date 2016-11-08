
### 21 SESSIONS

#' Parliamentary Sessions
#'
#' This imports data on Parliamentary Sessions
#' @param type The type of data being requested, allows the arguments "all" and "days"
#' @param all Imports information on all available parliamentary sessions
#' @param days Imports information on the days in all available parliamentary sessions
#' @keywords Parliamentary Sessions
#' @export
#' @examples
#' x <- sessions_info("all")
#' # NOT RUN
#' # x <- sessions_info("all")
#' # head(x)
#' #
#' # NOT RUN
#' # x <- sessions_info("days")
#' # head(x)


sessions_info <- function(type =c("all", "days")) {

      match.arg(type)

      if(type=="all") {

        baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json?_pageSize=500"

        sessionsJpage <- 0

        pages <- list()

        for (i in 0:sessionsJpage) {
          mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
          message("Retrieving page ", i+1, " of ", sessionsJpage+1)
          pages[[i + 1]] <- mydata$result$items
        }

      } else if (type=="days"){

        baseurl_sessions <- "http://lda.data.parliament.uk/sessions/days.json?_pageSize=500"

        sessionsJpage <- 0

        pages <- list()

        for (i in 0:sessionsJpage) {
          mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "&_page=", i), flatten = TRUE)
          message("Retrieving page ", i+1, " of ", sessionsJpage+1)
          pages[[i + 1]] <- mydata$result$items
        }

      }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}



