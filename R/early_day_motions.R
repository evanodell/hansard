
#' Early Day Motions
#'
#' This imports data on early day motions
#' @param all Imports all available EDMs. Defaults to TRUE.
#' @keywords EDM
#' @export
#' @examples
#' early_day_motions("all")
#' #Returns a data frame of all early day motions
#'
#'
#' early_day_motions("signatures")
#' #Returns a data frame of all early day motions signatures

### 9 EARLY DAY MOTIONS

early_day_motions <- function(type =c("all","signatures")) {

  match.arg(type)

  if(type=="all") {#Working

  baseurl_edms <- "http://lda.data.parliament.uk/edms.json?_pageSize=500"

  edms <- jsonlite::fromJSON(baseurl_edms)

  edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:edmsJpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
    message("Retrieving page ", i+1, " of ", edmsJpage+1)
    pages[[i + 1]] <- mydata$result$items
    }
  } else if (type=="signatures"){#Not working

    baseurl_edms <- "http://lda.data.parliament.uk/edmsignatures.json?_pageSize=500"

    edms <- jsonlite::fromJSON(baseurl_edms)

    edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:edmsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", edmsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }
  } #else if (type=="signature"){

#    baseurl_edms <- "http://lda.data.parliament.uk/edms.json?_pageSize=500"

#    edms <- jsonlite::fromJSON("http://lda.data.parliament.uk/edms.json?_pageSize=500")

#    edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

#    pages <- list()

#    for (i in 0:edmsJpage) {
#      mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
#      message("Retrieving page ", i+1, " of ", edmsJpage+1)
#      pages[[i + 1]] <- mydata$result$items
#    }
#  }

  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
