
### 4 TV PROGRAMMES - NOT DONE

#' TV Programmes
#'
#' This imports data on TV broadcasts
#' @param type The type of data you want, allows the arguments "TV" and "clips"
#' @param TV Imports data on all TV broadcasts produced by the Westminster Parliament
#' @param clips Imports data on all video clips produced by the Westminster Parliament
#' @keywords TV
#' @export
#' @examples
#' x <- tv_programmes("TV")
#' # Returns a data frame with details on all TV broadcasts produced by the
#' # Westminster Parliament
#'
#' x <- tv_programmes("clips")
#' # Returns a data frame with details on all video clips produced by the
#' # Westminster Parliament




tv_programmes  <- function(type =c("TV","clips")) {

  match.arg(type)

  if(type=="TV") {

    baseurl_tv <- "http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500"

    tv <- jsonlite::fromJSON(baseurl_tv)

    tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:tvJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", tvJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="clips") {

    baseurl_tv <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

    tv <- jsonlite::fromJSON(baseurl_tv)

    tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:tvJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", tvJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned

}
