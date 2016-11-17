

#' tv_programmes
#'
#' Imports data on TV broadcasts
#' @param AVtype The type of data you want, allows the arguments 'TV' and 'clips'
#' @param TV Returns a data frame with details on all TV broadcasts produced by the Westminster Parliament.
#' @param clips Returns a data frame with details on all video clips produced by the Westminster Parliament.
#' @keywords TV
#' @export
#' @examples \dontrun{
#'
#' x <- tv_programmes('TV')
#'
#'
#' x <- tv_programmes('clips')
#' }

tv_programmes <- function(AVtype = c("TV", "clips")) {

    match.arg(AVtype)

    if (AVtype == "TV") {

        baseurl_tv <- "http://lda.data.parliament.uk/tvprogrammes.json?_pageSize=500"

        tv <- jsonlite::fromJSON(baseurl_tv)

        message("Connecting to API")

        tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:tvJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", tvJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (AVtype == "clips") {

        baseurl_tv <- "http://lda.data.parliament.uk/tvclips.json?_pageSize=500"

        tv <- jsonlite::fromJSON(baseurl_tv)

        message("Connecting to API")

        tvJpage <- round(tv$result$totalResults/tv$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:tvJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_tv, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", tvJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
