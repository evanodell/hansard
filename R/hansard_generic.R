

#' Hansard - Generic API Function
#'
#' A semi-generic function for the parliamentary API. Provides greater flexibility, including any newly released features or data not yet included in the individual functions of the hansard package.
#'
#' Users must specify '.json?' in their path. The function uses the default of 10 items per page, to include more include \code{'&_pageSize=[number]'}, e.g. \code{'&_pageSize=500'} to specifiy the maximum amount of 500 items per page.
#'
#' This function does not tidy any variable names.
#'
#'
#' @param path The url path to the data you wish to retrieve.
#'
#' @export
#' @examples \dontrun{
#' x <- hansard_generic('elections.json?')
#'
#' y <- hansard_generic('elections.json?electionType=General Election')
#' }


hansard_generic <- function(path) {

    url <- httr::modify_url("http://lda.data.parliament.uk/", path = utils::URLencode(path))

    mydata <- jsonlite::fromJSON(url)

    jpage <- floor(mydata$result$totalResults/mydata$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(url, "&_page=", i), flatten = TRUE)
        #message("Retrieving page ", i + 1, " of ", genericJPages + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    df

}
