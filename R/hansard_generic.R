

#' Hansard - Generic API Function
#'
#' A semi-generic function for the parliamentary API. Provides greater flexibility, including any newly released features or data not yet included in the individual functions of the hansard package. Users must specify '.json' in their path.
#' @param path The url path to the data you wish to retrieve
#' @keywords Hansard API
#' @export
#' @examples \dontrun{
#'
#'  x <- hansard_generic('elections.json')
#'
#' }


hansard_generic <- function(path) {
    
    url <- modify_url("http://lda.data.parliament.uk/", path = path)
    
    message("Connecting to API")
    
    mydata <- jsonlite::fromJSON(url)
    
    genericJPages <- round(mydata$result$totalResults/mydata$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:genericJPages) {
        mydata <- jsonlite::fromJSON(paste0(url, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", genericJPages + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- dplyr::bind_rows(pages)
    
    df
    
}
