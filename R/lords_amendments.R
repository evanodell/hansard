#' lords_amendments
#'
#' Imports data on House of Lords Amendments
#' @param all Returns a data frame with all available House of Lords Amendments Defaults to TRUE.
#' @keywords House of Lords Amendments
#' @export
#' @examples \dontrun{
#' x <- lords_amendments()
#' }

lords_amendments <- function(all = TRUE) {

    baseurl_lordsAmmend <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

    message("Connecting to API")

    lordsAmmend <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500")

    lordsAmmendJpage <- round(lordsAmmend$result$totalResults/lordsAmmend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:lordsAmmendJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsAmmend, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", lordsAmmendJpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


lords_ammendments <- function(all=TRUE) {
  .Deprecated("lords_amendments") #include a package argument, too
  lords_amendments(all = TRUE)
}

