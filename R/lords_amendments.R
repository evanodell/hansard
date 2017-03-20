#' lords_amendments
#'
#' Imports data on House of Lords Amendments. Returns a data frame with all available House of Lords Amendments. Defaults to TRUE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords House of Lords Amendments
#' @export
#' @examples \dontrun{
#' x <- lords_amendments()
#' }


lords_amendments <- function(withdrawn=NULL, extra_args = NULL) {

  if(withdrawn==TRUE){
    withdrawn_query <- "&withdrawn=true"
  } else if(withdrawn==FALSE){
    withdrawn_query <- "&withdrawn=false"
  } else {
    withdrawn_query <- NULL
  }


    baseurl <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

    message("Connecting to API")

    ammend <- jsonlite::fromJSON(paste0(baseurl, withdrawn_query, extra_args), flatten = TRUE)

    jpage <- round(ammend$result$totalResults/ammend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, withdrawn_query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


lords_ammendments <- function(all = TRUE) {
    .Deprecated("lords_amendments")
    lords_amendments()
}

