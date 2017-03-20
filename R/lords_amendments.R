#' lords_amendments
#'
#' Imports data on House of Lords Amendments. Returns a data frame with all available House of Lords Amendments. Defaults to TRUE.
#' @param decision The decision on the amendements. Accepts one of "Withdrawn", "Agreed", "Disagreed", "Pending", "NotMoved", "Disposed".
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords House of Lords Amendments
#' @export
#' @examples \dontrun{
#' x <- lords_amendments()
#' }


lords_amendments <- function(decision=NULL, extra_args = NULL) {

  if(is.null(decision)==FALSE){
   decision_query <- paste0("&decision=",decision)
  } else {
   decision_query <- NULL
  }

    baseurl <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"

    message("Connecting to API")

    ammend <- jsonlite::fromJSON(paste0(baseurl, decision_query, extra_args), flatten = TRUE)

    jpage <- round(ammend$result$totalResults/ammend$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, decision_query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

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

