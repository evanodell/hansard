

#' elections
#'
#' Imports data on elections
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the date and type of the elction. If NULL, returns the date and type of all available elections. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords Elections
#' @export
#' @examples \dontrun{
#'
#' x <- elections(517994)
#' }


elections <- function(ID = NULL, extra_args=NULL) {

    if (is.null(ID) == FALSE) {

        ID <- paste0("/", ID, ".json?")

        baseurl <- "http://lda.data.parliament.uk/elections"

        message("Connecting to API")

        elect <- jsonlite::fromJSON(paste0(baseurl, ID, extra_args), flatten = TRUE)

        df <- as.data.frame(elect$result$primaryTopic)

    } else {

        ID <- ".json?&_pageSize=500"

        baseurl <- "http://lda.data.parliament.uk/elections"

        message("Connecting to API")

        elect <- jsonlite::fromJSON(paste0(baseurl, ID, extra_args), flatten = TRUE)

        df <- as.data.frame(elect$result$items)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}
