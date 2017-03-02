
#' early_day_motions
#'
#' Imports data on early day motions
#' @param edm_id Accepts a given MP and returns a data frame with all early day motions signed by that MP. Defaults to NULL.
#' @param start_date The earliest date to include in the data frame, if calling all early day motions. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame, if calling all early day motions. Defaults to current system date.
#' @param signatures The minimum number of signatures required for inclusion in the data frame. Defaults to 1.
#' @keywords EDM
#' @export
#' @examples \dontrun{
#' x <- early_day_motions('all')
#'
#' x <- early_day_motions('allSponsors')
#'
#' x <- early_day_motions('all')
#'
#' x <- early_day_motions('primarySponsor')
#'
#' x <- early_day_motions('signatures')
#'
#' x <- early_day_motions('ID')
#' }


early_day_motions <- function(edm_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), signatures = 1) {

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", end_date, "&min-dateTabled=", start_date)

    sig_min <- paste0("&min-numberOfSignatures=" , signatures)

    baseurl <- "http://lda.data.parliament.uk/edms"

    message("Connecting to API")

    edms <- jsonlite::fromJSON(paste0(baseurl, ".json?", dates, "&_pageSize=500", sig_min), flatten = TRUE)

    jpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?", dates, sig_min, "&_pageSize=500&_page=", i), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
