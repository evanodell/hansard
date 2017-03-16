
#' early_day_motions
#'
#' Imports data on early day motions
#' @param edm_id Accepts the ID number of an early day motion, and returns data on that motion. Note that EDM numbers reset each parliamentary session, so using this as the only parameter will return multiple early day motions. Defaults to NULL.
#' @param session Accepts a parliamentary session, in yyyy/yy format. Defaults to NULL.
#' @param start_date The earliest date to include in the data frame, if calling all early day motions. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame, if calling all early day motions. Defaults to current system date.
#' @param signatures The minimum number of signatures required for inclusion in the data frame. Defaults to 1.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords EDM
#' @export
#' @examples \dontrun{
#'
#' x <- early_day_motions(edm_id = 1073)
#'
#' x <- early_day_motions(edm_id = 1073, session="2015/16")
#'
#' }


early_day_motions <- function(edm_id = NULL, session=NULL,  start_date = "1900-01-01", end_date = Sys.Date(), signatures = 1, extra_args = NULL) {

    if(is.null(edm_id)==FALSE){
      edm_query <- paste0("&edmNumber=", edm_id)
    } else {
      edm_query <- NULL
    }

  if(is.null(session)==FALSE){
    session_query <- paste0("&session.=", session)
  } else {
    session_query <- NULL
  }

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", end_date, "&min-dateTabled=", start_date)

    sig_min <- paste0("&min-numberOfSignatures=", signatures)

    baseurl <- "http://lda.data.parliament.uk/edms"

    message("Connecting to API")

    edms <- jsonlite::fromJSON(paste0(baseurl, ".json?",edm_query, dates, session_query, "&_pageSize=500", sig_min, extra_args), flatten = TRUE)

    jpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json?",edm_query, dates,session_query,  sig_min, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
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
