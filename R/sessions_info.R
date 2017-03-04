
#' sessions_info
#'
#' Imports data on Parliamentary Sessions. Note that due to the date format used by the API, if \code{days}==TRUE and the \code{end_date} and \code{start_date} parameters are not set to the default values, the function downloads all available data and then subsets the data frame between the two given dates.
#' @param days If TRUE, returns data for all available days. If FALSE, returns data on each parliamentary session. Defaults to FALSE.
#' @param start_date The earliest date to include in the data frame. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @keywords Parliamentary Sessions
#' @export
#' @examples \dontrun{
#'
#' x <- sessions_info(days=TRUE)
#'
#' }

sessions_info <- function(days = FALSE, start_date = "1900-01-01", end_date = Sys.Date()) {
    
    if (days == FALSE) {
        dates <- paste0("&_properties=startDate&max-startDate=", end_date, "&min-startDate=", start_date)
        query <- NULL
    } else {
        dates <- NULL
        query <- "/days"
    }
    
    baseurl <- "http://lda.data.parliament.uk/sessions"
    
    message("Connecting to API")
    
    session <- jsonlite::fromJSON(paste0(baseurl, query, ".json?_pageSize=500", dates))
    
    jpage <- round(session$result$totalResults/session$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?_pageSize=500&_page=", i, dates), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
    if (days == TRUE) {
        df$date._value <- as.Date(df$date._value)
        df <- df[df$date._value <= end_date & df$date._value >= start_date, ]
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
