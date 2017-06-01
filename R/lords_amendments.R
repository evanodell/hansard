
#' Imports data on House of Lords Amendments. Returns a tibble with all available House of Lords amendments.
#' @param decision The decision on the amendments. Accepts one of 'Withdrawn', 'Agreed', 'Disagreed', 'Pending', 'NotMoved', 'Disposed'. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.POSIXct()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.POSIXct()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on amendments proposed by the House of Lords.
#' @keywords House of Lords Amendments
#' @export
#' @examples \dontrun{
#'
#' x <- lords_amendments()
#'
#' }

lords_amendments <- function(decision = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {
    
    dates <- paste0("&min-bill.date=", as.POSIXct(start_date ), "&max-bill.date=", as.POSIXct(end_date))
    
    if (is.null(decision) == FALSE) {
        decision_query <- paste0("&decision=", decision)
    } else {
        decision_query <- NULL
    }
    
    baseurl <- "http://lda.data.parliament.uk/lordsbillamendments.json?_pageSize=500"
    
    message("Connecting to API")
    
    ammend <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, extra_args), flatten = TRUE)
    
    jpage <- round(ammend$result$totalResults/ammend$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, decision_query, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- tibble::as_tibble(dplyr::bind_rows(pages))
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            df$bill.date._value <- as.POSIXct(df$bill.date._value)
            
            df$bill.date._datatype <- "POSIXct"
            
            df <- hansard::hansard_tidy(df, tidy_style)
            
            df
            
        } else {
            
            df
            
        }
        
    }
}
