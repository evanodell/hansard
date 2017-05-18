

#' Imports data on papers laid before the House.
#' @param withdrawn If TRUE, only returns withdrawn papers. Defaults to FALSE.
#' @param house The house the paper was laid in. Accepts 'commons' and 'lords'. If NULL, returns both House of Commons and House of Lords. This parameter is case-insensitive. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on papers laid before the given House.
#' @keywords Papers Laid
#' @export
#' @examples \dontrun{
#' x <- papers_laid(withdrawn = FALSE, house = 'commons')
#'
#' x <- papers_laid(withdrawn = TRUE, house = NULL)
#' }
#'

papers_laid <- function(withdrawn = FALSE, house = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {
    
    if (is.null(house) == FALSE) {
        house <- tolower(house)
        
        if (house == "commons") {
            house <- "&legislature.prefLabel=House of Commons"
            house <- utils::URLencode(house)
        } else if (house == "lords") {
            house <- "&legislature.prefLabel=House of Lords"
            house <- utils::URLencode(house)
        } else {
            house <- NULL
        }
        
    }
    
    if (withdrawn == TRUE) {
        query <- "&withdrawn=true"
    } else {
        query <- "&withdrawn=false"
    }
    
    dates <- paste0("&max-ddpModified=", as.Date(end_date), "&min-ddpModified=", as.Date(start_date))
    
    baseurl <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"
    
    message("Connecting to API")
    
    papers <- jsonlite::fromJSON(paste0(baseurl, query, house, dates, extra_args), flatten = TRUE)
    
    jpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, house, dates, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- tibble::as_tibble(dplyr::bind_rows(pages))
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            df$dateLaid._value <- as.Date(df$dateLaid._value)
            
            df$dateLaid._datatype <- "Dat"
            
            if (withdrawn == TRUE) {
                
                df$dateWithdrawn._value <- gsub("T", " ", df$dateWithdrawn._value)
                
                df$dateWithdrawn._value <- as.Date(lubridate::parse_date_time(df$dateWithdrawn._value, "Y-m-d H:M:S"))
                
                df$dateWithdrawn._datatype <- "Date"
                
            }
            
            df <- hansard_tidy(df, tidy_style)
            
            df
            
        } else {
            
            df
            
        }
        
    }
}
