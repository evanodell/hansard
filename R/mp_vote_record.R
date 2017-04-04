
#' mp_vote_record
#'
#' Accepts an ID number for a member of the House of Commons, and returns a data frame of their votes.
#' @param mp_id The ID number of a member of the House of Commons.
#' @param lobby Accepts one of 'all', 'aye' or 'no'. 'aye' returns votes where the MP voted 'aye', 'no' returns votes where the MP voted 'no', 'all' returns all available votes by the MP. Defaults to 'all'.
#' @param start_date The earliest date to include in the data frame. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- mp_vote_record(172, lobby='all')
#'
#' x <- mp_vote_record(172, lobby='aye')
#'
#' x <- mp_vote_record(172, lobby='no')
#'
#' # the extra_args parameter allows the inclusion of additional arguments or
#' # queries that are not available through the function's parameters
#' x <- mp_vote_record(172, extra_args = '&session=2016/17')
#'
#'
#' }


mp_vote_record <- function(mp_id = NULL, lobby = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {
    
    if (is.null(extra_args) == FALSE) {
        extra_args <- utils::URLencode(extra_args)
    }
    
    if (is.null(mp_id) == TRUE) {
        stop("mp_id must not be empty", call. = FALSE)
    }
    
    dates <- paste0("&_properties=date&max-date=", end_date, "&min-date=", start_date)
    
    if (lobby == "aye") {
        
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="
        
        message("Connecting to API")
        
        url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)
        
        jpage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- dplyr::bind_rows(pages)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)
        
    } else if (lobby == "no") {
        
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="
        
        message("Connecting to API")
        
        url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)
        
        jpage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- dplyr::bind_rows(pages)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)
        
    } else {
        
        message("Retrieving aye votes:")
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="
        
        message("Connecting to API")
        
        url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)
        
        jpage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df_aye <- dplyr::bind_rows(pages)
        
        df_aye$vote <- "aye"
        
        message("Retrieving no votes:")
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="
        
        message("Connecting to API")
        
        url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)
        
        jpage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, extra_args, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df_no <- dplyr::bind_rows(pages)
        
        df_no$divisionNumber <- NULL
        
        df_no$vote <- "no"
        
        df <- rbind(df_aye, df_no)
        df$vote <- as.factor(df$vote)
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            
            df$date._datatype <- as.factor(df$date._datatype)
            df$date._value <- as.Date(df$date._value)
            
            df <- hansard_tidy(df)
            
            df
            
        } else {
            
            df
            
        }
        
    }
    
    
}
