
#' mp_vote_record
#'
#' Accepts an ID number for a member of the House of Commons, and returns a data frame of their votes. Provides similar functionality to the commons_divisions() function, but accepts member IDs as function parameters rather than requesting them from the console.
#' @param mp_id The ID number of a member of the House of Commons. To look up the ID number of a member of the House of Commons use the members_search() function.
#' @param lobby Accepts one of 'all', 'aye', 'no'. 'aye' returns votes where the MP voted 'aye', 'no' returns votes where the MP voted 'no', 'all' returns all available votes by the MP. Defaults to 'all'.
#' @param date Returns all divisions on a given date. Defaults to NULL.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- mp_vote_record(172, lobby='all')
#'
#' x <- mp_vote_record(172, lobby='aye')
#'
#' x <- mp_vote_record(172, lobby='no')
#' }


mp_vote_record <- function(mp_id = NULL, date = NULL, lobby = c("all", "aye", "no")) {
    
    match.arg(lobby)
    
    if (is.null(date) == FALSE) {
        date <- as.character(date)
        date <- paste0("date=", date)
    }
    
    if (lobby == "all") {
        
        message("Retrieving aye votes:")
        df_aye <- mp_vote_record(mp_id = mp_id, date = date, lobby = "aye")
        
        df_aye$vote <- "aye"
        message("Retrieving no votes:")
        df_no <- mp_vote_record(mp_id = mp_id, date = date, lobby = "no")
        
        df_no$divisionNumber <- NULL
        
        df_no$vote <- "no"
        
        df <- rbind(df_aye, df_no)
        df$vote <- as.factor(df$vote)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)
        
        df
        
    } else if (lobby == "aye") {
        
        if (is.null(date) == FALSE) {
            date <- as.character(date)
            date <- paste0("date=", date)
        }
        
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="
        
        message("Connecting to API")
        
        url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", date), flatten = TRUE)
        
        if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
            ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        } else {
            ayeJPage <- 0
        }
        
        pages <- list()
        
        for (i in 0:ayeJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", date, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)
        
        
    } else if (lobby == "no") {
        
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="
        
        message("Connecting to API")
        
        url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", date), flatten = TRUE)
        
        if (url_no$result$itemsPerPage < url_no$result$totalResults) {
            noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        } else {
            noJPage <- 0
        }
        
        pages <- list()
        
        for (i in 0:noJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", date, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", noJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}
