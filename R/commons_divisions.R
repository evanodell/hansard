

#' House of Commons Divisions
#'
#' Imports data on House of Commons divisions
#' @param comsDivType The type of data you want, allows the arguments 'all', 'date', 'no', 'aye'
#' @param all Imports all available divisions.
#' @param date Imports all available divisions on a date.
#' @param no Imports all divisions where a given MP voted no.
#' @param aye Imports all divisions where a given MP voted aye.
#' @keywords divisions
#' @export
#' @examples \donttest{
#' ### NOT RUN:
#' # x <- commons_divisions('all')
#' # Returns all divisions
#'
#' # x <- commons_divisions('date')
#' # Requests a date, and returns all divisions on that date
#'
#' # x <- commons_divisions('no')
#' # Requests an MP ID number, and returns all divisions in which that MP voted no
#'
#' # x <- commons_divisions('aye')
#' # Requests an MP ID number, and returns all divisions in which that MP voted aye
#' }
#'

commons_divisions <- function(comsDivType = c("all", "date", "no", "aye")) {
    
    match.arg(comsDivType)
    
    if (comsDivType == "all") {
        
        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500"
        
        divis <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500")
        
        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (comsDivType == "date") {
        
        divis_date <- readline("Enter division date (yyyy-mm-dd): ")
        
        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/date/"
        
        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/", divis_date, ".json?_pageSize=500"))
        
        if (divis$result$itemsPerPage > divis$result$totalResults) {
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        } else {
            divisJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/date/", divis_date, ".json?_pageSize=500", 
                "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (comsDivType == "no") {
        # WORKING
        
        mp.id <- readline("Enter Member ID: ")
        
        baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="
        
        divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId=", mp.id, "&_pageSize=500"))
        
        divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:divisJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_divis, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", divisJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (comsDivType == "aye") 
        {
            # WORKING
            
            mp.id <- readline("Enter Member ID: ")
            
            baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="
            
            divis <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=", mp.id, "&_pageSize=500"))
            
            divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
            
            pages <- list()
            
            for (i in 0:divisJpage) {
                mydata <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId=", mp.id, 
                  "&_pageSize=500", "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", divisJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }
        }  #else if (comsDivType=='session') {
    
    # baseurl_divis <- 'http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500'
    
    # divis <- jsonlite::fromJSON('http://lda.data.parliament.uk/commonsdivisions.json?_pageSize=500')
    
    # divisJpage <- round(divis$result$totalResults/divis$result$itemsPerPage, digits = 0)
    
    # pages <- list()
    
    # for (i in 0:divisJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_divis, '&_page=', i), flatten = TRUE)
    # message('Retrieving page ', i+1, ' of ', divisJpage+1) pages[[i + 1]] <- mydata$result$items } }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
}



