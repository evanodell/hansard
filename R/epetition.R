
#' epetition
#'
#' Imports data on Epetitions
#' @param ID The ID of a given constituency. If NULL, returns all petitions. Defaults to NULL.
#' @param by_constituency Accepts either TRUE or FALSE. If TRUE, provides a data frame with a breakdown of signatures for each petition, by constituency. Defaults to FALSE.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords ePetitions
#' @export
#' @examples \dontrun{
#' x <- epetition(ID = 706964, by_constituency=TRUE)
#'
#'}

epetition <- function(ID = NULL, by_constituency = FALSE, extra_args = NULL) {
    
    if (is.null(ID) == FALSE) {
        ID <- paste0("/", ID)
    }
    
    if (by_constituency == TRUE) {
        by_constituency <- "/signaturesbyconstituency"
    } else {
        by_constituency <- NULL
    }
    
    baseurl <- "http://lda.data.parliament.uk/epetitions"
    
    message("Connecting to API")
    
    if (is.null(ID) == FALSE & is.null(by_constituency) == TRUE) {
        
        petition <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?", extra_args), flatten = TRUE)
        
        df <- as.data.frame(petition$result$primaryTopic)
        
    } else {
        
        petition <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?&_pageSize=500", extra_args), 
            flatten = TRUE)
        
        jpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?&_pageSize=500", "&_page=", 
                i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
        df$member <- NULL  # Removes superfluous member column
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
