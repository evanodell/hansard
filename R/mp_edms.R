

#' mp_edms
#'
#' Imports data on early day motions
#' @param edmType The type of data you want, allows the arguments 'signed' and 'sponsor'. Defaults to 'signed'.
#' @param mp.id Required parameter. The ID number of an MP.
#' @param signed Returns a data frame of all early day motions signed by the given MP.
#' @param sponsor Returns a data frame of early day motions where the given member is the primary sponsor or a sponsor.
#' @keywords Early Day Motion
#' @export
#' @examples \dontrun{
#' x <- mp_edms('signed')
#'
#' x <- mp_edms('sponsor')
#'
#' }


mp_edms <- function(mp.id, edmType = c("signed", "sponsor")) {
    
    match.arg(edmType)
    
    if (edmType == "signed") {
        
        baseurl_edms <- "http://lda.data.parliament.uk/edms.json?mnisId="
        
        message("Connecting to API")
        
        edms <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500"))
        
        if (edms$result$totalResults > edms$result$itemsPerPage) {
            
            edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)
            
        } else {
            
            edmsJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (edmType == "sponsor") {
        
        mp.id <- readline("Enter Member ID: ")
        
        baseurl_edms <- "http://lda.data.parliament.uk/edmbysponsor.json?mnisId="
        
        message("Connecting to API")
        
        edms <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500"))
        
        if (edms$result$totalResults > edms$result$itemsPerPage) {
            
            edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)
            
        } else {
            
            edmsJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, mp.id, "&_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


