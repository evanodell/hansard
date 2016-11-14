
#' Early Day Motions
#'
#' Imports data on early day motions
#' @param edmType The type of data you want, allows the arguments 'all', 'allSponsors', 'primarySponsor', 'signatures' and 'ID'
#' @param all Returns a data frame of all early day motions
#' @param allSponsors Requests a member's ID, and returns a data frame of early day motions where the given member is a sponsor
#' @param primarySponsor Requests a member's ID, and returns a data frame of early day motions where the given member is the primary sponsor
#' @param signatures Returns a data frame of all early day motion signatures.
#' @param ID Requests an Early Day Motion ID, and returns a data frame with information on that Motion.
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


early_day_motions <- function(edmType = c("all", "allSponsors", "primarySponsor", "signatures", "ID")) {
    
    match.arg(edmType)
    
    if (edmType == "all") {
        
        baseurl_edms <- "http://lda.data.parliament.uk/edms.json?_pageSize=500"
        
        edms <- jsonlite::fromJSON(baseurl_edms)
        
        edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (edmType == "allSponsors") {
        # Not tested
        
        mp.id <- readline("Enter Member ID: ")
        
        baseurl_edms <- "http://lda.data.parliament.uk/edms.json?mnisId="
        
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
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (edmType == "primarySponsor") {
        ## Not tested
        
        mp.id <- readline("Enter Member ID: ")
        
        baseurl_edms <- "http://lda.data.parliament.uk/edmbysponsor.json?mnisId="
        
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
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (edmType == "signatures") {
        ## Not tested
        
        baseurl_edms <- "http://lda.data.parliament.uk/edmsignatures.json?_pageSize=500"
        
        edms <- jsonlite::fromJSON(baseurl_edms)
        
        edmsJpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:edmsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_edms, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", edmsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
        
    } else if (edmType == "ID") {
        ## Returns
        
        edmsID <- readline("Enter an Early Day Motion ID: ")
        
        baseurl_edms <- "http://lda.data.parliament.uk/edms/"
        
        edms <- jsonlite::fromJSON(paste0(baseurl_edms, edmsID, ".json"))
        
        # df <- edms$result$primaryTopic
        
        list <- edms$result$primaryTopic
        
        list
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
