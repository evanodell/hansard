

#' lords_vote_record
#'
#' Accepts an ID number for a member of the House of Lords, and returns a data frame of their votes. Provides similar functionality to the lords_divisions() function, but accepts member IDs as function parameters rather than requesting them from the console.
#' @param lord.id The ID number of a member of the House of Lords. To look up the ID number of a member of the House of Lords use the members_search() function.
#' @param lordsRecord Accepts the arguments 'all', 'content' and 'notContent'. Defaults to 'all'.
#' @param all Returns a data frame with all recorded votes for a given member of the house of lords.
#' @param content Returns a data frame with all divisions where a given lord voted aye.
#' @param notContent Returns a data frame with all divisions where a given lord voted no.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- lords_vote_record(172, 'all')
#' }
#'

lords_vote_record <- function(lord.id, lordsRecord = c("all", "content", "notContent")) {
    
    match.arg(lordsRecord)
    
    if (lordsRecord == "all") {
        
        message("Retrieving content votes:")
        df_content <- lords_vote_record(lord.id, "content")
        
        df_content$vote <- "content"
        message("Retrieving not content votes:")
        df_notContent <- lords_vote_record(lord.id, "notContent")
        
        df_notContent$divisionNumber <- NULL
        
        df_notContent$vote <- "notContent"
        
        common <- intersect(colnames(df_content), colnames(df_notContent))
        
        df <- rbind(subset(df_content, select = common), subset(df_notContent, select = common))
        
        df
        
    } else if (lordsRecord == "content") {
        
        baseurl_aye <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="
        
        message("Connecting to API")
        
        url_aye <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id), flatten = TRUE)
        
        if (url_aye$result$itemsPerPage < url_aye$result$totalResults) {
            ayeJPage <- round(url_aye$result$totalResults/url_aye$result$itemsPerPage, digits = 0)
        } else {
            ayeJPage <- 0
        }
        
        pages <- list()
        
        for (i in 0:ayeJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_aye, lord.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", ayeJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (lordsRecord == "notContent") {
        
        baseurl_no <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="
        
        message("Connecting to API")
        
        url_no <- jsonlite::fromJSON(paste0(baseurl_no, lord.id), flatten = TRUE)
        
        if (url_no$result$itemsPerPage < url_no$result$totalResults) {
            noJPage <- round(url_no$result$totalResults/url_no$result$itemsPerPage, digits = 0)
        } else {
            noJPage <- 0
        }
        
        pages <- list()
        
        for (i in 0:noJPage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_no, lord.id, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", noJPage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    }
    
    df$divisionResult <- as.factor(df$divisionResult)
    df$officialContentsCount <- as.factor(df$officialContentsCount)
    df$officialNotContentsCount <- as.factor(df$officialNotContentsCount)
    df$date._value <- as.Date(df$date._value)
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}
