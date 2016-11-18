
#' members
#'
#' Imports data on All Members of Parliament including the Lords and the Commons
#' @param house The type of data you want, allows the arguments 'all', 'commons', 'lords', 'lordsInterests'
#' @param all Returns a data frame with information on all members of Parliament, including both the House of Lords and the House of Commons. The data frame includes both current and previous members, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#' @param commons Returns a data frame with information on all members of the House of Commons. The data frame includes both current and previous members of the House of Commons, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#' @param lords Returns a data frame with all available members of the House of Lords.
#' @param lordsInterests  Requests a member ID, and returns a data frame of the registered interests of that member of the House of Lords.
#' @keywords All Members of Parliament
#' @export
#' @examples \dontrun{
#' x <- members('all')
#'
#' x <- members('commons')
#'
#' x <- members('lords')
#'
#' b <- members('lordsInterests')
#'}

members <- function(house = c("all", "commons", "lords", "lordsInterests")) {
    
    match.arg(house)
    
    if (house == "all") {
        
        baseurl_allMems <- "http://lda.data.parliament.uk/members.json?_pageSize=500"
        
        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json?_pageSize=500")
        
        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (house == "commons") {
        
        baseurl_allMems <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"
        
        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500")
        
        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (house == "lords") {
        
        baseurl_allMems <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"
        
        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500")
        
        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (house == "lordsInterests") {
        
        MP.ID <- readline("Enter the members ID number: ")
        
        MP.ID <- URLencode(MP.ID)
        
        baseurl_allMems <- "http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member="
        
        allMems <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member=", 
            MP.ID))
        
        if (allMems$result$totalResults > allMems$result$itemsPerPage) {
            
            allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
        } else {
            allMemsJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, MP.ID, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}
