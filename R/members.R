
#' members
#'
#' Imports data on All Members of Parliament including the Lords and the Commons
#' @param ID The ID of a member of the House of Commons or the House of Lords. Defaults to NULL. If NULL, returns a data frame of all members. If not NULL, returns a data frame with basic information on that member.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords All Members of Parliament
#' @export
#' @examples \dontrun{
#' x <- members(172)
#'
#' x <- commons_members()
#'
#' x <- lords_members()
#'
#' x <- commons_interests()
#'
#' x <- lords_interests(530)
#'}

members <- function(ID = NULL, extra_args = NULL) {
    
    if (is.null(ID) == TRUE) {
        query <- ".json?_pageSize=500"
    } else {
        query <- ".json?"
    }
    
    baseurl <- "http://lda.data.parliament.uk/members"
    
    message("Connecting to API")
    
    members <- jsonlite::fromJSON(paste0(baseurl, ID, query, extra_args), flatten = TRUE)
    
    if (is.null(ID) == TRUE) {
        
        jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ID, query, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else {
        
        df <- as.data.frame(members$result$primaryTopic)
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}

#' commons_members
#' @export
#' @rdname members
commons_members <- function(extra_args = NULL) {
    
    baseurl <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"
    
    message("Connecting to API")
    
    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)
    
    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


# lords_members
#' @export
#' @rdname members
lords_members <- function(extra_args = NULL) {
    
    baseurl <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"
    
    message("Connecting to API")
    
    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)
    
    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


# commons_interests
#' @export
#' @rdname members
commons_interests <- function(extra_args = NULL) {
    
    baseurl <- "http://lda.data.parliament.uk/commonsregisteredinterests.json?_pageSize=500"
    
    message("Connecting to API")
    
    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)
    
    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}



# lords_interests
#' @param peer_id The ID of a member of the house of lords. If NULL, returns a data frame with all listed financial interests for all members. Defaults to NULL.
#' @rdname members
#' @export
lords_interests <- function(peer_id = NULL, extra_args = NULL) {
    
    if (is.null(peer_id) == TRUE) {
        query <- ".json?_pageSize=500"
    } else {
        query <- paste0(".json?member=", peer_id, "&_pageSize=500")
    }
    
    baseurl <- "http://lda.data.parliament.uk/lordsregisteredinterests"
    
    message("Connecting to API")
    
    members <- jsonlite::fromJSON(paste0(paste0(baseurl, extra_args), query), flatten = TRUE)
    
    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}




