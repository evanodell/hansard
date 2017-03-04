
#' research_briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible topics and subtopics, call \code{\link{research_topics}}. To see a list of briefing types, call \code{\link{research_types_list}}.
#' @param topic The topic of the parliamentary briefing.
#' @param subtopic The subtopic of the parliamentary briefing.
#' @param type The type of research briefing.
#' @keywords  Parliamentary Research Briefings
#' @seealso research_topics
#' @export
#' @examples \dontrun{
#' x <- research_briefings('Housing and planning')
#'
#' #Requests can be made using the `research_topics_list`
#' # and `research_subtopics_list` included in the data
#' x <- research_briefings(topic = hansard::research_topics_list[[7]])
#'
#' x <- research_briefings(subtopic = hansard::research_subtopics_list[[7]][10])
#'
#' # Requests for certain briefing types can also be made using 'research_types_list'.
#' x <- research_briefings(type = hansard::research_types_list[[3]])
#'
#'
#' }

research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL) {
    
    
    
    if (is.null(topic) == TRUE & is.null(subtopic) == TRUE) {
        
        if (is.null(type) == FALSE) {
            type <- utils::URLencode(type)
            query <- paste0("&subType.prefLabel=", type)
        } else {
            query <- NULL
        }
        
        baseurl <- "http://lda.data.parliament.uk/researchbriefings.json?"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(paste0(baseurl, query), flatten = TRUE)
        
        jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else {
        
        if (is.null(topic) == TRUE & is.null(subtopic) == FALSE) {
            
            g <- rep(seq_along(hansard::research_subtopics_list), sapply(hansard::research_subtopics_list, length))
            dex <- g[match(subtopic, unlist(hansard::research_subtopics_list))]
            topic <- names(hansard::research_subtopics_list)[dex]
            
        }
        
        if (is.null(subtopic) == FALSE) {
            subtopic <- utils::URLencode(subtopic)
            subtopic_query <- paste0("/", subtopic)
        }
        
        if (is.null(topic) == FALSE) {
            topic_query <- utils::URLencode(topic)
        }
        
        if (is.null(type) == FALSE) {
            type <- utils::URLencode(type)
            query <- paste0("&subType.prefLabel=", type)
        } else {
            query <- NULL
        }
        
        baseurl <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"
        
        research <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?", query), flatten = TRUE)
        
        jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?", query, "&_pageSize=500&_page=", 
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
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


#' research_topics
#'
#' Prints or assigns to an object a list of topics and subtopics on  Parliamentary Research Briefings.
#' @rdname research_briefings
#' @param subtopics If TRUE, returns full list with all topics and subtopics. If FALSE, only returns list of topics. Defaults to TRUE.
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples
#'
#' research_topics()
#'
#'
research_topics <- function(subtopics = TRUE) {
    
    if (subtopics == TRUE) {
        hansard::research_subtopics_list
    } else {
        hansard::research_topics_list
    }
    
    
    
}


#' research_topics
#'
#' Prints or assigns to an object a list types of Parliamentary Research Briefings.
#' @rdname research_briefings
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples
#' research_types()
#'
#'
#'
#'
research_types <- function() {
    
    hansard::research_types_list
}







