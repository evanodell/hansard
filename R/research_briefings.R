
#' research_briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible topics call \code{\link{research_topics_list}} or \code{\link{research_subtopics_list}} for both topics and subtopics. To see a list of briefing types, call \code{\link{research_types_list}}.
#' @param topic The topic of the parliamentary briefing.
#' @param subtopic The subtopic of the parliamentary briefing.
#' @param type The type of research briefing.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords  Parliamentary Research Briefings
#' @seealso research_topics
#' @export
#' @examples \dontrun{
#' x <- research_briefings('Housing and planning')
#'
#' # Requests can be made using lists created using `research_topics_list`
#' # and `research_subtopics_list`
#' x <- research_briefings(topic = research_topics_list[[7]])
#'
#' x <- research_briefings(subtopic = research_subtopics_list[[7]][10])
#'
#' # Requests for certain briefing types can also be made using lists
#' # created with 'research_types_list'.
#' x <- research_briefings(type = research_types_list[[3]])
#'
#'
#' }

research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL, extra_args = NULL) {
    
    if (is.null(topic) == TRUE & is.null(subtopic) == TRUE) {
        
        if (is.null(type) == FALSE) {
            type <- utils::URLencode(type)
            query <- paste0("&subType.prefLabel=", type)
        } else {
            query <- NULL
        }
        
        baseurl <- "http://lda.data.parliament.uk/researchbriefings.json?&_pageSize=500"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)
        
        jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- dplyr::bind_rows(pages)
        
    } else {
        
        if (is.null(topic) == TRUE & is.null(subtopic) == FALSE) {
            
            g <- rep(seq_along(hansard::research_subtopics_list()), sapply(hansard::research_subtopics_list(), length))
            dex <- g[match(subtopic, unlist(hansard::research_subtopics_list()))]
            topic <- names(hansard::research_subtopics_list())[dex]
            
        }
        
        if (is.null(subtopic) == FALSE) {
            subtopic <- utils::URLencode(subtopic)
            subtopic_query <- paste0("/", subtopic)
        } else {
            subtopic_query <- NULL
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
        
        research <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?&_pageSize=500", query, 
            extra_args), flatten = TRUE)
        
        jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?", query, "&_pageSize=500&_page=", 
                i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- dplyr::bind_rows(pages)
        
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
    
}






