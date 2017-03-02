
#' research_briefings
#'
#' Imports data on  Parliamentary Research Briefings
#' @param resType The type of data you want, allows the arguments 'all', 'topics', 'types',
#' # 'byTopic','subTopic' and 'topicSubTopic'
#' @param all Imports a data frame with all available Parliamentary Research Briefings.
#' @param topics Imports a data frame with all Parliamentary Research Briefings topics.
#' @param types Imports a data frame with all Parliamentary Research Briefings types.
#' @param byTopic Requests a topic, and imports a data frame with all available Parliamentary Research Briefings on that topic
#' @param subTopic Requests a topic, and imports a data frame with all of the possible sub-topics for that topic.
#' @param topicSubTopic Requests a topic and a subtopic, and imports a data frame with all available Parliamentary Research Briefings on that subtopic
#' @keywords  Parliamentary Research Briefings
#' @export
#' @examples \dontrun{
#' x <- research_briefings('all')
#'
#' x <- research_briefings('topics')
#'
#' x <- research_briefings('types')
#'
#' x <- research_briefings('byTopic')
#'
#' x <- research_briefings('subTopic')
#'
#' x <- research_briefings('topicSubTopic')
#' }

research_briefings <- function(resType = c("all", "topics", "types", "byTopic", "subTopic", "topicSubTopic")) {
    
    match.arg(resType)
    
    if (resType == "all") {
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json?_pageSize=500"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(baseurl_research)
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resType == "topics") {
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(baseurl_research)
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resType == "types") {
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingtypes.json?_pageSize=500"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(baseurl_research)
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resType == "byTopic") {
        
        message("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic:  ")
        topic <- URLencode(topic)
        
        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")
        
        if (topic %in% yesList == TRUE) {
            
            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")
            
            print(research$result$items$prefLabel$`_value`)
            
            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)
            
        }
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resType == "subTopic") {
        
        topic <- readline("Enter topic:  ")
        topic <- URLencode(topic)
        
        
        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")
        
        if (topic %in% yesList == TRUE) {
            
            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")
            
            print(research$result$items$prefLabel$`_value`)
            
            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)
        }
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingsubtopics/"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (resType == "topicSubTopic") {
        
        print("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic: ")
        topic <- URLencode(topic)
        
        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")
        
        if (topic %in% yesList == TRUE) {
            
            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")
            
            print(research$result$items$prefLabel$`_value`)
            
            topic <- readline("Enter Topic. For ease of use, copy and paste the topic (do not include quotes): ")
            topic <- URLencode(topic)
            
        }
        message("Sub-topics are case sensititve. To return list of sub-topics, enter yes.")
        subTopic <- readline("Enter sub-topic:  ")
        subTopic <- URLencode(subTopic)
        
        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")
        
        if (subTopic %in% yesList == TRUE) {
            
            research <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", topic, ".json?_pageSize=500"))
            
            print(research$result$items$prefLabel$`_value`)
            
            subTopic <- readline("Enter sub-topic. For ease of use, copy and paste the sub-topic: ")
            subTopic <- URLencode(subTopic)
            
        }
        
        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"
        
        message("Connecting to API")
        
        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500"))
        
        researchJpage <- 0
        
        pages <- list()
        
        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}



