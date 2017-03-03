
#' research_briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible topics and subtopics, call \code{\link{research_topics}}
#' @keywords  Parliamentary Research Briefings
#' @seealso research_topics
#' @export
#' @examples \dontrun{
#'
#' }

research_briefings <- function(topic=NULL, sub_topic=NULL) {

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

            research <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", topic,
                ".json?_pageSize=500"))

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
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500", "&_page=",
                i), flatten = TRUE)
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


#' research_topics
#'
#' Prints or assigns to an object a list of topics and subtopics on  Parliamentary Research Briefings.
#' @rdname research_briefings
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples \dontrun{
#'
#' research_topics()
#'
#' }
research_topics <- function(){


  x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?",flatten=TRUE)


  y <- x$result$items$prefLabel._value

  y <- as.list(y)

  research_topics_list <- list()

  for (i in y) {

    i <- URLencode(i)

    g <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", i, ".json?"), flatten = TRUE)


    i <- URLdecode(i)

    research_topics_list[[i]] <- g$result$items$prefLabel._value


  }


  research_topics_list

}



