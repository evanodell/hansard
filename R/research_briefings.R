
#' Parliamentary Research Briefings
#'
#' This imports data on  Parliamentary Research Briefings
#' @param resType The type of data you want, allows the arguments 'all', 'topics', 'types',
#' # 'byTopic','subTopic' and 'topicSubTopic'
#' @param all Imports a data frame with all available Parliamentary Research Briefings.
#' @param topics Imports a data frame with all Parliamentary Research Briefings topics.
#' @param types Imports a data frame with all Parliamentary Research Briefings types.
#' @param subTopic Requests a topic, and imports a data frame with all of the possible
#' # sub-topics for that topic.
#' @param byTopic Requests a topic, and imports a data frame with all available
#' # Parliamentary Research Briefings on that topic
#' @param topicSubTopic Requests a topic and a subtopic, and imports a data frame with
#' # all available
#' # Parliamentary Research Briefings on that subtopic
#' @keywords  Parliamentary Research Briefings
#' @export
#' @examples \donttest{
#' x <- research_briefings('all')
#' # NOT RUN
#' # x <- research_briefings('all')
#' # head(x)
#'
#' x <- research_briefings('topics')
#' # NOT RUN
#' # x <- research_briefings('topics')
#' # print(x)
#'
#' x <- research_briefings('types')
#' # NOT RUN
#' # x <- research_briefings('types')
#' # print(x)
#'
#' x <- research_briefings('byTopic')
#' # NOT RUN
#' # x <- research_briefings('byTopic')
#' # RETURNS:
#' # [1]Sub-topics are case sensititve. To return list of sub-topics, enter yes.
#' # Enter sub-topic:  #yes
#' # RETURNS:
#' # [1] 'Agriculture, animals, food and rural affairs' 'Asylum, immigration and nationality'
#' # [3] 'Business, industry and consumers'             'Communities and families'
#' # [5] 'Crime, civil law, justice and rights'         'Culture, media and sport'
#' # [7] 'Defence'                                      'Economy and finance'
#' # [9] 'Education'                                    'Employment and training'
#' # [11] 'Energy and environment'                       'European Union'
#' # [13] 'Health services and medicine'                 'Housing and planning'
#' # [15] 'International affairs'                        'Parliament, government and politics'
#' # [17] 'Science and technology'                       'Social Security and pensions'
#' # [19] 'Social services'                              'Transport'
#' # Enter Topic. For ease of use, copy and paste the topic: Education
#' # head(x)
#'
#' x <- research_briefings('subTopic')
#' # RETURNS:
#' # [1]Sub-topics are case sensititve. To return list of sub-topics, enter yes.
#' # Enter sub-topic:  #yes
#' # RETURNS:
#' # [1] 'Agriculture, animals, food and rural affairs' 'Asylum, immigration and nationality'
#' # [3] 'Business, industry and consumers'             'Communities and families'
#' # [5] 'Crime, civil law, justice and rights'         'Culture, media and sport'
#' # [7] 'Defence'                                      'Economy and finance'
#' # [9] 'Education'                                    'Employment and training'
#' # [11] 'Energy and environment'                       'European Union'
#' # [13] 'Health services and medicine'                 'Housing and planning'
#' # [15] 'International affairs'                        'Parliament, government and politics'
#' # [17] 'Science and technology'                       'Social Security and pensions'
#' # [19] 'Social services'                              'Transport'
#' # Enter Topic. For ease of use, copy and paste the topic: Education
#' # head(x)
#'
#' x <- research_briefings('topicSubTopic')
#' # NOT RUN
#' # [1]Sub-topics are case sensititve. To return list of sub-topics, enter yes.
#' # Enter sub-topic: yes
#' # RETURNS:
#' # [1] 'Agriculture, animals, food and rural affairs' 'Asylum, immigration and nationality'
#' # [3] 'Business, industry and consumers'             'Communities and families'
#' # [5] 'Crime, civil law, justice and rights'         'Culture, media and sport'
#' # [7] 'Defence'                                      'Economy and finance'
#' # [9] 'Education'                                    'Employment and training'
#' # [11] 'Energy and environment'                       'European Union'
#' # [13] 'Health services and medicine'                 'Housing and planning'
#' # [15] 'International affairs'                        'Parliament, government and politics'
#' # [17] 'Science and technology'                       'Social Security and pensions'
#' # [19] 'Social services'                              'Transport'
#' # Enter Topic. For ease of use, copy and paste the topic:
#' # [1]Sub-topics are case sensititve. To return list of sub-topics, enter yes.
#' # Enter sub-topic:}

research_briefings <- function(resType = c("all", "topics", "types", "byTopic", "subTopic", "topicSubTopic")) {

    match.arg(resType)

    if (resType == "all") {

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings.json?_pageSize=500"

        research <- jsonlite::fromJSON(baseurl_research)

        researchJpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "topics") {

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500"

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

        research <- jsonlite::fromJSON(baseurl_research)

        researchJpage <- 0

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "byTopic") {

        print("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic:  ")

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic: ")

        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))

        if (research$result$totalResults > research$result$itemsPerPage) {

            researchJpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

        } else {
            researchJpage <- 0
        }

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "subTopic") {

        print("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic:  ")

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic: ")

        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefingsubtopics/"

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500"))

        if (research$result$totalResults > research$result$itemsPerPage) {

            researchJpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

        } else {
            researchJpage <- 0
        }

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (resType == "topicSubTopic") {

        print("Topics are case sensititve. To return list of topics, enter yes.")
        topic <- readline("Enter topic:  ")

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (topic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?_pageSize=500")

            print(research$result$items$prefLabel$`_value`)

            topic <- readline("Enter Topic. For ease of use, copy and paste the topic: ")

        }
        print("Sub-topics are case sensititve. To return list of sub-topics, enter yes.")
        subTopic <- readline("Enter sub-topic:  ")

        yesList <- c("yes", "Yes", "yEs", "yeS", "YES", "yES", "YEs", "YeS", "y", "ye", "Y", "YE", "Ye", "yE")

        if (subTopic %in% yesList == TRUE) {

            research <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", topic, ".json?_pageSize=500"))

            print(research$result$items$prefLabel$`_value`)

            subTopic <- readline("Enter sub-topic. For ease of use, copy and paste the sub-topic: ")

        }

        baseurl_research <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"

        research <- jsonlite::fromJSON(paste0(baseurl_research, topic, "/", subTopic, ".json?_pageSize=500"))

        if (research$result$totalResults > research$result$itemsPerPage) {

            researchJpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

        } else {
            researchJpage <- 0
        }

        pages <- list()

        for (i in 0:researchJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_research, topic, ".json?_pageSize=500", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", researchJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}



