
#' research_briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible topics and subtopics, call \code{\link{research_topics}}. To see a list of briefing types, call \code{\link{research_types}}.
#' @param topic The topic of the parliamentary briefing.
#' @param sub_topic The subtopic of the parliamentary briefing.
#' @param type The
#' @keywords  Parliamentary Research Briefings
#' @seealso research_topics
#' @export
#' @examples \dontrun{
#' x <- research_briefings("Housing and planning")
#'
#' #Requests can be made using research_topics_list data
#' x <- research_briefings(topic = "Defence", sub_topic = research_topics_list[["Defence"]][10])
#'
#'
#' }

research_briefings <- function(topic=NULL, sub_topic=NULL, type=NULL) {

  if(is.null(topic)==TRUE & is.null(sub_topic)==FALSE){

  g <- rep(seq_along(research_topics_list), sapply(research_topics_list, length))
  dex <- g[match(sub_topic, unlist(research_topics_list))]
  topic <- research_topics_list[dex]

}

  if(is.null(topic)==TRUE & is.null(sub_topic)==TRUE){

    if(is.null(type)==FALSE){
      type <- utils::URLencode(type)
      query <- paste0("&subType.prefLabel=",type)
    } else {
      query <- NULL
    }

        baseurl <- "http://lda.data.parliament.uk/researchbriefings.json?"

        message("Connecting to API")

        research <- jsonlite::fromJSON(paste0(baseurl, query),flatten=TRUE)

        jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

  } else {

    if(is.null(sub_topic)==FALSE){
      sub_topic <- utils::URLencode(sub_topic)
      sub_topic_query <- paste0("/", sub_topic)
    }

    if(is.null(topic)==FALSE){
      topic_query <- utils::URLencode(topic)
    }

    if(is.null(type)==FALSE){
      type <- utils::URLencode(type)
      query <- paste0("&subType.prefLabel=",type)
    } else {
      query <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/researchbriefings/bridgeterm/"

    research <- jsonlite::fromJSON(paste0(baseurl, topic_query, sub_topic_query, ".json?", query),flatten=TRUE)

    jpage <- round(research$result$totalResults/research$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, topic_query, sub_topic_query, ".json?", query, "&_pageSize=500&_page=", i), flatten = TRUE)
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
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples \dontrun{
#'
#' research_topics()
#'
#' }


research_topics <- function(){

  hansard::research_topics_list

}


#' research_topics
#'
#' Prints or assigns to an object a list of topics and subtopics on  Parliamentary Research Briefings.
#' @rdname research_briefings
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples \dontrun{
#' research_types(){
#'
#' }
#'
#'
research_types <- function(){

  x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtypes.json?")

  y <- x$result$items$prefLabel

  y

}







