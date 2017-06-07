

#' A list of Parliamentary Research Briefings topics.
#' @rdname research_briefings_lists
#' @return A list with the different research topics available.
#' @export
#' @seealso \code{\link{research_briefings}}
#' @keywords  Parliamentary Research Briefings
#' @examples  \dontrun{
#'
#' research_topics_list <- research_topics_list()
#' }
#'
research_topics_list <- function() {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?", flatten = TRUE)

    y <- x$result$items$prefLabel._value

    research_topics_list <- as.list(y)

    research_topics_list

}

#' research_subtopics_list
#'
#' A list of Parliamentary Research Briefings subtopics, grouped by topic.
#' @rdname research_briefings_lists
#' @return A list with the different research subtopics available.
#' @export
#' @keywords Parliamentary Research Briefings
#' @examples  \dontrun{
#'
#' research_subtopics_list <- research_subtopics_list()
#'
#' }


research_subtopics_list <- function() {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?", flatten = TRUE)

    y <- x$result$items$prefLabel._value

    research_topics_list <- as.list(y)

    research_subtopics_list <- list()

    for (i in research_topics_list) {

        i <- utils::URLencode(i)

        g <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", i, ".json?"), flatten = TRUE)

        i <- utils::URLdecode(i)

        research_subtopics_list[[i]] <- g$result$items$prefLabel._value

    }

    research_subtopics_list

}



#' research_types_list
#'
#' A list of types of Parliamentary Research Briefings.
#' @rdname research_briefings_lists
#' @return A list with the different types of research briefings.
#' @export
#' @keywords  Parliamentary Research Briefings
#' @examples  \dontrun{
#'
#' research_types_list <- research_types_list()
#'
#' }

research_types_list <- function() {

    x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtypes.json?")

    research_types_list <- as.list(x$result$items$prefLabel$`_value`)

    research_types_list
}


