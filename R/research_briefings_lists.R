
#' Lists of research briefing topics, subtopics and types.
#'
#' Returns lists of research briefing topics, subtopics and types.
#' These functions do not accept any arguments.
#'
#' @rdname research_briefings_lists
#' @return A list with the different research topics/subtopics/types available.
#' @export
#' @seealso [research_briefings()]
#'
#' @section Member details functions:
#' \describe{
#' \item{research_topics_list}{A list with the different research
#' topics available}
#' \item{research_subtopics_list}{A list of Parliamentary Research
#'  Briefings topics}
#' \item{research_types_list}{A list of types of Parliamentary
#' Research Briefings}
#' }
#' @examples  \dontrun{
#' research_topics_list <- research_topics_list()
#'
#' research_subtopics_list <- research_subtopics_list()
#'
#' research_types_list <- research_types_list()
#' }

research_topics_list <- function() {
  x <- jsonlite::fromJSON(paste0(url_util, "researchbriefingtopics.json?"),
    flatten = TRUE
  )

  research_topics_list <- as.list(x$result$items$prefLabel._value)

  research_topics_list
}


#' @rdname research_briefings_lists
#' @export
hansard_research_topics_list <- research_topics_list



#' @rdname research_briefings_lists
#' @export
research_subtopics_list <- function() {
  x <- jsonlite::fromJSON(paste0(url_util, "researchbriefingtopics.json?"),
    flatten = TRUE
  )

  research_topics_list <- as.list(x$result$items$prefLabel._value)

  research_subtopics_list <- list()

  for (i in research_topics_list) {
    i <- utils::URLencode(i)

    g <- jsonlite::fromJSON(paste0(
      url_util, "researchbriefingsubtopics/",
      i, ".json?"
    ), flatten = TRUE)

    i <- utils::URLdecode(i)

    research_subtopics_list[[i]] <- g$result$items$prefLabel._value
  }

  research_subtopics_list
}

#' @rdname research_briefings_lists
#' @export
hansard_research_subtopics_list <- research_subtopics_list


#' @rdname research_briefings_lists
#' @export
research_types_list <- function() {
  x <- jsonlite::fromJSON(paste0(url_util, "researchbriefingtypes.json?"))

  research_types_list <- as.list(x$result$items$prefLabel$`_value`)

  research_types_list
}


#' @rdname research_briefings_lists
#' @export
hansard_research_types_list <- research_types_list
