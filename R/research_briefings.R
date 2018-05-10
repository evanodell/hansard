
#' Parliamentary Research Briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible
#' topics call \code{\link{research_topics_list}} or
#' \code{\link{research_subtopics_list}} for both topics and subtopics. To
#' see a list of briefing types, call \code{\link{research_types_list}}. This
#' function can return results with newlines in the text of the abstract or
#' description of the research briefing, represented as \code{'\\n'}.
#' @param topic The topic of the parliamentary briefing.
#' Defaults to \code{NULL}.
#' @param subtopic The subtopic of the parliamentary briefing.
#' Defaults to \code{NULL}.
#' @param type The type of research briefing. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with details on parliamentary research briefings on
#' the given topic.
#' @seealso \code{\link{research_subtopics_list}}
#' @seealso \code{\link{research_types_list}}
#' @seealso \code{\link{research_topics_list}}
#' @export
#' @examples \dontrun{
#' x <- research_briefings('Housing and planning')
#'
#' # Requests can be made using lists created using `research_topics_list`
#' # and `research_subtopics_list`
#'
#' research_topics_list <- research_topics_list()
#'
#' x <- research_briefings(topic = research_topics_list[[7]])
#'
#' research_subtopics_list <- research_subtopics_list()
#'
#' x <- research_briefings(subtopic = research_subtopics_list[[7]][10])
#'
#' # Requests for certain briefing types can also be made using lists
#' # created with `research_types_list`.
#'
#' research_types_list <- research_types_list()
#'
#' x <- research_briefings(type = research_types_list[[3]])
#' }

research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL,
                               extra_args = NULL, tidy = TRUE,
                               tidy_style = "snake_case", verbose = TRUE) {
  if (verbose == TRUE) {
    message("Connecting to API")
  }

  if (is.null(topic) == TRUE & is.null(subtopic) == TRUE) {
    type_query <- ifelse(
      is.null(type) == FALSE,
      utils::URLencode(
        paste0("&subType.prefLabel=", type)
      ),
      ""
    )

    baseurl <- paste0(url_util, "researchbriefings.json?")

    research <- jsonlite::fromJSON(paste0(
      baseurl, type_query,
      extra_args, "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(research$result$totalResults / 500)

    query <- paste0(
      baseurl, type_query, extra_args,
      "&_pageSize=500&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  } else {
    if (is.null(topic) == TRUE & is.null(subtopic) == FALSE) {
      g <- rep(
        seq_along(hansard::research_subtopics_list()),
        lapply(hansard::research_subtopics_list(), length)
      )

      dex <- g[match(
        subtopic,
        unlist(hansard::research_subtopics_list())
      )]

      topic <- names(hansard::research_subtopics_list())[dex]
    }

    subtopic_query <- ifelse(
      is.null(subtopic) == FALSE,
      utils::URLencode(paste0("/", subtopic)),
      ""
    )

    topic_query <- ifelse(
      is.null(topic) == FALSE,
      utils::URLencode(topic),
      ""
    )

    null_type_query <- ifelse(
      is.null(type) == FALSE, utils::URLencode(
        paste0("&subType.prefLabel=", type)
      ),
      ""
    )

    baseurl <- paste0(url_util, "researchbriefings/bridgeterm/")

    research <- jsonlite::fromJSON(paste0(
      baseurl, topic_query,
      subtopic_query, ".json?",
      null_type_query, extra_args,
      "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(research$result$totalResults / 500)

    query <- paste0(
      baseurl, topic_query, subtopic_query,
      ".json?", null_type_query, extra_args,
      "&_pageSize=500&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- research_tidy(df, tidy_style) ## in utils-research.R
    }

    df
  }
}

#' @rdname research_briefings
#' @export
hansard_research_briefings <- research_briefings
