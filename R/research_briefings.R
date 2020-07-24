
#' Parliamentary Research Briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible
#' topics call [research_topics_list()] or
#' [research_subtopics_list()] for both topics and subtopics. To
#' see a list of briefing types, call [research_types_list()]. This
#' function can return results with newlines in the text of the abstract or
#' description of the research briefing, represented as `'\\n'`.
#'
#' @param topic The topic of the parliamentary briefing.
#' Defaults to `NULL`.
#'
#' @param subtopic The subtopic of the parliamentary briefing.
#' Defaults to `NULL`.
#'
#' @param type The type of research briefing. Defaults to `NULL`.
#' @inheritParams all_answered_questions
#' @return A tibble with details on parliamentary research briefings on
#' the given topic.
#' @seealso [research_subtopics_list()]
#' @seealso [research_types_list()]
#' @seealso [research_topics_list()]
#' @export
#' @examples
#' \dontrun{
#' x <- research_briefings("Housing and planning")
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
#'
research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL,
                               extra_args = NULL, tidy = TRUE,
                               tidy_style = "snake", verbose = TRUE) {
  veb(verbose)

  if (is.null(topic) & is.null(subtopic)) {
    if (!is.null(type)) {
      type_query <- utils::URLencode(
        paste0("&subType.prefLabel=", type)
      )
    } else {
      type_query <- ""
    }

    query <- paste0(url_util, "researchbriefings.json?", type_query, extra_args)

    research <- jsonlite::fromJSON(paste0(
      query, "&_pageSize=1"
    ),
    flatten = TRUE
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

    if (!is.null(subtopic)) {
      subtopic_query <- utils::URLencode(paste0("/", subtopic))
    } else {
      subtopic_query <- ""
    }

    if (!is.null(topic)) {
      topic_query <- utils::URLencode(topic)
    } else {
      topic_query <- ""
    }

    if (!is.null(type)) {
      null_type_query <- paste0("&subType.prefLabel=", type)
    } else {
      null_type_query <- ""
    }

    query <- paste0(
      url_util, "researchbriefings/bridgeterm/", topic_query, subtopic_query,
      ".json?", null_type_query, extra_args
    )

jpage <- jpage_func(query)

    

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- research_tidy(df, tidy_style) ## in utils-research.R
    }

    df
  }
}

#' @rdname research_briefings
#' @export
hansard_research_briefings <- research_briefings
