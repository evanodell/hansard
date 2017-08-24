
#' Parliamentary Research Briefings
#'
#' Imports data on  Parliamentary Research Briefings. To see a list of possible topics call \code{\link{research_topics_list}} or \code{\link{research_subtopics_list}} for both topics and subtopics. To see a list of briefing types, call \code{\link{research_types_list}}. This function can return results with newlines in the text of the abstract or description of the research briefing, represented as \code{'\\n'}.
#' @param topic The topic of the parliamentary briefing. Defaults to \code{NULL}.
#' @param subtopic The subtopic of the parliamentary briefing. Defaults to \code{NULL}.
#' @param type The type of research briefing. Defaults to \code{NULL}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on parliamentary research briefings on the given topic.
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
#' # created with 'research_types_list'.
#'
#' research_types_list <- research_types_list()
#'
#' x <- research_briefings(type = research_types_list[[3]])
#' }

research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(topic) == TRUE & is.null(subtopic) == TRUE) {

        if (is.null(type) == FALSE) {
            type <- utils::URLencode(type)
            query <- paste0("&subType.prefLabel=", type)
        } else {
            query <- NULL
        }

        baseurl <- "http://lda.data.parliament.uk/researchbriefings.json?"

        if(verbose==TRUE){message("Connecting to API")}

        research <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

        jpage <- floor(research$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

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

        research <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?", query, extra_args), flatten = TRUE)

        jpage <- floor(research$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, topic_query, subtopic_query, ".json?", query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$date._value <- gsub("T", " ", df$date._value)

            df$date._value <- lubridate::parse_date_time(df$date._value, "Y-m-d H:M:Sz!*")

            df$date._datatype <- "POSIXct"

            df$description <- as.character(df$description)

            df$description[df$description == "NULL"] <- NA

            for (i in 1:nrow(df)) {

                if (is.null(df$section[[i]]) == FALSE) {

                  df$section[[i]] <- hansard_tidy(df$section[[i]], tidy_style)

                }
            }

            df <- hansard_tidy(df, tidy_style)

        } else {

            df

        }

    }

}

#' @rdname research_briefings
#' @export
hansard_research_briefings <- function(topic = NULL, subtopic = NULL, type = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- research_briefings(topic = topic, subtopic = subtopic, type = type, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
