

#' Individual epetitions
#'
#' Imports data on a given epetition. For bulk epetion data,
#' see [epetition_tibble()].
#'
#' @param ID The ID of a given petition. If `NULL`, returns all
#' epetitions. Defaults to `NULL`. See [epetition_tibble()]
#' for a greater degree of flexibility when querying all epetitions.
#'
#' @param by_constituency Accepts either `TRUE` or `FALSE`.
#' If `TRUE`, provides a tibble with a breakdown of signatures for
#' each petition, by constituency. Defaults to `FALSE`.
#' @inheritParams all_answered_questions
#' @return A tibble with details on electronic petitions
#' submitted to parliament.
#' @seealso [epetition_tibble()]
#'
#' @export
#' @examples
#' \dontrun{
#' x <- epetition(ID = 706964, by_constituency = TRUE)
#' }
#'
epetition <- function(ID = NULL, by_constituency = FALSE,
                      extra_args = NULL, tidy = TRUE,
                      tidy_style = "snake", verbose = TRUE) {
  if (!is.null(ID)) {
    ID <- paste0("/", ID)
  }

  if (by_constituency) {
    json_query <- "/signaturesbyconstituency.json?"
  } else {
    json_query <- ".json?"
  }

  baseurl <- paste0(url_util, "epetitions")

  if (!is.null(ID) & !by_constituency) {
    veb(verbose)

    petition <- jsonlite::fromJSON(paste0(
      baseurl, ID,
      json_query, extra_args
    ),
    flatten = TRUE
    )

    df <- tibble::tibble(
      about = petition$result$primaryTopic$`_about`,
      abstract = petition$result$primaryTopic$abstract$`_value`,
      created = petition$result$primaryTopic$created$`_value`,
      identifier = petition$result$primaryTopic$identifier$`_value`,
      isPrimaryTopicOf = petition$result$primaryTopic$isPrimaryTopicOf,
      label = petition$result$primaryTopic$label$`_value`,
      modified = petition$result$primaryTopic$modified$`_value`,
      numberOfSignatures = petition$result$primaryTopic$numberOfSignatures,
      replyActionAbout = petition$result$primaryTopic$replyAction$`_about`,
      replyAction =
        petition$result$primaryTopic$replyAction$abstract$`_value`,
      status = petition$result$primaryTopic$status,
      subType = petition$result$primaryTopic$subType$`_about`,
      website = petition$result$primaryTopic$website
    )
  } else {
    query <- paste0(baseurl, ID, json_query, extra_args)

    df <- loop_query(query, verbose) # in utils-loop.R

    df$member <- NULL # Removes superfluous member column
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname epetition
#' @export
hansard_epetition <- epetition
