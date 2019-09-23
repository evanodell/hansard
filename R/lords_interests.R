
#' Peers' registered interests
#'
#' Registered financial interests of members of the House of Lords.
#' If `peer_id=NULL` the actual details of registered interests
#' are stored in a nested data frame.
#'
#' @param peer_id The ID of a member of the House of lords. If `NULL`,
#' returns a tibble with all listed financial interests for all members.
#' Defaults to `NULL`.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the interests of peers in
#' the House of Lords.
#' @export
#' @examples
#' \dontrun{
#' x <- lords_interests(4170)
#'
#' y <- lords_interests()
#' }
lords_interests <- function(peer_id = NULL, extra_args = NULL, tidy = TRUE,
                            tidy_style = "snake", verbose = TRUE) {
  json_query <- ifelse(
    is.null(peer_id) == TRUE,
    ".json?",
    paste0(".json?member=", peer_id)
  )

  baseurl <- paste0(url_util, "lordsregisteredinterests")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  members <- jsonlite::fromJSON(paste0(
    baseurl, json_query,
    extra_args, "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(members$result$totalResults / 100)

  query <- paste0(baseurl, json_query, extra_args)

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      if (is.null(peer_id)) {
        df <- lords_interests_tidy2(df, tidy_style) ## in utils-lords.R
      } else {
        df <- lords_interests_tidy(df, tidy_style) ## in utils-lords.R
      }
    }

    df
  }
}


#' @export
#' @rdname lords_interests
hansard_lords_interests <- lords_interests
