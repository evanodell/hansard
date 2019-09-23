

#' Search for an MP or Peer by name and constituency
#'
#' Note that there are problems with the remote endpoint for this
#' API, and that correct search queries may not return any results.
#'
#' Function searches for the string and returns a tibble with all matches from
#' both houses of parliament. Returns all partial matches in the members'
#' names, constituencies, twitter handle and webpage. The default search is
#' `NULL`, which returns a tibble of all members of both houses, the
#' same result as `members()`.
#'
#'
#' @param search Accepts any lucene query string, using * as a multiple
#' character wildcard, and ? as the single character wildcard. Searchs are
#' not case sensitive. If `NULL`, returns a tibble with all members of
#' both houses of parliament. Defaults to `NULL`.
#'
#' @inheritParams all_answered_questions
#' @return A tibble with the results of the search.
#' @seealso [members()]
#' @export
#' @examples
#' \dontrun{
#' x <- members_search("*chris*")
#'
#' x <- members_search(search = "*chris*")
#' }
#'
members_search <- function(search = NULL, tidy = TRUE,
                           tidy_style = "snake", verbose = TRUE) {
  if (is.null(search)) {
    df <- members(tidy = tidy, tidy_style = tidy_style, verbose = verbose)

    df
  } else {
    warning("Search functions are not consistently working on the API")

    search <- utils::URLencode(search)

    baseurl <- paste0(url_util, "members.json?_search=")

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    results <- jsonlite::fromJSON(paste0(baseurl, search))

    jpage <- floor(results$result$totalResults / 100)

    query <- paste0(baseurl, search)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {
      message("The request did not return any data.
                Please check your parameters.")
    } else {
      if (tidy == TRUE) {
        names(df)[names(df) == "_about"] <- "mnis_id"

        df$mnis_id <- gsub(
          "http://data.parliament.uk/members/", "", df$mnis_id
        )

        df <- hansard_tidy(df, tidy_style)
      }

      df
    }
  }
}

#' @rdname members_search
#' @export
hansard_members_search <- members_search
