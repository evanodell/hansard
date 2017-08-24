
#' Peers' registered interests
#'
#' Registered interests of members of the House of Lords.
#' @param peer_id The ID of a member of the House of lords. If \code{NULL}, returns a tibble with all listed financial interests for all members. Defaults to \code{NULL}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}..
#' @return A tibble with details on the interests of peers in the House of Lords.
#' @export
#' @examples \dontrun{
#' x <- lords_interests(4170)
#'
#' y <- lords_interests()
#'}
lords_interests <- function(peer_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (is.null(peer_id) == TRUE) {### Better handling of lords with and without registered interests
    query <- ".json?"
  } else {
    query <- paste0(".json?member=", peer_id)
  }

  baseurl <- "http://lda.data.parliament.uk/lordsregisteredinterests"

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  members <- jsonlite::fromJSON(paste0(baseurl, extra_args, query), flatten = TRUE)

  jpage <- floor(members$result$totalResults/500)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
    if (verbose == TRUE) {
      message("Retrieving page ", i + 1, " of ", jpage + 1)
    }
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  if (nrow(df) == 0 && verbose == TRUE) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      if(is.null(peer_id)){

        df <- lords_interests_tidy2(df, tidy_style)

      } else {

      df <- lords_interests_tidy(df, tidy_style)

      }

    }

    df

  }
}


#' @export
#' @rdname lords_interests
hansard_lords_interests <- function(peer_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- lords_interests(peer_id = peer_id, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}
