
#' Registered interests of members of the House of Lords
#' @param peer_id The ID of a member of the House of lords. If NULL, returns a tibble with all listed financial interests for all members. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on the interests of peers in the House of Lords.
#' @export
lords_interests <- function(peer_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  if (is.null(peer_id) == TRUE) {
    query <- ".json?_pageSize=500"
  } else {
    query <- paste0(".json?member=", peer_id, "&_pageSize=500")
  }

  baseurl <- "http://lda.data.parliament.uk/lordsregisteredinterests"

  message("Connecting to API")

  members <- jsonlite::fromJSON(paste0(paste0(baseurl, extra_args), query), flatten = TRUE)

  jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", jpage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

  df <- dplyr::bind_rows(pages)

  df <- tibble::as_tibble(df)

  if (nrow(df) == 0) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      df <- hansard::hansard_tidy(df, tidy_style)

      df

    } else {

      df

    }

  }
}


#' @export
#' @rdname lords_interests
hansard_lords_interests<- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- lords_interests(peer_id=peer_id, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}
