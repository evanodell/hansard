

#' Search for an MP or Peer by name and constituency
#'
#' Function searches for the string and returns a tibble with all matches from both houses of parliament. Returns all partial matches in the members' names, constituencies, twitter handle and webpage. The default search is NULL, which returns a tibble of all members of both houses, the same result as \code{members('all')}.
#' @param search Accepts any string. Defaults to \code{NULL}. If \code{NULL}, returns a tibble with all members of both houses of parliament.
#' @inheritParams all_answered_questions
#' @return A tibble with the results of the search.
#' @seealso \code{\link{members}}
#' @export
#' @examples \dontrun{
#' x <- members_search('chris')
#'
#' x <- members_search(search='chris')
#' }

members_search <- function(search = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(search)) {
        df <- members("all")
    } else {

        search <- utils::URLencode(search)

        baseurl <- "http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*"

        if(verbose==TRUE){message("Connecting to API")}

        results <- jsonlite::fromJSON(paste0(baseurl, search, "*"))

        jpage <- floor(results$result$totalResults/results$result$itemsPerPage)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, search, "*", "&_page=", i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            names(df)[names(df) == "_about"] <- "mnis_id"

            df$mnis_id <- gsub("http://data.parliament.uk/members/", "", df$mnis_id)

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }

}

#' @rdname members_search
#' @export
hansard_members_search <- function(search = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE){

  df <- members_search(search = search, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
