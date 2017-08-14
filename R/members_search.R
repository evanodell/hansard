

#' Search for an MP or Lord by name and constituency
#'
#' Function searches for the string and returns a tibble with all matches from both houses of parliament. Returns all partial matches in the members' names, constituencies, twitter handle and webpage. The default search is NULL, which returns a tibble of all members of both houses, the same result as \code{members('all')}.
#' @param search Accepts any string. Defaults to NULL. If NULL, returns a tibble with all members of both houses of parliament.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. For the `members_search` function it also changes the '_about' column name to 'mnis_id' (or 'mnisId' or 'mnis.id', depending on the value of the `tidy_text` parameter, and removes the URL to preserve only the numerical ID. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If TRUE, returns data to console on the progress of the API request. Defaults to FALSE.
#' @return  A tibble with the results of the search.
#'
#' @keywords All Members of Parliament
#' @seealso \code{\link{members}}
#' @export
#' @examples \dontrun{
#'
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
