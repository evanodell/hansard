
#' members_search
#'
#' Search for an MP or Lord by name and constituency
#'
#'Function searches for the string and returns a data frame with all matches from both houses of parliament. Returns all partial matches in the members' names, constituencies, twitter handle and webpage. The default search is NULL, which returns a data frame of all members of both houses, the same result as members('all').
#' @param search Accepts any string. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords All Members of Parliament
#' @export
#' @examples \dontrun{
#'
#' x <- members_search('chris')
#'
#' x <- members_search(search='chris')
#' }

members_search <- function(search = NULL, tidy = TRUE) {

    if (is.null(search)) {
        df <- members("all")
    } else {

        search <- utils::URLencode(search)

        baseurl <- "http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*"

        message("Connecting to API")

        results <- jsonlite::fromJSON(paste0(baseurl, search, "*"))

        jpage <- round(results$result$totalResults/results$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, search, "*", "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df$about <- gsub("http://data.parliament.uk/members/", "", df$about)

            names(df)[names(df)=="about"] <- "mnis_id"

            df

        } else {

            df

        }

    }

}

