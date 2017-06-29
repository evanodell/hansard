

#' Imports the parliamentary thesaurus. The API is rate limited to 5500 requests at a time, so some use of parameters is required.
#' @param search A string to search the parliamentary thesaurus for.
#' @param class The class of definition to be returned Accepts one of 'ID', 'ORG', 'SIT', 'NAME', 'LEG','CTP', 'PBT' and 'TPG'.  Defaults to NULL
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with results from the parliamentary thesaurus.
#' @keywords parliamentary thesaurus
#' @export
#' @examples \dontrun{
#'
#' x <- commons_terms(search='estate')
#'
#' x <- commons_terms(search='estate', class='ORG')
#'
#'}
commons_terms <- function(search = NULL, class = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    if (is.null(search) == FALSE) {
        search <- utils::URLencode(search)
        search_query <- paste0("&_search=", search)
    } else {
        search_query <- NULL
    }
    if (is.null(class) == FALSE) {
        class_list <- list("ID", "ORG", "SIT", "NAME", "LEG", "CTP", "PBT", "TPG")
        if (!(class %in% class_list)) {
            stop("Please check your class parameter. It must be one of \"ID\", \"ORG\", \"SIT\", \"NAME\", \"LEG\", \"CTP\", \"PBT\" or\"TPG\"",
                call. = FALSE)
        } else {
            class_query <- paste0("&class=", class)
        }
    } else {
        class_query <- NULL
    }
    baseurl <- "http://lda.data.parliament.uk/terms.json?_pageSize=500&_view=description"
    message("Connecting to API")
    terms <- jsonlite::fromJSON(paste0(baseurl, search_query, class_query, extra_args), flatten = TRUE)
    jpage <- floor(terms$result$totalResults/terms$result$itemsPerPage)
    pages <- list()
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, search_query, class_query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

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


#' @rdname commons_terms
#' @export
hansard_commons_terms <- function(search = NULL, class = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- commons_terms(search = search, class = class, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}



