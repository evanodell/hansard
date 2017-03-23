

#' commons_terms
#'
#' Imports the parliamentary thesaurus. The API is rate limited to 5500 requests at a time, so some use of parameters is required.
#' @param search A string to search the parliamentary thesaurus for.
#' @param class The class of definition to be returned Accepts one of 'ID', 'ORG', 'SIT', 'NAME', 'LEG','CTP', 'PBT' and 'TPG'.  Defaults to NULL
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords parliamentary thesaurus
#' @export
#' @examples \dontrun{
#'
#' x <- commons_terms(search='estate')
#'
#' x <- commons_terms(search='estate', class='ORG')
#'
#'}

commons_terms <- function(search = NULL, class = NULL, extra_args = NULL, tidy = TRUE) {

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

    jpage <- round(terms$result$totalResults/terms$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, search_query, class_query, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    if (nrow(df) == 0) {
      message("The request did not return any data. Please check your search parameters.")
    } else {

      if (tidy == TRUE) {

        df <- hansard_tidy(df)

      } else {

        df

      }

    }

}
