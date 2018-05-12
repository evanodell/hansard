

#' Parliamentary Thesaurus
#'
#' Imports the parliamentary thesaurus. The API is rate limited to 5500
#' requests at a time, so some use of parameters is required.
#' @param search A string to search the parliamentary thesaurus for.
#' @param class The class of definition to be returned Accepts one of
#' \code{'ID'}, \code{'ORG'}, \code{'SIT'}, \code{'NAME'}, \code{'LEG'},
#' \code{'CTP'}, \code{'PBT'} and \code{'TPG'}.  Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with results from the parliamentary thesaurus.
#' @export
#' @examples \dontrun{
#' x <- commons_terms(search='estate')
#'
#' x <- commons_terms(search='estate', class='ORG')
#' }

commons_terms <- function(search = NULL, class = NULL, extra_args = NULL,
                          tidy = TRUE, tidy_style = "snake_case",
                          verbose = TRUE) {
  warning("Search functions are not consistently working on the API")

  search_query <- ifelse(
    is.null(search) == FALSE,
    paste0("&_search=", utils::URLencode(search)), NULL
  )

  if (is.null(class) == FALSE) {
    class_list <- list(
      "ID", "ORG", "SIT", "NAME", "LEG",
      "CTP", "PBT", "TPG"
    )

    if (!(class %in% class_list)) {
      stop("Please check your class parameter.
                 It must be one of \"ID\", \"ORG\", \"SIT\", \"NAME\",
                 \"LEG\", \"CTP\", \"PBT\" or\"TPG\"", call. = FALSE)
    } else {
      class_query <- paste0("&class=", class)
    }
  } else {
    class_query <- NULL
  }

  baseurl <- paste0(url_util, "terms.json?&_view=description")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  terms <- jsonlite::fromJSON(paste0(
    baseurl, search_query,
    class_query, extra_args,
    "&_pageSize=1"
  ),
  flatten = TRUE
  )

  jpage <- floor(terms$result$totalResults / 500)

  query <- paste0(
    baseurl, search_query, class_query,
    extra_args, "&_pageSize=500&_page="
  )

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- hansard_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname commons_terms
#' @export
hansard_commons_terms <- commons_terms
