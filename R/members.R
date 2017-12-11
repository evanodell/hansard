

#' Members of both houses
#'
#' Imports basic details on current and former Members of Parliament including
#' the Lords and the Commons.
#'
#' For more details on a given member see \code{\link[mnis]{mnis_full_biog}}.
#'
#' @param ID The ID of a member of the House of Commons or the House of Lords
#' to return data on. If \code{NULL}, returns a tibble of all members of both
#' houses. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with data on members of the House of Commons
#' (\code{commons_members()}), the House of Lords, (\code{lords_members()}),
#' or both (\code{members()}).
#'
#' @export
#' @section Member details functions:
#' \describe{
#' \item{\code{members}}{Basic details on a given member from either house}
#' \item{\code{commons_members}}{MPs in the House of Commons}
#' \item{\code{lords_members}}{Peers in the House of Lords}
#' }
#' @seealso \code{\link{members_search}}
#' @examples \dontrun{
#' a <- members()
#'
#' x <- members(172)
#'
#' y <- commons_members()
#'
#' z <- lords_members()
#'}

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE,
                    tidy_style = "snake_case", verbose = FALSE) {

    id_query <- dplyr::if_else(is.null(ID) == TRUE,
                               ".json?",
                               paste0("/", ID, ".json?"))

    baseurl <- "http://lda.data.parliament.uk/members"

    if (verbose == TRUE) {
        message("Connecting to API")
    }

    members <- jsonlite::fromJSON(paste0(baseurl, id_query, extra_args), flatten = TRUE)

    if (is.null(ID) == TRUE) {

        jpage <- floor(members$result$totalResults/500)

        query <- paste0(baseurl, id_query, extra_args, "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    } else {

        df <- tibble::tibble(about = members$result$primaryTopic$`_about`,
                             additionalName = members$result$primaryTopic$additionalName$`_value`,
                             constituencyAbout = members$result$primaryTopic$constituency$`_about`,
                             constituencyLabel = members$result$primaryTopic$constituency$label,
                             familyName = members$result$primaryTopic$familyName$`_value`,
                             fullName = members$result$primaryTopic$fullName$`_value`,
                             gender = members$result$primaryTopic$gender$`_value`,
                             givenName = members$result$primaryTopic$givenName$`_value`,
                             homePage = members$result$primaryTopic$homePage,
                             isPrimaryTopicOf = members$result$primaryTopic$isPrimaryTopicOf,
                             label = members$result$primaryTopic$label$`_value`,
                             party = members$result$primaryTopic$party$`_value`,
                             twitter = members$result$primaryTopic$twitter$`_value`)

    }

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

            df$about <- gsub("http://data.parliament.uk/members/", "", df$about)

        }

        df <- tibble::as.tibble(df)

        df

    }
}

#' @export
#' @rdname members
hansard_members <- members


#' @export
#' @rdname members
commons_members <- function(extra_args = NULL, tidy = TRUE,
                            tidy_style = "snake_case", verbose = FALSE) {

    baseurl <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

    if (verbose == TRUE) {
        message("Connecting to API")
    }

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    query <- paste0(baseurl, extra_args, "&_page=")

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

        }

        df

    }
}


#' @export
#' @rdname members
hansard_commons_members <- commons_members


#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE,
                          tidy_style = "snake_case", verbose = FALSE) {

    baseurl <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

    if (verbose == TRUE) {
        message("Connecting to API")
    }

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    query <- paste0(baseurl, extra_args, "&_page=")

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

        }

        df

    }
}

#' @export
#' @rdname members
hansard_lords_members <- lords_members

