
#' members
#'
#' Imports data on all current and former Members of Parliament including the Lords and the Commons
#' @param ID The ID of a member of the House of Commons or the House of Lords. Defaults to NULL. If NULL, returns a tibble of all members. If not NULL, returns a tibble with basic information on that member.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords All Members of Parliament
#' @export
#' @examples \dontrun{
#' x <- members(172)
#'
#' x <- commons_members()
#'
#' x <- lords_members()
#'
#' x <- lords_interests(530)
#'}

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE) {

    if (is.null(ID) == TRUE) {
        query <- ".json?_pageSize=500"
    } else {
        query <- paste0("/", ID, ".json?")
    }

    baseurl <- "http://lda.data.parliament.uk/members"

    message("Connecting to API")

    members <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

    if (is.null(ID) == TRUE) {

        jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ID, query, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df <- tibble::as_tibble(df)

    } else {

        df <- tibble::as_tibble(members$result$primaryTopic)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}

#' commons_members
#'
#' Imports data on all current and former MPs
#' @export
#' @rdname members
commons_members <- function(extra_args = NULL, tidy = TRUE) {

    baseurl <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

    message("Connecting to API")

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}


# lords_members
#'
#' Imports data on all current and former peers
#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE) {

    baseurl <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

    message("Connecting to API")

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}


# lords_interests
#' @param peer_id The ID of a member of the house of lords. If NULL, returns a tibble with all listed financial interests for all members. Defaults to NULL.
#' @rdname members
#' @export
lords_interests <- function(peer_id = NULL, extra_args = NULL, tidy = TRUE) {

    if (is.null(peer_id) == TRUE) {
        query <- ".json?_pageSize=500"
    } else {
        query <- paste0(".json?member=", peer_id, "&_pageSize=500")
    }

    baseurl <- "http://lda.data.parliament.uk/lordsregisteredinterests"

    message("Connecting to API")

    members <- jsonlite::fromJSON(paste0(paste0(baseurl, extra_args), query), flatten = TRUE)

    jpage <- round(members$result$totalResults/members$result$itemsPerPage, digits = 0)

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

            df <- hansard_tidy(df)

            df

        } else {

            df

        }

    }
}
