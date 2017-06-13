

#' Imports data on all current and former Members of Parliament including the Lords and the Commons
#' @param ID The ID of a member of the House of Commons or the House of Lords. Defaults to NULL. If NULL, returns a tibble of all members. If not NULL, returns a tibble with basic information on that member.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with data on members of the House of Commons and/or the House of Lords.
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

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

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
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else {

        df <- list()

        df$`_about` <- members$result$primaryTopic$`_about`
        df$additionalName <- members$result$primaryTopic$additionalName$`_value`
        df$constituency_about <- members$result$primaryTopic$constituency$`_about`
        df$constituency_label <- members$result$primaryTopic$constituency$label
        df$familyName <- members$result$primaryTopic$familyName$`_value`
        df$fullName <- members$result$primaryTopic$fullName$`_value`
        df$gender <- members$result$primaryTopic$gender$`_value`
        df$givenName <- members$result$primaryTopic$givenName$`_value`
        df$homePage <- members$result$primaryTopic$homePage
        df$isPrimaryTopicOf <- members$result$primaryTopic$isPrimaryTopicOf
        df$label <- members$result$primaryTopic$label$`_value`
        df$party <- members$result$primaryTopic$party$`_value`
        df$twitter <- members$result$primaryTopic$twitter$`_value`

        df <- tibble::as.tibble(df)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$`_about` <- gsub("http://data.parliament.uk/members/", "", df$`_about`)

            df <- hansard::hansard_tidy(df, tidy_style)

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
commons_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

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


# lords_members
#'
#' Imports data on all current and former peers
#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

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

            df <- hansard::hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}


# lords_interests
#' @param peer_id The ID of a member of the House of lords. If NULL, returns a tibble with all listed financial interests for all members. Defaults to NULL.
#' @return A tibble with details on the interests of peers in the House of Lords.
#' @rdname members
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

            df <- hansard::hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}
