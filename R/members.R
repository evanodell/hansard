

#' Members of Both Houses
#'
#' Imports basic details on current and former Members of Parliament including the Lords and the Commons.
#'
#' For more details on a given member see \code{\link[mnis]{mnis_full_biog}}.
#'
#' @param ID The ID of a member of the House of Commons or the House of Lords to return data on. If \code{NULL}, returns a tibble of all members. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with data on members of the House of Commons and/or the House of Lords.
#'
#' @export
#' @section Member details functions:
#' \describe{
#' \item{\code{members}}{Basic details on a given member from either house}
#' \item{\code{commons_members}}{MPs in the House of Commons}
#' \item{\code{lords_members}}{Peers in the House of Lords}
#' }
#' @seealso \code{\link{members}}
#' @examples \dontrun{
#' x <- members(172)
#'
#' x <- commons_members()
#'
#' x <- lords_members()
#'}

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(ID) == TRUE) {
        query <- ".json?"
    } else {
        query <- paste0("/", ID, ".json?")
    }

    baseurl <- "http://lda.data.parliament.uk/members"

    if(verbose==TRUE){message("Connecting to API")}

    members <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

    if (is.null(ID) == TRUE) {

        jpage <- floor(members$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, "_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

    } else {

        df <- list()

        df$about <- members$result$primaryTopic$`_about`
        df$additionalName <- members$result$primaryTopic$additionalName$`_value`
        df$constituencyAbout <- members$result$primaryTopic$constituency$`_about`
        df$constituencyLabel <- members$result$primaryTopic$constituency$label
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

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
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
hansard_members <- function(ID = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- members(ID=ID, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}


#' @export
#' @rdname members
commons_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    baseurl <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

    if(verbose==TRUE){message("Connecting to API")}

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}


#' @export
#' @rdname members
hansard_commons_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- commons_members(extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}


#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    baseurl <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

    if(verbose==TRUE){message("Connecting to API")}

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}

#' @export
#' @rdname members
hansard_lords_members<- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lords_members(extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}


