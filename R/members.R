

#' Members of both houses
#'
#' Imports basic details on current and former Members of Parliament including the Lords and the Commons.
#'
#' For more details on a given member see \code{\link[mnis]{mnis_full_biog}}.
#'
#' @param ID The ID of a member of the House of Commons or the House of Lords to return data on. If \code{NULL}, returns a tibble of all members of both houses. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with data on members of the House of Commons (\code{commons_members()}), the House of Lords, (\code{lords_members()}), or both (\code{members()}).
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

members <- function(ID = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

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
            mydata <- jsonlite::fromJSON(paste0(baseurl, query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

    } else {

      df <- tibble::tibble(
        about = members$result$primaryTopic$`_about`,
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
        twitter = members$result$primaryTopic$twitter$`_value`
      )

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
hansard_members <- function(ID = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- members(ID=ID, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}


#' @export
#' @rdname members
commons_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

    baseurl <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

    if(verbose==TRUE){message("Connecting to API")}

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, extra_args, "&_page=", i), flatten = TRUE)
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
hansard_commons_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- commons_members(extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}


#' @export
#' @rdname members
lords_members <- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

    baseurl <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

    if(verbose==TRUE){message("Connecting to API")}

    members <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(members$result$totalResults/members$result$itemsPerPage)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, extra_args, "&_page=", i), flatten = TRUE)
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
hansard_lords_members<- function(extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- lords_members(extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}


