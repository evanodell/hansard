


#' Imports data on early day motions in signed, sponsored or primarily sponsored by a given MP.
#' @param mp_id The ID number of an MP. Required parameter, defaults to NULL.
#' @param primary_sponsor Returns a tibble of all early day motions where the given member is the primary sponsor. Defaults to TRUE.
#' @param sponsor Returns a tibble of early day motions where the given member is the primary sponsor or a sponsor. Defaults to FALSE.
#' @param signatory Returns a tibble of all early day motions signed by the given member. Because of the structure of the API, there is less information contained in the tibble return if signatory is TRUE, unless full_data is also TRUE. Defaults to FALSE.
#' @param full_data If TRUE, returns all available data on the EDMs signed or sponsored by a member. Defaults to FALSE. Note that this can be a very slow process compared to other \code{hansard} functions.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with information on the tibbles signed, sponsored or primarily sponsored by the given MP.
#' @keywords Early Day Motion
#' @export
#' @examples \dontrun{
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = FALSE, signatory=FALSE)
#'
#' x <- mp_edms(mp_id=3967, primary_sponsor=TRUE, sponsor = FALSE, signatory=TRUE, full_data=TRUE)
#'
#' }


mp_edms <- function(mp_id = NULL, primary_sponsor = TRUE, sponsor = FALSE, signatory = FALSE, full_data = FALSE, extra_args = NULL, tidy = TRUE) {

    if (is.null(mp_id) == TRUE) {
        stop("mp_id must not be empty", call. = FALSE)
    }

    query_primary_sponsor <- paste0("&isPrimarySponsor=", tolower(primary_sponsor))

    query_sponsor <- paste0("&isSponsor=", tolower(sponsor))

    query <- paste0("&member=http://data.parliament.uk/members/", mp_id)

    baseurl <- "http://lda.data.parliament.uk/edmsignatures.json?"

    message("Connecting to API")

    edms <- jsonlite::fromJSON(paste0(baseurl, query, query_primary_sponsor, query_sponsor, "&_pageSize=500", extra_args), flatten = TRUE)

    if (edms$result$totalResults > edms$result$itemsPerPage) {

        jpage <- round(edms$result$totalResults/edms$result$itemsPerPage, digits = 0)

    } else {

        jpage <- 0
    }

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, query_primary_sponsor, query_sponsor, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- dplyr::bind_rows(pages)

    df <- tibble::as_tibble(df)

    df$dateSigned._value <- as.Date(df$dateSigned._value)

    if (full_data == TRUE) {

        names(df)[names(df) == "_about"] <- "about"

        df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)

        df$about <- gsub("/signatures/.*", "", df$about)

        search_list <- as.list(df$about)

        baseurl <- "http://lda.data.parliament.uk/resources/"

        dat <- list()

        message("Retrieving EDM data")

        for (i in search_list) {

            search <- jsonlite::fromJSON(paste0(baseurl, i, ".json?"), flatten = TRUE)

            search_df <- data.frame(about = search$result$primaryTopic$`_about`,
                                    creator_label = search$result$primaryTopic$creator$`_about`,
                                    date = search$result$primaryTopic$date$`_value`,
                                    dateTabled = search$result$primaryTopic$dateTabled$`_value`,
                                    edmNumber = search$result$primaryTopic$edmNumber$`_value`,
                                    edmStatus = search$result$primaryTopic$edmStatus$`_value`,
                                    externalLocation = search$result$primaryTopic$externalLocation,
                                    humanIndexable = search$result$primaryTopic$humanIndexable$`_value`,
                                    identifier = search$result$primaryTopic$identifier$`_value`,
                                    isPrimaryTopicOf = search$result$primaryTopic$isPrimaryTopicOf,
                                    legislature = search$result$primaryTopic$legislature$prefLabel._value,
                                    motionText = search$result$primaryTopic$motionText,
                                    numberOfSignatures = search$result$primaryTopic$numberOfSignatures,
                                    primarySponsor = search$result$primaryTopic$primarySponsor$`_about`,
                                    primarySponsorPrinted = search$result$primaryTopic$primarySponsorPrinted,
                                    published = search$result$primaryTopic$published$`_value`,
                                    publisher = search$result$primaryTopic$publisher$prefLabel$`_value`,
                                    session = search$result$primaryTopic$session,
                                    sessionNumber = search$result$primaryTopic$sessionNumber$`_value`,
                                    title = search$result$primaryTopic$title)

            dat[[i]] <- search_df
        }

        message("Joining data")

        df2 <- do.call(rbind, dat)

        df2$about <- as.character(df2$about)

        df <- dplyr::left_join(df, df2, by = "about")

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
