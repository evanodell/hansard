

#' House publications
#'
#' Imports data on House of Commons and House of Lords publications.
#' @param ID Publication ID. Defaults to \code{NULL}. If not \code{NULL},
#'  requests a tibble with information on the given publication.
#' @param house The house that produced the particular publication. Accepts
#' \code{'commons'} and \code{'lords'}. If \code{NULL} or not \code{'commons'}
#' or \code{'lords'}, returns publications from both House of Commons and
#' House of Lords. This parameter is case-insensitive. Defaults to \code{NULL}.
#' @param start_date Only includes publications issued on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes publications issued on or before this
#' date. Accepts character values in \code{'YYYY-MM-DD'} format, and
#' objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#' \code{POSIXlt} or anything else that can be coerced to a date with
#' \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details from publications in the House of
#' Commons and House of Lords
#' @export
#' @examples \dontrun{
#' x <- publication_logs(house='commons')
#'
#' x <- publication_logs(683267)
#' }

publication_logs <- function(ID = NULL, house = NULL, start_date = "1900-01-01",
                             end_date = Sys.Date(), extra_args = NULL,
                             tidy = TRUE, tidy_style = "snake_case",
                             verbose = FALSE) {

    if (is.null(ID) == FALSE) {

        id_query <- paste0("/", ID, ".json?")

    } else {

      id_query <- ".json?"

    }

    if (is.null(house) == FALSE) {

        house <- tolower(house)

        if (house == "commons") {

            house_query <- utils::URLencode("&legislature.prefLabel=House of Commons")

        } else if (house == "lords") {

            house_query <- utils::URLencode("&legislature.prefLabel=House of Lords")

        } else {

            house_query <- NULL

        }

    } else {

        house_query <- NULL

    }

    dates <- paste0("&_properties=publicationDate&max-publicationDate=",
                    as.Date(end_date),
                    "&min-publicationDate=",
                    as.Date(start_date))

    baseurl <- "http://lda.data.parliament.uk/publicationlogs"

    if (verbose == TRUE) {
        message("Connecting to API")
    }

    logs <- jsonlite::fromJSON(paste0(baseurl, id_query, house_query,
                                      dates, extra_args),
                               flatten = TRUE)

    if (is.null(ID) == FALSE) {

        df <- tibble::as_tibble(as.data.frame(logs$result$primaryTopic))

    } else {

        jpage <- floor(logs$result$totalResults/500)

        query <- paste0(baseurl, id_query, house_query, dates,
                        extra_args, "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    }

    if (nrow(df) == 0 && verbose == TRUE) {

        message("The request did not return any data. Please check your search parameters.")

    } else {

        if (tidy == TRUE) {

            df <- pub_tidy(df, tidy_style)  ## in utils-publogs.R

        }

        df

    }
}


#' @rdname publication_logs
#' @export
hansard_publication_logs <- publication_logs
