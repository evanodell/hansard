
#' House of Commons divisions
#'
#' Imports data on House of Commons divisions (votes), either full details
#' on how each member voted, or a summary of vote totals.
#'
#' @param division_id The id of a particular vote. If empty, returns a
#' tibble with information on all commons divisions, subject to all other
#' parameters. Defaults to \code{NULL}. Only accepts a single division ID
#' at a time, but to return details on a list of division IDs use with
#' \code{lapply}.
#' @param summary If \code{TRUE}, returns a small tibble summarising a
#' division outcome. Otherwise returns a tibble with details on how each
#' MP voted. Has no effect if `division_id` is empty. Defaults to \code{FALSE}.
#' @param start_date Only includes divisions on or after this date. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}. Defaults
#' to \code{'1900-01-01'}.
#' @param end_date Only includes divisions on or before this date. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}. Defaults to
#' the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with the results of divisions in the House of Commons.
#' @export
#' @examples \dontrun{
#' x <- commons_divisions()
#'
#' x <- commons_divisions(division_id = 694163, summary = FALSE)
#' }

commons_divisions <- function(division_id = NULL, summary = FALSE,
                              start_date = "1900-01-01",
                              end_date = Sys.Date(), extra_args = NULL,
                              tidy = TRUE, tidy_style = "snake_case",
                              verbose = FALSE) {

    dates <- paste0("&_properties=date&max-date=",
                    as.Date(end_date), "&min-date=",
                    as.Date(start_date))

    if (is.null(division_id) == TRUE) {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions"

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        divis <- jsonlite::fromJSON(paste0(baseurl, ".json?",
                                           dates, extra_args, "&_pageSize=1"),
                                    flatten = TRUE)

        jpage <- floor(divis$result$totalResults/500)

        query <- paste0(baseurl, ".json?",
                        dates, extra_args,
                        "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    } else {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/id/"

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id, ".json?",
                                           dates, extra_args),
                                    flatten = TRUE)

        if (summary == TRUE) {

            df <- tibble::tibble(abstainCount = divis$result$primaryTopic$AbstainCount$`_value`,
                ayesCount = divis$result$primaryTopic$AyesCount$`_value`,
                noesVoteCount = divis$result$primaryTopic$Noesvotecount$`_value`,
                didNotVoteCount = divis$result$primaryTopic$Didnotvotecount$`_value`,
                errorVoteCount = divis$result$primaryTopic$Errorvotecount$`_value`,
                nonEligibleCount = divis$result$primaryTopic$Noneligiblecount$`_value`,
                suspendedOrExpelledVotesCount = divis$result$primaryTopic$Suspendedorexpelledvotescount$`_value`,
                margin = divis$result$primaryTopic$Margin$`_value`,
                date = divis$result$primaryTopic$date$`_value`,
                divisionNumber = divis$result$primaryTopic$divisionNumber,
                session = divis$result$primaryTopic$session[[1]],
                title = divis$result$primaryTopic$title,
                uin = divis$result$primaryTopic$uin)

        } else {

            df <- tibble::as_tibble(divis$result$primaryTopic$vote)

        }

    }

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- cd_tidy(df, tidy_style, division_id, summary)  ## in utils-commons.R

        }

        df

    }

}


#' @rdname commons_divisions
#' @export
hansard_commons_divisions <- commons_divisions
