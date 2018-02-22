

#' House of Lords divisions
#'
#' Imports data on House of Lords divisions. Either a general query subject to
#' parameters, or the results of a specific division. Individual divisions can
#' be queried to return a short summary of the votes, or details on how each
#' peer voted.
#' @param division_id The id of a particular vote. If empty, returns a tibble
#' with information on all lords divisions. Defaults to \code{NULL}.
#' @param summary If \code{TRUE}, returns a small tibble summarising a
#' division outcome. Otherwise returns a tibble with details on how each peer
#' voted. Has no effect if \code{division_id} is empty.
#' Defaults to \code{FALSE}.
#' @param start_date Only includes divisions on or after this date. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes divisions on or before this date. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with the results of divisions in the House of Lords.
#'
#' @export
#' @examples \dontrun{
#' x <- lords_divisions(division_id = 705891, summary = TRUE)
#'
#' x <- lords_divisions(division_id = 705891, summary = FALSE)
#'
#' # Return all lords divisions in 2016
#' x <- lords_divisions(NULL, FALSE, start_date = '2016-01-01',
#'                      end_date = '2016-12-31')
#' }

lords_divisions <- function(division_id = NULL, summary = FALSE,
                            start_date = "1900-01-01", end_date = Sys.Date(),
                            extra_args = NULL, tidy = TRUE,
                            tidy_style = "snake_case", verbose = TRUE) {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date),
                    "&min-date=", as.Date(start_date))

    if (is.null(division_id) == TRUE) {

        baseurl <- paste0(url_util,  "lordsdivisions.json?")

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        divis <- jsonlite::fromJSON(paste0(baseurl, dates,
                                           extra_args, "&_pageSize=1"))

        jpage <- floor(divis$result$totalResults/500)

        query <- paste0(baseurl, dates, extra_args, "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    } else {

        baseurl <- paste0(url_util,  "lordsdivisions/id/")

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        divis <- jsonlite::fromJSON(paste0(baseurl, division_id,
                                           ".json?", dates, extra_args),
                                    flatten = TRUE)

        if (summary == TRUE) {

            df <- list()

            df$about <- divis$result$primaryTopic$`_about`
            df$title <- divis$result$primaryTopic$title
            df$description <- divis$result$primaryTopic$description
            df$officialContentsCount <-
              divis$result$primaryTopic$officialContentsCount
            df$officialNotContentsCount <-
              divis$result$primaryTopic$officialNotContentsCount
            df$divisionNumber <- divis$result$primaryTopic$divisionNumber
            df$divisionResult <- divis$result$primaryTopic$divisionResult
            df$date <- divis$result$primaryTopic$date
            df$session <- divis$result$primaryTopic$session
            df$uin <- divis$result$primaryTopic$uin

        } else {

            df <- divis$result$primaryTopic

        }

        df <- tibble::as_tibble(as.data.frame(df))

    }

    if (nrow(df) == 0) {

        message("The request did not return any data.
                Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- lords_division_tidy(df, division_id,
                                      summary, tidy_style)  ##in utils-lords.R

        }

        df

    }
}


#' @rdname lords_divisions
#' @export
hansard_lords_divisions <- lords_divisions
