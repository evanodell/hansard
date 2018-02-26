
#' House of Lords attendance by date
#'
#' Imports data on House of Lords attendance on a given date.
#'
#' Please note that House of Lords attendance data is not as
#' tidy as some of the others that are accessible through this
#' API, and so additional work on the return from the API may be required.
#'
#' Also note that this API does not appear to have been
#' updated with data after 2017-01-31.
#'
#' @param date Accepts a date to return attendance data for. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the lords who attended on a given date.
#' @export
#' @seealso \code{\link{lords_attendance_session}}
#' @examples \donttest{
#' x <- lords_attendance_date(date = '2016-03-01')
#' }

lords_attendance_date <- function(date = NULL, tidy = TRUE,
                                  tidy_style = "snake_case", verbose = TRUE) {

    if (is.null(date) == TRUE)
        stop("Please include a date.", call. = FALSE)

    date_query <- as.Date(date)

    baseurl <- paste0(url_util,  "lordsattendances/date/")

    if (verbose == TRUE) {
        message("Connecting to API")
    }

    attend <- jsonlite::fromJSON(paste0(baseurl, date_query, ".json"),
                                 flatten = TRUE)

    df <- tibble::as_tibble(as.data.frame(attend$result$items$attendee))

    df <- tidyr::unnest_(df, "member")

    if (nrow(df) == 0) {

        message("The request did not return any data.
                Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            names(df)[names(df) == "X_about"] <- "about"

            names(df)[names(df) == "_about"] <- "peer_id"

            df$peer_id <- stringi::stri_replace_all_fixed(
              df$peer_id,
              "http://data.parliament.uk/members/", "",
              vectorize_all = FALSE)

            df <- hansard_tidy(df, tidy_style)

        }

        df

    }
}


#' @rdname lords_attendance_date
#' @export
hansard_lords_attendance_date <- lords_attendance_date
