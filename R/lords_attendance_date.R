
#' House of Lords attendance by date
#'
#' Imports data on House of Lords attendance by date. Please note that the attendance data is not as tidy as some of the others that are accessible through this API, and so additional work to prepare this data in a way that you want may be required.
#' @param date Accepts a date to return attendance data for. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the lords who attended on a given date.
#' @export
#' @examples \dontrun{
#' x <- lords_attendance_date(date = 706178)
#' }

lords_attendance_date <- function(date = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (is.null(date) == TRUE)
    stop("Please include a date.", call. = FALSE)

  date_query <- as.Date(date)

  baseurl <- "http://lda.data.parliament.uk/lordsattendances/date/"

  if(verbose==TRUE){message("Connecting to API")}

  attend <- jsonlite::fromJSON(paste0(baseurl, date_query, ".json"), flatten = TRUE)

  df <- tibble::as_tibble(as.data.frame(attend$result$items$attendee))

  df <- tidyr::unnest_(df, "member")

  if (nrow(df) == 0 && verbose==TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      names(df)[names(df)=="X_about"] <- "about"

      names(df)[names(df)=="_about"] <- "peer_id"

      df$peer_id <- gsub("http://data.parliament.uk/members/", "", df$peer_id)

      df <- hansard_tidy(df, tidy_style)

    }

    df

  }
}


#' @rdname lords_attendance_date
#' @export
hansard_lords_attendance_date <- function(date = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  df <- lords_attendance_date(date = date, tidy = tidy, tidy_style = tidy_style, verbose = verbose)

  df

}
