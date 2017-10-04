

#' Lords sessions
#'
#' Returns the session code and other basic details for individual House of Lords sittings.
#'
#' @param start_date Only includes sessions starting on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes sessions ending on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @seealso \code{\link{lords_attendance_session}}
#' @seealso \code{\link{sessions_info}}
#'
#' @inheritParams all_answered_questions
#' @export

lords_sessions <- function(start_date = "1900-01-01", end_date = Sys.Date(), tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  baseurl <- "http://lda.data.parliament.uk/lordsattendances.json"

  dates <- paste0("?&min-date=", as.Date(start_date), "&max-date=", as.Date(end_date))

  if(verbose==TRUE){message("Connecting to API")}

  attend <- jsonlite::fromJSON(paste0(baseurl, dates), flatten = TRUE)

  jpage <- floor(attend$result$totalResults/500)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, dates, "&_pageSize=500&_page=", i), flatten = TRUE)
    if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  if (nrow(df) == 0 && verbose==TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      df <- lords_attendance_tidy(df, tidy_style)

    }

    df

  }

}



#' @rdname lords_sessions
#' @export
hansard_lords_sessions <- lords_sessions
