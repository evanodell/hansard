
#' House of Lords attendance by session
#'
#' Imports data on House of Lords attendance for each parliamentary session. Please note that the attendance data is not as tidy as some of the others that are accessible through this API, and so additional work to prepare this data in a way that you want may be required.
#'
#' To return a tibble with all codes for available individual sessions, use \code{lords_attendance_session(session_id=NULL)}, or use \code{\link{lords_sessions}} to retrieve codes for a given date range. Attendance from multiple sessions can be accessed by using \code{lapply} with the output from \code{\link{lords_sessions}}.
#'
#' Please note that House of Lords attendance data is not as tidy as some of the others that are accessible through this API, and so additional work on the return from the API may be required.
#'
#' @param session_id The ID of the House of Lords session. If \code{NULL}, returns a list of all sessions, subject to other parameters. Defaults to \code{NULL}.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the lords who attended a given session.
#' @export
#' @seealso \code{\link{lords_attendance_date}}
#' @seealso \code{\link{lords_sessions}}
#'
#' @examples \dontrun{
#' x <- lords_attendance_session(session_id = 706178)
#'
#' #Returns a list of data frames with details of attendance for each day of a given vector of sessions.
#' u <- lords_sessions(start_date="2017-01-01")
#' m <- lapply(u$about, lords_attendance_session)
#'
#' }

lords_attendance_session <- function(session_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (is.null(session_id) == FALSE) {

    query <- paste0("/", session_id, ".json?")

  } else {

    query <- ".json?"

  }

  baseurl <- "http://lda.data.parliament.uk/lordsattendances"

  if(verbose==TRUE){message("Connecting to API")}

  attend <- jsonlite::fromJSON(paste0(baseurl, query, extra_args), flatten = TRUE)

  if (is.null(session_id) == FALSE) {

    if(verbose==TRUE){message("Retrieving attendance data")}

    df <- tibble::as_tibble(as.data.frame(attend$result$primaryTopic))

  } else {

    jpage <- floor(attend$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
      if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
      pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  }

  if (nrow(df) == 0 && verbose==TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      df <- lords_attendance_tidy(df, tidy_style)

    }

    df

  }

}



#' @rdname lords_attendance_session
#' @export
hansard_lords_attendance_session <- lords_attendance_session
