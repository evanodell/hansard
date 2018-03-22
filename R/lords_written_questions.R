

#' House of Lords written questions
#'
#' Imports data on House of Lords written questions asked by a given peer(s),
#' and/or directed to a given department.
#'
#'
#' @param peer_id Accepts a member ID or list of IDs, and returns a tibble
#' with all written questions asked by that member or members. If \code{NULL},
#' peer_id is not included in the query to the API and so all written
#' questions are returned subject to other function parameters.
#' Defaults to \code{NULL}.
#' @param answering_department Accepts a string with a department name or
#' partial name or a list of departmental names, and returns all written
#' questions by that department. The query acts as a search, so entering
#' \code{'health'} will return all questions answered by the Department of
#' Health. If \code{NULL}, answering_department is not included as a query
#' parameter. Defaults to \code{NULL}.
#' @param start_date Only includes questions tabled on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes questions tabled on or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on written questions in the House of Lords.
#'
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#' x <- lords_written_questions() # Returns all written questions ever
#'
#' x <- lords_written_questions(peer_id = c(3526,4176),
#'                              answering_department = c('cabinet',
#'                                                       'Transport'))
#' }

lords_written_questions <- function(peer_id = NULL,
                                    answering_department = NULL,
                                    start_date = "1900-01-01",
                                    end_date = Sys.Date(),
                                    extra_args = NULL, tidy = TRUE,
                                    tidy_style = "snake_case",
                                    verbose = TRUE) {
  if (length(answering_department) > 1 || length(peer_id) > 1) {
    df <- lwq_multi(
      answering_department, peer_id, start_date,
      end_date, extra_args, verbose
    ) ### in utils-lords.R
  } else {
    dates <- paste0(
      "&_properties=dateTabled&max-dateTabled=",
      as.Date(end_date),
      "&min-dateTabled=",
      as.Date(start_date)
    )


    peer_id <- dplyr::if_else(
      is.null(peer_id) == FALSE && is.na(peer_id) == FALSE,
      utils::URLencode(
        paste0(
          "&tablingMember=http://data.parliament.uk/members/",
          peer_id
        )
      ),
      ""
    )

    dept_query <- dplyr::if_else(
      is.null(answering_department) == FALSE &&
        is.na(answering_department) == FALSE,
      utils::URLencode(
        paste0("/answeringdepartment.json?q=", answering_department)
      ),
      ".json?"
    )

    baseurl <- paste0(url_util, "lordswrittenquestions")

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    writ <- jsonlite::fromJSON(paste0(
      baseurl, dept_query, peer_id,
      dates, extra_args, "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(writ$result$totalResults / 500)

    query <- paste0(
      baseurl, dept_query, peer_id, dates,
      extra_args, "&_pageSize=500&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- lwq_tidy(df, tidy_style) ## in utils-lords.R
    }

    df
  }
}

#' @rdname lords_written_questions
#' @export
hansard_lords_written_questions <- lords_written_questions
