

#' House publications
#'
#' Imports data on House of Commons and House of Lords publications.
#'
#'
#' @param ID Publication ID. Defaults to \code{NULL}. If not \code{NULL},
#' requests a tibble with information on the given publication.
#'
#' @param house The house that produced the particular publication. Accepts
#' \code{'commons'} and \code{'lords'}. If \code{NULL} or not \code{'commons'}
#' or \code{'lords'}, returns publications from both House of Commons and
#' House of Lords. This parameter is case-insensitive. Defaults to \code{NULL}.
#'
#' @param start_date Only includes publications issued on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#'
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
#' # All publications in the house of commons
#' x <- publication_logs(house = 'commons')
#'
#' # Returns a given publication
#' y <- publication_logs(683267)
#' }

publication_logs <- function(ID = NULL, house = NULL, start_date = "1900-01-01",
                             end_date = Sys.Date(), extra_args = NULL,
                             tidy = TRUE, tidy_style = "snake_case",
                             verbose = TRUE) {
  id_query <- ifelse(
    is.null(ID) == FALSE,
    paste0("/", ID, ".json?"),
    ".json?"
  )

  if (is.null(house)) {
    house_query <- ""
  } else {
  house <- tolower(house)

  house_query <- house_query_util(house) ## in utils-house.R
  }

  dates <- paste0(
    "&_properties=publicationDate&max-publicationDate=",
    as.Date(end_date),
    "&min-publicationDate=",
    as.Date(start_date)
  )

  baseurl <- paste0(url_util, "publicationlogs")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  logs <- jsonlite::fromJSON(paste0(
    baseurl, id_query, house_query,
    dates, extra_args
  ),
  flatten = TRUE
  )

  if (is.null(ID) == FALSE) {
    df <- tibble::as.tibble(as.data.frame(logs$result$primaryTopic))
  } else {
    jpage <- floor(logs$result$totalResults / 500)

    query <- paste0(
      baseurl, id_query, house_query, dates,
      extra_args, "&_pageSize=500&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- pub_tidy(df, tidy_style) ## in utils-publogs.R
    }

    df
  }
}


#' @rdname publication_logs
#' @export
hansard_publication_logs <- publication_logs
