

#' Voting record of members of the House of Lords
#'
#' Accepts an ID number for a member of the House of Commons, and returns a
#' tibble of their votes.
#' @param peer_id The ID number of a member of the House of Lords. A value
#' must be included for this parameter. Use the \code{\link{lords_members}}
#' to find IDs for members of the House of Lords. Defaults to \code{NULL}.
#' @param lobby Accepts one of \code{'all'}, \code{'content'},
#' \code{'notcontent'}. \code{'content'} returns votes where the peer voted
#' 'Content', \code{'notcontent'} returns votes where the peer voted
#' 'Not Content' and \code{'all'} returns all available votes by the peer.
#' This parameter is not case sensitive. Defaults to \code{'all'}.
#' @param start_date Only includes divisions on or after this date. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes divisions on or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the voting record of a
#' member of the House of Lords.
#'
#' @export
#' @examples \dontrun{
#' x <- lord_vote_record(530, lobby='all')
#'
#' x <- lord_vote_record(530, lobby='content')
#'
#' x <- lord_vote_record(530, lobby='notcontent')
#'
#' x <- lord_vote_record(530, lobby='not-content')
#' # This will also work
#' }


lord_vote_record <- function(peer_id = NULL, lobby = "all",
                             start_date = "1900-01-01", end_date = Sys.Date(),
                             extra_args = NULL, tidy = TRUE,
                             tidy_style = "snake_case", verbose = TRUE) {
  if (is.null(peer_id) == TRUE) {
    stop("peer_id must not be empty", call. = FALSE)
  }

  dates <- paste0(
    "&_properties=date&max-date=", as.Date(end_date),
    "&min-date=", as.Date(start_date)
  )

  lobby <- stringi::stri_replace_all_fixed(lobby, "-", "")

  lobby <- tolower(lobby)

  if (lobby == "all") {
    if (verbose == TRUE) {
      message("Retrieving 'content' votes")
    }

    df_content <- hansard::lord_vote_record(
      peer_id = peer_id,
      lobby = "content",
      start_date = start_date,
      end_date = end_date,
      extra_args = extra_args,
      tidy = FALSE,
      verbose = verbose
    )

    if (verbose == TRUE) {
      message("Retrieving 'not-content' votes")
    }

    df_not_content <- hansard::lord_vote_record(
      peer_id = peer_id,
      lobby = "notcontent",
      start_date = start_date,
      end_date = end_date,
      extra_args = extra_args,
      tidy = FALSE,
      verbose = verbose
    )

    df <- dplyr::bind_rows(df_content, df_not_content)

    df
  } else {
    baseurl <- paste0(url_util, "lordsdivisions/")

    if (verbose == TRUE) {
      message("Connecting to API")
    }

    content <- jsonlite::fromJSON(paste0(
      baseurl, lobby, ".json?mnisId=",
      peer_id, dates, extra_args,
      "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(content$result$totalResults / 500)

    query <- paste0(
      baseurl, lobby, ".json?mnisId=", peer_id,
      dates, extra_args, "&_pageSize=500&_page="
    )

    df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) > 0 & lobby == "content") {
      df$vote <- "Content"
    } else if (nrow(df) > 0) {
      df$vote <- "Not-Content"
    }

    df
  } ### End of else for specific lobbies above

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- lord_vote_record_tidy(df, tidy_style) ## in utils-lords.R
    }

    df
  }
}


#' @rdname lord_vote_record
#' @export
hansard_lord_vote_record <- lord_vote_record
