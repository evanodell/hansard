

#' Individual MP voting records
#'
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of their votes.
#' @param mp_id The ID number of a member of the House of Commons.
#' @param lobby Accepts one of \code{'all'}, \code{'aye'} or \code{'no'}. \code{'aye'} returns votes where the MP voted \code{'aye'}, \code{'no'} returns votes where the MP voted \code{'no'}, \code{'all'} returns all available votes by the MP. This parameter is not case sensitive. Defaults to \code{'all'}.
#' @param session The parliamentary session to return votes from, in \code{'YYYY/YY'} format. Defaults to \code{NULL}.
#' @param start_date Only includes divisions on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes divisions on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on the voting record of the given MP.
#' @export
#' @examples \dontrun{
#' x <- mp_vote_record(172, lobby='all')
#'
#' x <- mp_vote_record(172, lobby='aye')
#'
#' x <- mp_vote_record(172, lobby='no')
#'
#' x <- mp_vote_record(172, session = '2016/17')
#' }


mp_vote_record <- function(mp_id = NULL, lobby = "all", session = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (is.null(mp_id) == TRUE) {

    stop("mp_id must not be empty", call. = FALSE)

  }

  if (is.null(extra_args) == FALSE) {

    extra_args <- utils::URLencode(extra_args)

  }

  if (is.null(session) == FALSE) {

    session_query <- paste0("&session=", session)

  } else {

    session_query <- NULL

  }

  lobby <- tolower(lobby)

  dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

  if (lobby == "aye") {

    baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

    if (verbose == TRUE) {message("Connecting to API")}

    url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

    jpage <- floor(url_aye$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
      if (verbose == TRUE) {message("Retrieving page ", i + 1, " of ", jpage + 1)}
      pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  } else if (lobby == "no") {

    baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

    if (verbose == TRUE) {message("Connecting to API")}

    url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

    jpage <- floor(url_no$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
      if (verbose == TRUE) {message("Retrieving page ", i + 1, " of ", jpage + 1)}
      pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  } else {

    if (verbose == TRUE) {message("Retrieving aye votes:")}

    df_aye <- mp_vote_record(mp_id = mp_id, lobby = "aye", session = session, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style, verbose = verbose)

    df_aye$vote <- "aye"

    if (verbose == TRUE) {message("Retrieving no votes:")}

    df_no <- mp_vote_record(mp_id = mp_id, lobby = "no", session = session, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style, verbose = verbose)

    df_no$divisionNumber <- NULL

    df_no$vote <- "no"

    df <- dplyr::bind_rows(df_aye, df_no)

    if (tidy == TRUE) {

      df$vote <- as.factor(df$vote)

    }

  }

  if (nrow(df) == 0 && verbose == TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      df$date._datatype <- "POSIXct"

      df$date._value <- as.POSIXct(df$date._value)

      df <- hansard_tidy(df, tidy_style)

    }

    df

  }

}


#' @rdname mp_vote_record
#' @export
hansard_mp_vote_record <- mp_vote_record
